#include "MAPL_Generic.h"

module mapl3g_OuterMetaComponent
   use mapl3g_geom_mgr
   use mapl3g_UserSetServices,   only: AbstractUserSetServices
   use mapl3g_VariableSpec
   use mapl3g_StateItem
   use mapl3g_MultiState
   use mapl3g_VariableSpecVector
   use mapl3g_ComponentSpec
   use mapl3g_GenericPhases
   use mapl3g_ChildComponent
   use mapl3g_Validation, only: is_valid_name
   use mapl3g_InnerMetaComponent
   use mapl3g_MethodPhasesMap
   use mapl3g_ChildComponentMap, only: ChildComponentMap
   use mapl3g_ChildComponentMap, only: ChildComponentMapIterator
   use mapl3g_ChildComponentMap, only: operator(/=)
   use mapl3g_AbstractStateItemSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualPtVector
   use mapl3g_ConnectionVector
   use mapl3g_HierarchicalRegistry
   use mapl3g_StateExtension
   use mapl3g_ExtensionVector
   use mapl3g_ESMF_Interfaces, only: I_Run, MAPL_UserCompGetInternalState, MAPL_UserCompSetInternalState
   use mapl_ErrorHandling
   use mapl3g_VerticalGeom
   use gFTL2_StringVector
   use mapl_keywordEnforcer, only: KE => KeywordEnforcer
   use esmf
   use pflogger, only: logging, Logger
   implicit none
   private

   public :: OuterMetaComponent
   public :: get_outer_meta
   public :: attach_outer_meta
   public :: free_outer_meta

   type :: UserComponent
      class(AbstractUserSetServices), allocatable :: setservices
      type(ESMF_GridComp) :: gridcomp
      type(MultiState) :: states
   end type UserComponent

   type :: OuterMetaComponent
      private
      
      type(ESMF_GridComp)                         :: self_gridcomp

      type(UserComponent) :: user_component
      type(ESMF_HConfig)                          :: hconfig

      type(ESMF_Geom), allocatable                :: geom
      type(VerticalGeom), allocatable             :: vertical_geom
      logical                                     :: is_root_ = .false.

      type(MethodPhasesMap)                       :: phases_map
      type(InnerMetaComponent), allocatable       :: inner_meta

      ! Hierarchy
      type(ChildComponentMap)                     :: children
      type(HierarchicalRegistry) :: registry
 
      class(Logger), pointer :: lgr  => null() ! "MAPL.Generic" // name

      type(ComponentSpec)                         :: component_spec
      type(ExtensionVector) :: state_extensions

      integer :: counter

   contains
      
      procedure :: set_hconfig
      procedure :: get_hconfig
      procedure :: get_registry
      procedure :: get_lgr

      procedure :: get_phases
!!$      procedure :: get_gridcomp
      procedure :: get_user_gridcomp
      procedure :: get_user_states
      procedure :: set_user_setServices
      procedure :: set_entry_point

      ! Generic methods
      procedure :: setServices => setservices_

      procedure :: init_meta  ! object

      procedure :: initialize ! init by phase name
      procedure :: initialize_user
      procedure :: initialize_geom
      procedure :: initialize_advertise
      procedure :: initialize_post_advertise
      procedure :: initialize_realize

      procedure :: run
      procedure :: finalize
      procedure :: read_restart
      procedure :: write_restart

      ! Hierarchy
      procedure, private :: add_child_by_name
      procedure, private :: get_child_by_name
      procedure, private :: run_child_by_name
      procedure, private :: run_children_

      generic :: add_child => add_child_by_name
      generic :: get_child => get_child_by_name
      generic :: run_child => run_child_by_name
      generic :: run_children => run_children_

      procedure :: traverse

      procedure :: set_geom
      procedure :: get_name
      procedure :: get_user_gridcomp_name
      procedure :: get_gridcomp
      procedure :: is_root

      procedure :: get_component_spec
      procedure :: get_internal_state

      procedure :: set_vertical_geom

   end type OuterMetaComponent

   type OuterMetaWrapper
      type(OuterMetaComponent), pointer :: outer_meta
   end type OuterMetaWrapper


   interface get_outer_meta
      module procedure :: get_outer_meta_from_outer_gc
   end interface get_outer_meta

   character(len=*), parameter :: OUTER_META_PRIVATE_STATE = "OuterMetaComponent Private State"



   ! Submodule interfaces
   interface

      recursive module subroutine SetServices_(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, intent(out) ::rc
      end subroutine

      module subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_Method_Flag), intent(in) :: method_flag
         procedure(I_Run) :: userProcedure
         class(KE), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
         integer, optional, intent(out) ::rc
      end subroutine set_entry_point

      module subroutine add_child_by_name(this, child_name, setservices, hconfig, rc)
         class(OuterMetaComponent), intent(inout) :: this
         character(len=*), intent(in) :: child_name
         class(AbstractUserSetServices), intent(in) :: setservices
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end subroutine add_child_by_name

   end interface

   interface OuterMetaComponent
      module procedure new_outer_meta
   end interface OuterMetaComponent


   abstract interface
      subroutine I_child_op(this, child_meta, rc)
         import OuterMetaComponent
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) :: child_meta
         integer, optional, intent(out) :: rc
      end subroutine I_child_Op
   end interface

   interface apply_to_children
      module procedure apply_to_children_simple
      module procedure apply_to_children_custom
   end interface apply_to_children

   integer, save :: counter = 0

contains


   ! Keep the constructor simple
   type(OuterMetaComponent) function new_outer_meta(gridcomp, user_gridcomp, set_services, hconfig) result(outer_meta)
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(ESMF_GridComp), intent(in) :: user_gridcomp
      class(AbstractUserSetServices), intent(in) :: set_services
      type(ESMF_HConfig), intent(in) :: hconfig

      outer_meta%self_gridcomp = gridcomp
      outer_meta%user_component%setservices = set_services
      outer_meta%user_component%gridcomp = user_gridcomp
      outer_meta%hconfig = hconfig

      counter = counter + 1
      outer_meta%counter = counter

   end function new_outer_meta

   ! NOTE: _Not_ an ESMF phase - this is initializing the object itself.
   ! Constructor (new_outer_meta) only copies basic parameters.  All
   ! other initialization is in this procedure.

   subroutine init_meta(this, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: user_gc_name

      call initialize_phases_map(this%phases_map)
      call create_user_states(this, _RC)
      user_gc_name = this%get_user_gridcomp_name(_RC)
      this%registry = HierarchicalRegistry(user_gc_name)

      this%lgr => logging%get_logger('MAPL.GENERIC')

      _RETURN(_SUCCESS)

   contains

      ! This procedure violates GEOS policy on providing a traceback
      ! for failure conditions.  But failure in ESMF_StateCreate()
      ! should be all-but-impossible and the usual error handling
      ! would induce tedious changes in the design. (Function ->
      ! Subroutine)
      subroutine create_user_states(this, rc)
         type(OuterMetaComponent), intent(inout) :: this
         integer, optional, intent(out) :: rc

         type(ESMF_State) :: importState, exportState, internalState
         integer :: status

         importState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_IMPORT, name=this%get_name(), _RC)
         exportState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_EXPORT, name=this%get_name(), _RC)
         internalState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_INTERNAL, name=this%get_name(), _RC)
         this%user_component%states = MultiState(importState=importState, exportState=exportState, internalState=internalState)

         _RETURN(_SUCCESS)
      end subroutine create_user_states
      
   end subroutine init_meta

   ! Deep copy of shallow ESMF objects - be careful using result
   ! TODO: Maybe this should return a POINTER
   type(ChildComponent) function get_child_by_name(this, child_name, rc) result(child_component)
      class(OuterMetaComponent), intent(in) :: this
      character(len=*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ChildComponent), pointer :: child_ptr

      child_ptr => this%children%at(child_name, rc=status)
      _ASSERT(associated(child_ptr), 'Child not found: <'//child_name//'>.')

      child_component = child_ptr

      _RETURN(_SUCCESS)
   end function get_child_by_name

   subroutine run_child_by_name(this, child_name, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      type(ESMF_Clock), intent(inout) :: clock
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ChildComponent) :: child
      logical :: found
      integer :: phase_idx

      child = this%get_child(child_name, _RC)

      phase_idx = 1
      if (present(phase_name)) then      
         phase_idx = get_phase_index(this%get_phases(ESMF_METHOD_RUN), phase_name=phase_name, found=found)
         _ASSERT(found, "run phase: <"//phase_name//"> not found.")
      end if

      call child%run(clock, phase_idx=phase_idx, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_child_by_name

   subroutine run_children_(this, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ChildComponentMapIterator) :: iter

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           call this%run_child(iter%first(), clock, phase_name=phase_name, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine run_children_


   function get_outer_meta_from_outer_gc(gridcomp, rc) result(outer_meta)
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _GET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE, outer_meta)

      _RETURN(_SUCCESS)
   end function get_outer_meta_from_outer_gc

   subroutine attach_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _SET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE, outer_meta)

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

   subroutine free_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper

      call MAPL_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not created for this gridcomp")

      call free_inner_meta(wrapper%outer_meta%user_component%gridcomp)

      deallocate(wrapper%outer_meta)

      _RETURN(_SUCCESS)
   end subroutine free_outer_meta

   function get_phases(this, method_flag) result(phases)
      use :: esmf, only: ESMF_Method_Flag
      use :: gFTL2_StringVector, only: StringVector
      type(StringVector), pointer :: phases
      class(OuterMetaComponent), target, intent(inout):: this
      type(ESMF_Method_Flag), intent(in) :: method_flag

      phases => this%phases_map%of(method_flag)

   end function get_phases

   ! Reexamine the names of the next 2 procedures when there is a
   ! clearer use case.  Might only be needed from within inner meta.
!!$   type(ESMF_GridComp) function get_gridcomp(this) result(gridcomp)
!!$      class(OuterMetaComponent), intent(in) :: this
!!$
!!$      gridcomp = this%self_gridcomp
!!$      
!!$   end function get_gridcomp
!!$
   type(ESMF_GridComp) function get_user_gridcomp(this) result(gridcomp)
      class(OuterMetaComponent), intent(in) :: this

      gridcomp = this%user_component%gridcomp
      
   end function get_user_gridcomp

   type(MultiState) function get_user_states(this) result(states)
      class(OuterMetaComponent), intent(in) :: this

      states = this%user_component%states
      
   end function get_user_states


   subroutine set_hconfig(this, hconfig)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_HConfig), intent(in) :: hconfig

      this%hconfig = hconfig

   end subroutine set_hconfig

   function get_hconfig(this) result(hconfig)
      type(ESMF_Hconfig) :: hconfig
      class(OuterMetaComponent), intent(inout) :: this

      hconfig = this%hconfig

   end function get_hconfig

!!$
!!$
!!$   subroutine get_yaml_hconfig(this, hconfig)
!!$      class(OuterMetaComponent), target, intent(inout) :: this
!!$      class(YAML_Node), pointer :: hconfig
!!$
!!$      hconfig => null
!!$      if (.not. allocated(this%yaml_cfg)) return
!!$
!!$      hconfig => this%yaml_cfg
!!$
!!$   end subroutine get_yaml_hconfig

   subroutine set_user_setservices(this, user_setservices)
      class(OuterMetaComponent), intent(inout) :: this
      class(AbstractUserSetServices), intent(in) :: user_setservices
      this%user_component%setservices = user_setservices
   end subroutine set_user_setservices


   ! ESMF initialize methods

   !----------
   ! The procedure initialize_geom() is responsible for passing grid
   ! down to children.  The parent geom can be overridden by a
   ! component by:
   !   - providing a geom spec in the generic section of its config
   !     file, or
   !   - specifying an INIT_GEOM phase
   ! If both are specified, the INIT_GEOM overrides the config spec.
   ! ---------
   recursive subroutine initialize_geom(this, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom), pointer :: mapl_geom
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_GEOM'
      type(GeomManager), pointer :: geom_mgr

      if (this%component_spec%has_geom_hconfig()) then
         geom_mgr => get_geom_manager()
         mapl_geom => geom_mgr%get_mapl_geom(this%component_spec%geom_hconfig, _RC)
         this%geom = mapl_geom%get_geom()
      end if

      call exec_user_init_phase(this, clock, PHASE_NAME, _RC)
      call apply_to_children(this, set_child_geom, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_GEOM, _RC)

      _RETURN(ESMF_SUCCESS)
   contains

      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc

         integer :: status
         
         if (allocated(this%geom)) then
            call child_meta%set_geom(this%geom)
         end if
         if (allocated(this%vertical_geom)) then
            call child_meta%set_vertical_geom(this%vertical_geom)
         end if

         _RETURN(ESMF_SUCCESS)
      end subroutine set_child_geom

   end subroutine initialize_geom

   recursive subroutine initialize_advertise(this, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call exec_user_init_phase(this, clock, PHASE_NAME, _RC)

      call self_advertise(this, _RC)
      call apply_to_children(this, add_subregistry, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_ADVERTISE, _RC)

      call process_connections(this, _RC)
      call this%registry%propagate_unsatisfied_imports(_RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

      subroutine add_subregistry(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc

         call this%registry%add_subregistry(child_meta%get_registry())

         _RETURN(ESMF_SUCCESS)
      end subroutine add_subregistry


      subroutine self_advertise(this, unusable, rc)
         class(OuterMetaComponent), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(VariableSpecVectorIterator) :: iter
         type(VariableSpec), pointer :: var_spec

         if (this%component_spec%var_specs%size() > 0) then
            _ASSERT(allocated(this%geom),'Component must define a geom to advertise variables.')
         end if
         associate (e => this%component_spec%var_specs%end())
           iter = this%component_spec%var_specs%begin()
           do while (iter /= e)
              var_spec => iter%of()
              call advertise_variable (var_spec, this%registry, this%geom, this%vertical_geom,  _RC)
              call iter%next()
           end do
         end associate

         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine self_advertise


      subroutine advertise_variable(var_spec, registry, geom, vertical_geom, unusable, rc)
         type(VariableSpec), intent(in) :: var_spec
         type(HierarchicalRegistry), intent(inout) :: registry
         type(ESMF_Geom), intent(in) :: geom
         type(VerticalGeom), intent(in) :: vertical_geom
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         class(AbstractStateItemSpec), allocatable :: item_spec
         type(VirtualConnectionPt) :: virtual_pt
         integer :: i
         type(ActualPtVector) :: dependencies
         type(StateItemSpecPtr), allocatable :: dependency_specs(:)

         _ASSERT(var_spec%itemtype /= MAPL_STATEITEM_UNKNOWN, 'Invalid type id in variable spec <'//var_spec%short_name//'>.')

         item_spec = var_spec%make_ItemSpec(geom, vertical_geom, _RC)
         dependencies = item_spec%get_dependencies(_RC)
         associate (n => dependencies%size())
           allocate(dependency_specs(n))
           do i = 1, n
              dependency_specs(i)%ptr =>  registry%get_item_spec(dependencies%of(i), _RC)
           end do
           call item_spec%create(dependency_specs, _RC)
         end associate

         virtual_pt = var_spec%make_virtualPt()
         call registry%add_item_spec(virtual_pt, item_spec)

         
         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine advertise_variable



     subroutine process_connections(this, rc)
        use mapl3g_VirtualConnectionPt
        class(OuterMetaComponent), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer :: status
        type(ConnectionVectorIterator) :: iter

        associate (e => this%component_spec%connections%end())
          iter = this%component_spec%connections%begin()
          do while (iter /= e)
             call this%registry%add_connection(iter%of(), _RC)
             call iter%next()
          end do
        end associate

        _RETURN(_SUCCESS)
     end subroutine process_connections
  end subroutine initialize_advertise

  recursive subroutine initialize_post_advertise(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_POST_ADVERTISE'
      type(MultiState) :: outer_states

      call exec_user_init_phase(this, clock, PHASE_NAME, _RC)
      call this%registry%add_to_states(this%user_component%states, mode='user', _RC)
      this%state_extensions = this%registry%get_extensions()
      
      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)

      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_POST_ADVERTISE, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_post_advertise



   recursive subroutine initialize_realize(this, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE'

      call exec_user_init_phase(this, clock, PHASE_NAME, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_REALIZE, _RC)

      call this%registry%allocate(_RC)
      
      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

   end subroutine initialize_realize

   subroutine exec_user_init_phase(this, clock, phase_name, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase_name
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(StringVector), pointer :: init_phases
      logical :: found

      init_phases => this%phases_map%at(ESMF_METHOD_INITIALIZE, _RC)
      ! User gridcomp may not have any given phase; not an error condition if not found.
      associate (phase => get_phase_index(init_phases, phase_name=phase_name, found=found))
        _RETURN_UNLESS(found)
        associate ( &
             importState => this%user_component%states%importState, &
             exportState => this%user_component%states%exportState)
          
          call ESMF_GridCompInitialize(this%user_component%gridcomp, &
               importState=importState, exportState=exportState, &
               clock=clock, phase=phase, userRC=userRC, _RC)
          _VERIFY(userRC)
        end associate
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine exec_user_init_phase

   recursive subroutine apply_to_children_simple(this, clock, phase_idx, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      type(ChildComponentMapIterator) :: iter
      type(ChildComponent), pointer :: child

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           call child%initialize(clock, phase_idx=phase_idx, _RC)
           call iter%next()
        end do
      end associate

   end subroutine apply_to_children_simple

   ! This procedure should not be invoked recursively - it is not for traversing the tree,
   ! but rather just to facilitate custom operations where a parent component must pass
   ! information to its children.
   subroutine apply_to_children_custom(this, oper, rc)
      class(OuterMetaComponent), intent(inout) :: this
      procedure(I_child_op) :: oper
      integer, optional, intent(out) :: rc

      integer :: status
      type(ChildComponentMapIterator) :: iter
      type(ChildComponent), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           child_outer_gc = child%get_outer_gridcomp()
           child_meta => get_outer_meta(child_outer_gc, _RC)
           call oper(this, child_meta, _RC)
           call iter%next()
        end do
      end associate

   end subroutine apply_to_children_custom

   recursive subroutine initialize_user(this, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_USER'

      call exec_user_init_phase(this, clock, PHASE_NAME, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_USER, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_user

   recursive subroutine initialize(this, importState, exportState, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      if (.not. present(phase_name)) then
         call exec_user_init_phase(this, clock, phase_name, _RC)
         _RETURN(ESMF_SUCCESS)
      end if

      select case (phase_name)
      case ('GENERIC::INIT_GEOM')
         call this%initialize_geom(clock, _RC)
      case ('GENERIC::INIT_ADVERTISE')
         call this%initialize_advertise(clock, _RC)
      case ('GENERIC::INIT_USER')
         call this%initialize_user(clock, _RC)
      case default ! custom user phase - does not auto propagate to children
         call exec_user_init_phase(this, clock, phase_name, _RC)
      end select

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize


   recursive subroutine run(this, clock, phase_name, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Clock) :: clock
      ! optional arguments
      character(len=*), optional, intent(in) :: phase_name
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC, i
      integer :: phase_idx
      type(StateExtension), pointer :: extension
      logical :: found

      associate(phase_idx => get_phase_index(this%phases_map%of(ESMF_METHOD_RUN), phase_name=phase_name, found=found))
        _ASSERT(found, "run phase: <"//phase_name//"> not found.")
        call ESMF_GridCompRun(this%user_component%gridcomp, &
             importState=this%user_component%states%importState, &
             exportState=this%user_component%states%exportState, &
             clock=clock, phase=phase_idx, userRC=userRC, _RC)
        _VERIFY(userRC)
      end associate

      ! TODO:  extensions should depend on phase ...
      do i = 1, this%state_extensions%size()
         extension => this%state_extensions%of(i)
         call extension%run(_RC)
      end do

      _RETURN(ESMF_SUCCESS)
   end subroutine run

   recursive subroutine finalize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ChildComponent), pointer :: child
      type(ChildComponentMapIterator) :: iter
      integer :: status, userRC
      character(*), parameter :: PHASE_NAME = 'GENERIC::FINALIZE_USER'
      type(StringVector), pointer :: finalize_phases
      logical :: found

      finalize_phases => this%phases_map%at(ESMF_METHOD_FINALIZE, _RC)
      ! User gridcomp may not have any given phase; not an error condition if not found.
      associate (phase => get_phase_index(finalize_phases, phase_name=phase_name, found=found))
        _RETURN_UNLESS(found)
        associate ( &
             importState => this%user_component%states%importState, &
             exportState => this%user_component%states%exportState)
          
          call ESMF_GridCompFinalize(this%user_component%gridcomp, importState=importState, exportState=exportState, &
               clock=clock, userRC=userRC, _RC)
          _VERIFY(userRC)
        end associate

        associate(b => this%children%begin(), e => this%children%end())
          iter = b
          do while (iter /= e)
             child => iter%second()
             call child%finalize(clock, phase_idx=GENERIC_FINALIZE_USER, _RC)
             call iter%next()
          end do
        end associate
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine finalize

   subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc


      _RETURN(ESMF_SUCCESS)
   end subroutine read_restart


   subroutine write_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine write_restart


   function get_name(this, rc) result(name)
      character(:), allocatable :: name
      class(OuterMetaComponent), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%self_gridcomp, name=buffer, _RC)
      name=trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_name


   function get_user_gridcomp_name(this, rc) result(inner_name)
      character(:), allocatable :: inner_name
      class(OuterMetaComponent), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%user_component%gridcomp, name=buffer, _RC)
      inner_name=trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_user_gridcomp_name


   recursive subroutine traverse(this, unusable, pre, post, rc)
      class(OuterMetaComponent), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      interface
         subroutine I_NodeOp(node, rc)
            import OuterMetaComponent
            class(OuterMetaComponent), intent(inout) :: node
            integer, optional, intent(out) :: rc
         end subroutine I_NodeOp
      end interface
      
      procedure(I_NodeOp), optional :: pre
      procedure(I_NodeOp), optional :: post
      integer, optional, intent(out) :: rc

      integer :: status
      type(ChildComponentMapIterator) :: iter
      type(ChildComponent), pointer :: child
      class(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc

      if (present(pre)) then
         call pre(this, _RC)
      end if

      associate (b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           child_outer_gc = child%get_outer_gridcomp()
           child_meta => get_outer_meta(child_outer_gc, _RC)
           call child_meta%traverse(pre=pre, post=post, _RC)
           call iter%next()
        end do
      end associate

      if (present(post)) then
         call post(this, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine traverse


   ! Needed for unit testing purposes.
   
   function get_gridcomp(this) result(gridcomp)
      type(ESMF_GridComp) :: gridcomp
      class(OuterMetaComponent), intent(in) :: this
      gridcomp = this%self_gridcomp
   end function get_gridcomp

!!$   subroutine validate_user_short_name(this, short_name, rc)
!!$
!!$      integer :: status
!!$      _ASSERT(len(short_name) > 0, 'Short names must have at least one character.')
!!$      _ASSERT(0 == verify(short_name(1:1), LOWER//UPPER), 'Short name must start with a character.')
!!$      _ASSERT(0 == verify(short_name, ALPHANUMERIC // '_'), 'Illegal short name.')
!!$
!!$      _RETURN(_SUCCESS)
!!$   end subroutine validate_user_short_name


   pure logical function is_root(this)
      class(OuterMetaComponent), intent(in) :: this
      is_root = this%is_root_
   end function is_root


   subroutine set_geom(this, geom)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      this%geom = geom

   end subroutine set_geom

   subroutine set_vertical_geom(this, vertical_geom)
      class(OuterMetaComponent), intent(inout) :: this
      type(VerticalGeom), intent(in) :: verticaL_geom

      this%vertical_geom = vertical_geom

   end subroutine set_vertical_geom
 
  function get_registry(this) result(registry)
      type(HierarchicalRegistry), pointer :: registry
      class(OuterMetaComponent), target, intent(in) :: this

      registry => this%registry
   end function get_registry


   function get_component_spec(this) result(component_spec)
      type(ComponentSpec), pointer :: component_spec
      class(OuterMetaComponent), target, intent(in) :: this
      component_spec => this%component_spec
   end function get_component_spec


   !TODO: put "user" in procedure name
   function get_internal_state(this) result(internal_state)
      type(ESMF_State) :: internal_state
      class(OuterMetaComponent), intent(in) :: this

     internal_state = this%user_component%states%internalState

   end function get_internal_state


   function get_lgr(this) result(lgr)
      class(Logger), pointer :: lgr
      class(OuterMetaComponent), target, intent(in) :: this

      lgr => this%lgr

   end function get_lgr

end module mapl3g_OuterMetaComponent
