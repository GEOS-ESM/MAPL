#include "MAPL_Generic.h"

module mapl3g_OuterMetaComponent
   use mapl3g_UserSetServices,   only: AbstractUserSetServices
   use mapl3g_VariableSpec
   use mapl3g_StateItemSpecTypeId
   use mapl3g_ExtraDimsSpec
   use mapl3g_InvalidSpec
   use mapl3g_FieldSpec
!!$   use mapl3g_BundleSpec
   use mapl3g_StateSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_VariableSpecVector
   use mapl3g_GenericConfig
   use mapl3g_ComponentSpec
   use mapl3g_GenericPhases
   use mapl3g_ChildComponent
   use mapl3g_Validation, only: is_valid_name
!!$   use mapl3g_CouplerComponentVector
   use mapl3g_InnerMetaComponent
   use mapl3g_MethodPhasesMap
   use mapl3g_ChildComponentMap, only: ChildComponentMap
   use mapl3g_ChildComponentMap, only: ChildComponentMapIterator
   use mapl3g_ChildComponentMap, only: operator(/=)
   use mapl3g_AbstractStateItemSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_ConnectionPt
   use mapl3g_ConnectionSpec
   use mapl3g_HierarchicalRegistry
   use mapl3g_ESMF_Interfaces, only: I_Run, MAPL_UserCompGetInternalState, MAPL_UserCompSetInternalState
   use mapl_ErrorHandling
   use gFTL2_StringVector
   use mapl_keywordEnforcer, only: KE => KeywordEnforcer
   use esmf
   use yaFyaml, only: YAML_Node
   use pflogger, only: logging, Logger
   implicit none
   private

   public :: OuterMetaComponent
   public :: get_outer_meta
   public :: attach_outer_meta
   public :: free_outer_meta

   type :: OuterMetaComponent
      private
      
      type(ESMF_GridComp)                         :: self_gridcomp
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(ESMF_GeomBase), allocatable            :: geom_base
      type(ESMF_State)                            :: esmf_internalState
      type(GenericConfig)                         :: config
      type(ChildComponentMap)                     :: children
      logical                                     :: is_root_ = .false.

      type(ESMF_GridComp)                         :: user_gridcomp
      type(MethodPhasesMap)                       :: phases_map
      type(InnerMetaComponent), allocatable       :: inner_meta

      class(Logger), pointer :: lgr  ! "MAPL.Generic" // name

      type(ComponentSpec)                         :: component_spec
      type(OuterMetaComponent), pointer           :: parent_private_state
      type(HierarchicalRegistry) :: registry

   contains
      procedure :: set_esmf_config
      procedure :: set_yaml_config
      generic   :: set_config => set_esmf_config, set_yaml_config

      procedure :: get_phases
!!$      procedure :: get_gridcomp
      procedure :: get_user_gridcomp
      procedure :: set_user_setServices
      procedure :: set_entry_point

      ! Generic methods
      procedure :: setServices => setservices_

      procedure :: initialize ! main/any phase
      procedure :: initialize_user
      procedure :: initialize_geom_base
      procedure :: initialize_advertise
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

      procedure :: set_geom_base
      procedure :: get_name
      procedure :: get_inner_name
      procedure :: get_gridcomp
      procedure :: is_root
      procedure :: get_registry
      procedure :: get_subregistries

      procedure :: get_component_spec

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

      module subroutine add_child_by_name(this, child_name, setservices, config, rc)
         class(OuterMetaComponent), intent(inout) :: this
         character(len=*), intent(in) :: child_name
         class(AbstractUserSetServices), intent(in) :: setservices
         type(GenericConfig), intent(in) :: config
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

contains


   ! Keep the constructor simple
   type(OuterMetaComponent) function new_outer_meta(gridcomp, user_gridcomp, set_services, config) result(outer_meta)
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(ESMF_GridComp), intent(in) :: user_gridcomp
      class(AbstractUserSetServices), intent(in) :: set_services
      type(GenericConfig), intent(in) :: config

      outer_meta%self_gridcomp = gridcomp
      outer_meta%user_setservices = set_services
      outer_meta%user_gridcomp = user_gridcomp
      outer_meta%config = config

      !TODO: this may be able to move outside of constructor
      call initialize_phases_map(outer_meta%phases_map)

   end function new_outer_meta

   subroutine initialize_meta(this, gridcomp)
      class(OuterMetaComponent), intent(out) :: this
      type(ESMF_GridComp), intent(inout) :: gridcomp

      this%self_gridcomp = gridcomp
      call initialize_phases_map(this%phases_map)

   end subroutine initialize_meta

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
      integer :: phase_idx

      child = this%get_child(child_name, _RC)

      phase_idx = GENERIC_INIT_USER
      if (present(phase_Name)) then
         phase_idx = get_phase_index(this%get_phases(ESMF_METHOD_RUN), phase_name=phase_name, _RC)
        _ASSERT(phase_idx /= -1,'No such run phase: <'//phase_name//'>.')
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

      call initialize_meta(outer_meta, gridcomp)
      outer_meta%lgr => logging%get_logger('MAPL.GENERIC')

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

   subroutine free_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper

      call MAPL_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not created for this gridcomp")

      call free_inner_meta(wrapper%outer_meta%user_gridcomp)
      
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

      gridcomp = this%user_gridcomp
      
   end function get_user_gridcomp


   subroutine set_esmf_config(this, config)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Config), intent(in) :: config

      this%config%esmf_cfg = config

   end subroutine set_esmf_config

   subroutine set_yaml_config(this, config)
      class(OuterMetaComponent), intent(inout) :: this
      class(YAML_Node), intent(in) :: config

      allocate(this%config%yaml_cfg, source=config)

   end subroutine set_yaml_config

   subroutine set_user_setservices(this, user_setservices)
      class(OuterMetaComponent), intent(inout) :: this
      class(AbstractUserSetServices), intent(in) :: user_setservices
      this%user_setServices = user_setservices
   end subroutine set_user_setservices


   ! ESMF initialize methods

   ! initialize_geom() is responsible for passing grid down to
   ! children.  User component can insert a different grid using
   ! GENERIC_INIT_GRID phase in their component.
   recursive subroutine initialize_geom_base(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_GRID'

      call exec_user_init_phase(this, importState, exportState, clock, PHASE_NAME, _RC)
      call apply_to_children(this, set_child_geom, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_GRID, _RC)

      _RETURN(ESMF_SUCCESS)
   contains

      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc

         integer :: status
         
         if (allocated(this%geom_base)) then
            call child_meta%set_geom_base(this%geom_base)
         end if

         _RETURN(ESMF_SUCCESS)
      end subroutine set_child_geom

   end subroutine initialize_geom_base

   recursive subroutine initialize_advertise(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call exec_user_init_phase(this, importState, exportState, clock, PHASE_NAME, _RC)
      call self_advertise(this, _RC)
      call apply_to_children(this, add_subregistry, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_ADVERTISE, _RC)

      call process_connections(this, _RC)

!!$      call this%registry%add_to_states(&
!!$           importState=importState, &
!!$           exportState=exportState, &
!!$           internalState=this%esmf_internalState, _RC)

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

         associate (e => this%component_spec%var_specs%end())
           iter = this%component_spec%var_specs%begin()
           do while (iter /= e)
              var_spec => iter%of()
              call advertise_variable (var_spec, this%registry, this%geom_base, _RC)
              call iter%next()
           end do
         end associate

         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine self_advertise


      subroutine advertise_variable(var_spec, registry, geom_base, unusable, rc)
         type(VariableSpec), intent(in) :: var_spec
         type(HierarchicalRegistry), intent(inout) :: registry
         type(ESMF_GeomBase), intent(in) :: geom_base
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         class(AbstractStateItemSpec), allocatable :: item_spec
         type(VirtualConnectionPt) :: virtual_pt
         type(ExtraDimsSpec) :: extra_dims

         _ASSERT(var_spec%type_id /= MAPL_TYPE_ID_INVALID, 'Invalid type id in variable spec <'//var_spec%short_name//'>.')
         item_spec = create_item_spec(var_spec%type_id)
         call item_spec%initialize(geom_base, var_spec, _RC)
         call item_spec%create(_RC)

         virtual_pt = VirtualConnectionPt(var_spec%state_intent, var_spec%short_name)
         call registry%add_item_spec(virtual_pt, item_spec)
         
         
         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine advertise_variable


      function create_item_spec(type_id) result(item_spec)
         class(AbstractStateItemSpec), allocatable :: item_spec
         type(StateItemSpecTypeId), intent(in) :: type_id
         
        if (type_id == MAPL_TYPE_ID_FIELD) then
           allocate(FieldSpec::item_spec)
!!$        else if (type_id == MAPL_TYPE_ID_BUNDLE) then
!!$           allocate(BundleSpec::item_spec)
        else if (type_id == MAPL_TYPE_ID_STATE) then
           allocate(StateSpec::item_spec)
        else
           ! We return an invalid item that will throw exceptions when
           ! used.
           allocate(InvalidSpec::item_spec)
        end if

     end function create_item_spec


     subroutine process_connections(this, rc)
        use mapl3g_VirtualConnectionPt
        use mapl3g_ConnectionSpec
        use mapl3g_ConnectionPt
        class(OuterMetaComponent), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer :: status

        type(VirtualConnectionPt) :: pt_a
        type(VirtualConnectionPt) :: pt_b
        type(ConnectionSpec) :: conn

        if (this%get_inner_name() == 'P') then
           pt_a = VirtualConnectionPt(state_intent='export', short_name='E_1')
           pt_b = VirtualConnectionPt(state_intent='import', short_name='E_1')
           
           conn = ConnectionSpec(ConnectionPt('CHILD_A',pt_a), ConnectionPt('CHILD_B', pt_b))
           call this%registry%add_connection(conn, _RC)
        end if
        
        
        _RETURN(_SUCCESS)
     end subroutine process_connections
  end subroutine initialize_advertise

   recursive subroutine initialize_realize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE'

      call this%registry%add_to_states(&
           importState=importState, &
           exportState=exportState, &
           internalState=this%esmf_internalState, _RC)

      call exec_user_init_phase(this, importState, exportState, clock, PHASE_NAME, _RC)
      call apply_to_children(this, clock, phase_idx=GENERIC_INIT_REALIZE, _RC)

      call this%registry%allocate(_RC)

      _RETURN(ESMF_SUCCESS)
   contains

   end subroutine initialize_realize

   subroutine exec_user_init_phase(this, importState, exportState, clock, phase_name, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase_name
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(StringVector), pointer :: init_phases

      init_phases => this%phases_map%at(ESMF_METHOD_INITIALIZE, _RC)
      ! User gridcomp may not have any given phase; not an error condition if not found.
      associate (phase => get_phase_index(init_phases, phase_name=phase_name, rc=status))
        if (phase /= -1) then
           call ESMF_GridCompInitialize(this%user_gridcomp, &
                importState=importState, exportState=exportState, &
                clock=clock, phase=phase, userRC=userRC, _RC)
           _VERIFY(userRC)
        end if
      end associate

      _RETURN(ESMF_SUCCESS)
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

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           child_meta => get_outer_meta(child%gridcomp, _RC)
           call oper(this, child_meta, _RC)
           call iter%next()
        end do
      end associate

   end subroutine apply_to_children_custom

   recursive subroutine initialize_user(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_USER'

      call exec_user_init_phase(this, importState, exportState, clock, PHASE_NAME, _RC)
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
      
      _HERE
      associate (phase => get_phase_index(this%phases_map%of(ESMF_METHOD_INITIALIZE), phase_name=phase_name, rc=status))
        if (status == _SUCCESS) then
           call ESMF_GridCompInitialize(this%user_gridcomp, importState=importState, exportState=exportState, &
                clock=clock, userRC=userRC, phase=phase, _RC)
           _VERIFY(userRC)
        end if
      end associate

      if (.not. present(phase_name)) then
         call this%initialize_user(importState, exportState, clock, _RC)
         _RETURN(ESMF_SUCCESS)
      end if

      _ASSERT(this%phases_map%count(ESMF_METHOD_RUN) > 0, "No phases registered for ESMF_METHOD_RUN.")

      _HERE
      select case (phase_name)
      case ('GENERIC::INIT_GRID')
         call this%initialize_geom_base(importState, exportState, clock, _RC)
      case ('GENERIC::INIT_ADVERTISE')
         call this%initialize_advertise(importState, exportState, clock, _RC)
      case ('GENERIC::INIT_USER')
         call this%initialize_user(importState, exportState, clock, _RC)
      case default
         _FAIL('unsupported initialize phase: '// phase_name)
      end select
      _HERE
      _RETURN(ESMF_SUCCESS)
   end subroutine initialize


   recursive subroutine run(this, importState, exportState, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      integer :: phase_idx

      phase_idx = 1
      if (present(phase_name)) then
         _ASSERT(this%phases_map%count(ESMF_METHOD_RUN) > 0, "No phases registered for ESMF_METHOD_RUN.")
         phase_idx = get_phase_index(this%phases_map%of(ESMF_METHOD_RUN), phase_name=phase_name, _RC)
      end if

      call ESMF_GridCompRun(this%user_gridcomp, importState=importState, exportState=exportState, &
           clock=clock, phase=phase_idx, userRC=userRC, _RC)
      _VERIFY(userRC)

!!$      call child couplers


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

      call ESMF_GridCompFinalize(this%user_gridcomp, importState=importState, exportState=exportState, &
           clock=clock, userRC=userRC, _RC)
      _VERIFY(userRC)

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           call child%finalize(clock, phase_name=get_default_phase_name(ESMF_METHOD_FINALIZE), _RC)
           call iter%next()
        end do
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


   function get_inner_name(this, rc) result(inner_name)
      character(:), allocatable :: inner_name
      class(OuterMetaComponent), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: buffer

      call ESMF_GridCompGet(this%user_gridcomp, name=buffer, _RC)
      inner_name=trim(buffer)

      _RETURN(ESMF_SUCCESS)
   end function get_inner_name



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

      if (present(pre)) then
         call pre(this, _RC)
      end if

      associate (b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           child_meta => get_outer_meta(child%gridcomp, _RC)
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


   subroutine set_geom_base(this, geom_base)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_GeomBase), intent(in) :: geom_base

      this%geom_base = geom_base

   end subroutine set_geom_base

   function get_registry(this) result(r)
      type(HierarchicalRegistry), pointer :: r
      class(OuterMetaComponent), target, intent(in) :: this

      r => this%registry
   end function get_registry

   subroutine get_subregistries(this, subregistries, rc)
      use mapl3g_RegistryPtrMap
      use mapl3g_RegistryPtr
      class(OuterMetaComponent), intent(in) :: this
      type(RegistryPtrMap), intent(out) :: subregistries
      integer, optional, intent(out) :: rc

      type(ChildComponentMapIterator) :: iter
      character(:), pointer :: name
      type(ChildComponent), pointer :: child
      type(Outermetacomponent), pointer :: child_meta
      type(RegistryPtr) :: wrap
      
      associate (e => this%children%end())
        iter = this%children%begin()

        do while (iter /= e)
           name => iter%first()
           child => iter%second()
           child_meta => get_outer_meta(child%gridcomp)
           wrap%registry => child_meta%get_registry()

           call subregistries%insert(name, wrap)

           call iter%next()
        end do

      end associate

      _RETURN(_SUCCESS)
   end subroutine get_subregistries

   function get_component_spec(this) result(component_spec)
      type(ComponentSpec), pointer :: component_spec
      class(OuterMetaComponent), target, intent(in) :: this
      component_spec => this%component_spec
   end function get_component_spec

end module mapl3g_OuterMetaComponent
