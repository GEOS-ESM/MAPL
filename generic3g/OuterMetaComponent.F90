#include "MAPL_Generic.h"

module mapl3g_OuterMetaComponent

   use mapl3g_geom_mgr, only: GeomManager, MaplGeom, get_geom_manager
   use mapl3g_UserSetServices,   only: AbstractUserSetServices
   use mapl3g_VariableSpec
   use mapl3g_StateItem
   use mapl3g_MultiState
   use mapl3g_VariableSpecVector
   use mapl3g_ComponentSpec
   use mapl3g_GenericPhases
   use mapl3g_Validation, only: is_valid_name
   use mapl3g_InnerMetaComponent
   use mapl3g_MethodPhasesMap
   use mapl3g_StateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_MatchConnection
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualPtVector
   use mapl3g_ConnectionVector
   use mapl3g_HierarchicalRegistry
   use mapl3g_StateExtension
   use mapl3g_ExtensionVector
   use mapl3g_ESMF_Interfaces, only: I_Run, MAPL_UserCompGetInternalState, MAPL_UserCompSetInternalState
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_GriddedComponentDriverMap, only: GriddedComponentDriverMap
   use mapl3g_GriddedComponentDriverMap, only: GriddedComponentDriverMapIterator
   use mapl3g_GriddedComponentDriverMap, only: operator(/=)
   use mapl3g_ActualPtComponentDriverMap
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INVALIDATE
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_UPDATE
   use mapl_ErrorHandling
   use mapl3g_VerticalGeom
   use mapl3g_GeometrySpec
   use gFTL2_StringVector
   use mapl_keywordEnforcer, only: KE => KeywordEnforcer
   use esmf
   use pflogger, only: logging, Logger
   use pFIO, only: FileMetaData, o_Clients
   use mapl3g_geomio, only: GeomPFIO, bundle_to_metadata, make_geom_pfio, get_mapl_geom
   use mapl3g_Restart, only: Restart

   implicit none
   private

   public :: OuterMetaComponent
   public :: get_outer_meta
   public :: attach_outer_meta
   public :: free_outer_meta

   type :: OuterMetaComponent
      private
      
      type(ESMF_GridComp)                         :: self_gridcomp
      type(GriddedComponentDriver)                :: user_gc_driver
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(MethodPhasesMap)                       :: user_phases_map
      type(ESMF_HConfig)                          :: hconfig

      type(ESMF_Geom), allocatable                :: geom
      type(VerticalGeom), allocatable             :: vertical_geom

      type(InnerMetaComponent), allocatable       :: inner_meta

      ! Hierarchy
      type(GriddedComponentDriverMap)             :: children
      type(HierarchicalRegistry) :: registry
 
      class(Logger), pointer :: lgr  => null() ! "MAPL.Generic" // name

      type(ComponentSpec)                         :: component_spec

      integer :: counter

   contains

      procedure :: get_user_gc_driver
      procedure :: set_hconfig
      procedure :: get_hconfig
      procedure :: get_geom
      procedure :: get_registry
      procedure :: get_lgr

      procedure :: get_phases

      ! Generic methods
      procedure :: setServices => setservices_

      procedure :: init_meta  ! object

      procedure :: run_custom
      procedure :: initialize_user
      procedure :: initialize_advertise_geom
      procedure :: initialize_realize_geom
      procedure :: initialize_advertise
      procedure :: initialize_post_advertise
      procedure :: initialize_realize

      procedure :: run_user
      procedure :: run_clock_advance
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

      procedure :: set_entry_point
      procedure :: set_geom
      procedure :: get_name
      procedure :: get_gridcomp

      procedure :: get_component_spec
      procedure :: get_internal_state

      procedure :: set_vertical_geom

      procedure :: connect_all

   end type OuterMetaComponent

   type OuterMetaWrapper
      type(OuterMetaComponent), pointer :: outer_meta
   end type OuterMetaWrapper


   interface get_outer_meta
      module procedure :: get_outer_meta_from_outer_gc
   end interface get_outer_meta

   character(len=*), parameter :: OUTER_META_PRIVATE_STATE = "MAPL::OuterMetaComponent::private"



   ! Submodule interfaces
   interface

      recursive module subroutine SetServices_(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, intent(out) ::rc
      end subroutine

      module recursive subroutine add_child_by_name(this, child_name, setservices, hconfig, rc)
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

   interface recurse
      module procedure recurse_
   end interface recurse

   interface apply_to_children
      module procedure apply_to_children_custom
   end interface apply_to_children

   integer, save :: counter = 0

contains


   ! Keep the constructor simple
   type(OuterMetaComponent) function new_outer_meta(gridcomp, user_gc_driver, user_setServices, hconfig) result(outer_meta)
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(GriddedComponentDriver), intent(in) :: user_gc_driver
      class(AbstractUserSetServices), intent(in) :: user_setservices
      type(ESMF_HConfig), intent(in) :: hconfig

      
      outer_meta%self_gridcomp = gridcomp
      outer_meta%user_gc_driver = user_gc_driver
      allocate(outer_meta%user_setServices, source=user_setServices)
      outer_meta%hconfig = hconfig

      counter = counter + 1
      outer_meta%counter = counter
      call initialize_phases_map(outer_meta%user_phases_map)

   end function new_outer_meta

   ! NOTE: _Not_ an ESMF phase - this is initializing the object itself.
   ! Constructor (new_outer_meta) only copies basic parameters.  All
   ! other initialization is in this procedure.

   subroutine init_meta(this, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: user_gc_name

      user_gc_name = this%user_gc_driver%get_name(_RC)
      this%registry = HierarchicalRegistry(user_gc_name)

      this%lgr => logging%get_logger('MAPL.GENERIC')

      _RETURN(_SUCCESS)
      
   end subroutine init_meta

   ! Deep copy of shallow ESMF objects - be careful using result
   ! TODO: Maybe this should return a POINTER
   type(GriddedComponentDriver) function get_child_by_name(this, child_name, rc) result(child_component)
      class(OuterMetaComponent), intent(in) :: this
      character(len=*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GriddedComponentDriver), pointer :: child_ptr

      child_ptr => this%children%at(child_name, rc=status)
      _ASSERT(associated(child_ptr), 'Child not found: <'//child_name//'>.')

      child_component = child_ptr

      _RETURN(_SUCCESS)
   end function get_child_by_name

   recursive subroutine run_child_by_name(this, child_name, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child
      logical :: found
      integer :: phase_idx

      child = this%get_child(child_name, _RC)

      phase_idx = 1
      if (present(phase_name)) then      
         phase_idx = get_phase_index(this%get_phases(ESMF_METHOD_RUN), phase_name=phase_name, found=found)
         _ASSERT(found, "run phase: <"//phase_name//"> not found.")
      end if

      call child%run(phase_idx=phase_idx, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_child_by_name

   recursive subroutine run_children_(this, unusable, phase_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           call this%run_child(iter%first(), phase_name=phase_name, _RC)
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
      type(ESMF_GridComp) :: user_gridcomp

      call MAPL_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not created for this gridcomp")

      user_gridcomp = wrapper%outer_meta%user_gc_driver%get_gridcomp()
      call free_inner_meta(user_gridcomp, _RC)

      deallocate(wrapper%outer_meta)

      _RETURN(_SUCCESS)
   end subroutine free_outer_meta

   function get_phases(this, method_flag) result(phases)
      use :: esmf, only: ESMF_Method_Flag
      use :: gFTL2_StringVector, only: StringVector
      type(StringVector), pointer :: phases
      class(OuterMetaComponent), target, intent(inout):: this
      type(ESMF_Method_Flag), intent(in) :: method_flag

      phases => this%user_phases_map%of(method_flag)

   end function get_phases

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

   function get_geom(this) result(geom)
      type(ESMF_Geom) :: geom
      class(OuterMetaComponent), intent(inout) :: this

      geom = this%geom

   end function get_geom

   ! ESMF initialize methods

   !----------
   !The parent geom can be overridden by a
   ! component by:
   !   - providing a geom spec in the generic section of its config
   !     file, or
   !   - specifying an INIT_GEOM phase
   ! If both are specified, the INIT_GEOM overrides the config spec.
   !----------
   recursive subroutine initialize_advertise_geom(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom), pointer :: mapl_geom
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE_GEOM'
      type(GeomManager), pointer :: geom_mgr
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta

      associate (geometry_spec => this%component_spec%geometry_spec)
        if (allocated(geometry_spec%geom_spec)) then
           geom_mgr => get_geom_manager()
           mapl_geom => geom_mgr%get_mapl_geom(geometry_spec%geom_spec, _RC)
           this%geom = mapl_geom%get_geom()
        end if

        call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

        call recurse(this, phase_idx=GENERIC_INIT_ADVERTISE_GEOM, _RC)

        if (geometry_spec%kind == GEOMETRY_FROM_CHILD) then
           provider => this%children%at(geometry_spec%provider, _RC)
           provider_gc = provider%get_gridcomp()
           provider_meta => get_outer_meta(provider_gc, _RC)
           _ASSERT(allocated(provider_meta%geom), 'Specified child does not provide a geom.')
           this%geom = provider_meta%geom
        end if
      end associate

      _RETURN(ESMF_SUCCESS)
   contains
      
   end subroutine initialize_advertise_geom

   !----------
   ! The procedure initialize_realize_geom() is responsible for passing grid
   ! down to children.
   ! ---------
   recursive subroutine initialize_realize_geom(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom), pointer :: mapl_geom
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_GEOM'
      type(GeomManager), pointer :: geom_mgr

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call apply_to_children(this, set_child_geom, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_REALIZE_GEOM, _RC)

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

   end subroutine initialize_realize_geom

   recursive subroutine initialize_advertise(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      call self_advertise(this, _RC)
      call apply_to_children(this, add_subregistry, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_ADVERTISE, _RC)

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
         class(StateItemSpec), allocatable :: item_spec
         type(VirtualConnectionPt) :: virtual_pt
         integer :: i

         _ASSERT(var_spec%itemtype /= MAPL_STATEITEM_UNKNOWN, 'Invalid type id in variable spec <'//var_spec%short_name//'>.')

!#         item_spec = var_spec%make_ItemSpec(geom, vertical_geom, registry, _RC)
         allocate(item_spec, source=var_spec%make_ItemSpec(geom, vertical_geom, registry, rc=status)); _VERIFY(status)
         call item_spec%create(_RC)
         
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
      type(MultiState) :: outer_states, user_states

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)
      
      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)

      call recurse(this, phase_idx=GENERIC_INIT_POST_ADVERTISE, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_post_advertise


   recursive subroutine initialize_realize(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE'

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_REALIZE, _RC)
      call this%registry%allocate(_RC)
      
      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

   end subroutine initialize_realize

   ! This procedure is used to recursively invoke a given ESMF phase down
   ! the hierarchy.
   recursive subroutine recurse_(this, phase_idx, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
           call child%initialize(phase_idx=phase_idx, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine recurse_

   ! This procedure should not be invoked recursively - it is not for traversing the tree,
   ! but rather just to facilitate custom operations where a parent component must pass
   ! information to its children.
   subroutine apply_to_children_custom(this, oper, rc)
      class(OuterMetaComponent), intent(inout) :: this
      procedure(I_child_op) :: oper
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           child_outer_gc = child%get_gridcomp()
           child_meta => get_outer_meta(child_outer_gc, _RC)
           call oper(this, child_meta, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine apply_to_children_custom

   recursive subroutine initialize_user(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_USER'

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_USER, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_user

   subroutine run_custom(this, method_flag, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_METHOD_FLAG), intent(in) :: method_flag
      character(*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc
      
      integer :: status
      integer :: phase_idx
      type(StringVector), pointer :: phases
      logical :: found

      phases => this%get_phases(method_flag)
      phase_idx = get_phase_index(phases, phase_name, found=found)
      _RETURN_UNLESS(found)
      if (method_flag == ESMF_METHOD_INITIALIZE) then
         call this%user_gc_driver%initialize(phase_idx=phase_idx, _RC)
      else if (method_flag == ESMF_METHOD_RUN) then
         call this%user_gc_driver%run(phase_idx=phase_idx, _RC)
      else if (method_flag == ESMF_METHOD_FINALIZE) then
         call this%user_gc_driver%finalize(phase_idx=phase_idx, _RC)
      else
         _FAIL('Unknown ESMF method flag.')
      end if

      _RETURN(_SUCCESS)
   end subroutine run_custom

   recursive subroutine run_user(this, phase_name, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      character(len=*), optional, intent(in) :: phase_name
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC, i
      integer :: phase_idx
      type(StateExtension), pointer :: extension
      type(StringVector), pointer :: run_phases
      logical :: found
      integer :: phase

      type(ActualPtComponentDriverMap), pointer :: export_Couplers
      type(ActualPtComponentDriverMap), pointer :: import_Couplers
      type(ActualPtComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: drvr

      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name, found=found)
      _ASSERT(found, 'phase <'//phase_name//'> not found for gridcomp <'//this%get_name()//'>')
      
      import_couplers => this%registry%get_import_couplers()
      associate (e => import_couplers%ftn_end())
        iter = import_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           drvr => iter%second()
           call drvr%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
        end do
      end associate

      call this%user_gc_driver%run(phase_idx=phase, _RC)
   
      export_couplers => this%registry%get_export_couplers()
      associate (e => export_couplers%ftn_end())
        iter = export_couplers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           drvr => iter%second()
           call drvr%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
        end do
      end associate

      
      _RETURN(ESMF_SUCCESS)
   end subroutine run_user

   recursive subroutine run_clock_advance(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(StringVector), pointer :: run_phases
      logical :: found
      integer :: phase

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
           call child%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
        end do
      end associate

      call this%user_gc_driver%clock_advance(_RC)

      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name='GENERIC::RUN_CLOCK_ADVANCE', found=found)
      if (found) then
         call this%user_gc_driver%run(phase_idx=phase, _RC)
      end if


      _RETURN(ESMF_SUCCESS)
   end subroutine run_clock_advance

   recursive subroutine finalize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver), pointer :: child
      type(GriddedComponentDriverMapIterator) :: iter
      integer :: status, userRC
      character(*), parameter :: PHASE_NAME = 'GENERIC::FINALIZE_USER'
      type(StringVector), pointer :: finalize_phases
      logical :: found

      finalize_phases => this%user_phases_map%at(ESMF_METHOD_FINALIZE, _RC)
      ! User gridcomp may not have any given phase; not an error condition if not found.
      associate (phase => get_phase_index(finalize_phases, phase_name=phase_name, found=found))
        _RETURN_UNLESS(found)

        ! TODO:  Should user finalize be after children finalize?

        ! TODO:  Should there be a phase option here?  Probably not
        ! right as is when things get more complicated.

        call this%run_custom(ESMF_METHOD_FINALIZE, PHASE_NAME, _RC)

        associate(b => this%children%begin(), e => this%children%end())
          iter = b
          do while (iter /= e)
             child => iter%second()
             call child%finalize(phase_idx=GENERIC_FINALIZE_USER, _RC)
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

      print *, "OuterMetaComp: read_restart - not implemented yet"

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

      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      character(:), allocatable :: child_name
      type(ESMF_GridComp) :: child_outer_gc
      type(OuterMetaComponent), pointer :: child_outer_meta
      type(MultiState) :: child_states
      type(ESMF_State) :: child_internal_state, child_import_state
      type(ESMF_Geom) :: child_geom
      type(ESMF_Clock) :: child_clock
      type(Restart) :: rstrt
      integer :: status

      associate(e => this%children%end())
        iter = this%children%begin()
        do while (iter /= e)
           child_name = iter%first()
           if (child_name /= "HIST") then
              print *, "writing restart: ", trim(child_name)
              child => iter%second()
              child_clock = child%get_clock()
              child_outer_gc = child%get_gridcomp()
              child_outer_meta => get_outer_meta(child_outer_gc, _RC)
              child_geom = child_outer_meta%get_geom()
              rstrt = Restart(child_name, child_geom, child_clock, _RC)
              child_internal_state = child_outer_meta%get_internal_state()
              call rstrt%write("internal", child_internal_state, _RC)
              child_states = child%get_states()
              call child_states%get_state(child_import_state, "import", _RC)
              call rstrt%write("import", child_import_state, _RC)
              call child%write_restart(_RC)
           end if
           call iter%next()
        end do
      end associate

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

      type(MultiState) :: user_states

      user_states = this%user_gc_driver%get_states()
      internal_state = user_states%internalState

   end function get_internal_state


   function get_lgr(this) result(lgr)
      class(Logger), pointer :: lgr
      class(OuterMetaComponent), target, intent(in) :: this

      lgr => this%lgr

   end function get_lgr

   function get_user_gc_driver(this) result(user_gc_driver)
      type(GriddedComponentDriver), pointer :: user_gc_driver
      class(OuterMetaComponent), target, intent(in) :: this
      user_gc_driver => this%user_gc_driver
   end function get_user_gc_driver


   
   ! ----------
   ! This is a "magic" connection that attempts to connect each
   ! unsatisfied import in dst_comp, with a corresponding export in
   ! the src_comp.  The corresponding export must have the same short
   ! name, or if the import is a wildcard connection point, the all
   ! exports with names that match the regexp of the wildcard are
   ! connected.
   ! ----------
   subroutine connect_all(this, src_comp, dst_comp, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: dst_comp
      integer, optional, intent(out) :: rc

      integer :: status
      class(Connection), allocatable :: conn

      conn = MatchConnection( &
           ConnectionPt(src_comp, VirtualConnectionPt(state_intent='export', short_name='^.*$')), &
           ConnectionPt(dst_comp, VirtualConnectionPt(state_intent='import', short_name='^.*$'))  &
           )
      call this%component_spec%add_connection(conn)

      _RETURN(_SUCCESS)
   end subroutine connect_all

   subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status
      character(:), allocatable :: phase_name_
      type(ESMF_GridComp) :: user_gridcomp
      logical :: found

      if (present(phase_name)) then
         phase_name_ = phase_name
      else
         phase_name_ = get_default_phase_name(method_flag)
      end if
      call add_phase(this%user_phases_map, method_flag=method_flag, phase_name=phase_name_, _RC)

      associate (phase_idx => get_phase_index(this%user_phases_map%of(method_flag), phase_name=phase_name_, found=found))
        _ASSERT(found, "run phase: <"//phase_name_//"> not found.")
        user_gridcomp = this%user_gc_driver%get_gridcomp()
        call ESMF_GridCompSetEntryPoint(user_gridcomp, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_entry_point

end module mapl3g_OuterMetaComponent
