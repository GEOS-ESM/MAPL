#include "MAPL_ErrLog.h"

module mapl3g_OuterMetaComponent
   use :: mapl3g_UserSetServices,   only: AbstractUserSetServices
   use :: mapl3g_ChildComponent
   use :: mapl3g_CouplerComponentVector
   use :: mapl3g_InnerMetaComponent
   use :: mapl3g_MethodPhasesMap
   use :: mapl3g_ChildComponentMap, only: ChildComponentMap
   use :: mapl3g_ChildComponentMap, only: ChildComponentMapIterator
   use :: mapl3g_ChildComponentMap, only: operator(/=)
   use :: mapl_ErrorHandling
   use :: gFTL2_StringVector
   use :: mapl_keywordEnforcer, only: KeywordEnforcer
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_Config
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_State
   use :: esmf, only: ESMF_SUCCESS
   use :: yaFyaml, only: Configuration
   use :: pflogger, only: logging, Logger
   implicit none
   private

   public :: OuterMetaComponent
   public :: get_outer_meta
   public :: attach_outer_meta
   public :: free_outer_meta

   type :: GenericConfig
      type(ESMF_Config), allocatable :: esmf_cfg
      type(Configuration), allocatable :: yaml_config
   end type GenericConfig


   type :: OuterMetaComponent
      private
      character(len=:), allocatable               :: name
      type(ESMF_GridComp)                         :: self_gc
      type(ESMF_GridComp)                         :: user_gc
      type(GenericConfig)                         :: config
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(MethodPhasesMap)                       :: phases_map
      type(OuterMetaComponent), pointer           :: parent_private_state
!!$      type(ComponentSpec)                         :: component_spec

      type(ChildComponentMap)                     :: children
      type(InnerMetaComponent), allocatable       :: inner_meta
      type(CouplerComponentVector)                :: couplers

      class(Logger), pointer :: lgr  ! "MAPL.Generic"

   contains

      procedure :: set_esmf_config
      procedure :: set_yaml_config
      generic   :: set_config => set_esmf_config, set_yaml_config
      procedure :: get_phases
!!$      procedure :: get_gridcomp
!!$      procedure :: get_user_gridcomp
      procedure :: set_user_setservices

      ! Generic methods
      procedure :: setservices
      procedure :: initialize
      procedure :: run
      procedure :: finalize

      procedure, private :: add_child_by_name
      procedure, private :: get_child_by_name
      procedure, private :: run_child_by_name
      procedure, private :: run_children_

      generic :: add_child => add_child_by_name
      generic :: get_child => get_child_by_name
      generic :: run_child => run_child_by_name
      generic :: run_children => run_children_

   end type OuterMetaComponent

   type OuterMetaWrapper
      type(OuterMetaComponent), pointer :: outer_meta
   end type OuterMetaWrapper

   !Constructor
   interface OuterMetaComponent
      module procedure new_outer_meta
   end interface OuterMetaComponent

   character(len=*), parameter :: OUTER_META_PRIVATE_STATE = "OuterMetaComponent Private State"


   ! Submodule interfaces
   interface
      module subroutine SetServices(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, intent(out) ::rc
      end subroutine
   end interface


contains


   type(OuterMetaComponent) function new_outer_meta(gridcomp) result(outer_meta)
      type(ESMF_GridComp), intent(in) :: gridcomp

      outer_meta%self_gc = gridcomp
      call initialize_phases_map(outer_meta%phases_map)

   end function new_outer_meta


   subroutine add_child_by_name(this, child_name, config, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status


      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


   ! Deep copy of shallow ESMF objects - be careful using result
   ! TODO: Maybe this should return a POINTER
   type(ChildComponent) function get_child_by_name(this, child_name, rc) result(child_component)
      class(OuterMetaComponent), intent(in) :: this
      character(len=*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status

      child_component = this%children%at(child_name, _RC)

      _RETURN(_SUCCESS)
   end function get_child_by_name

   subroutine run_child_by_name(this, child_name, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(ChildComponent) :: child
      integer:: phase_idx

      child = this%get_child(child_name, _RC)
      call child%run(clock, phase_name=phase_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_child_by_name

   subroutine run_children_(this, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(ChildComponent), pointer :: child
      type(ChildComponentMapIterator) :: iter

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           call child%run(clock, phase_name=phase_name, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine run_children_


   function get_outer_meta(gridcomp, rc) result(outer_meta)
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper

      outer_meta => null()

      call ESMF_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not found for this gridcomp.")
      outer_meta => wrapper%outer_meta


      _RETURN(_SUCCESS)
   end function get_outer_meta

   subroutine attach_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper
      type(OuterMetaComponent), pointer :: outer_meta

      allocate(wrapper%outer_meta) ! potential memory leak: use free_outer_meta()
      call ESMF_UserCompSetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent already created for this gridcomp?")

      outer_meta => wrapper%outer_meta
      outer_meta = OuterMetaComponent(gridcomp)
      outer_meta%lgr => logging%get_logger('MAPL.GENERIC')

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

   subroutine free_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper

      call ESMF_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not created for this gridcomp")
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
   ! clearer use case.  Might only be needd from within inner meta.
!!$   type(ESMF_GridComp) function get_gridcomp(this) result(gridcomp)
!!$      class(OuterMetaComponent), intent(in) :: this
!!$
!!$      gridcomp = this%self_gc
!!$      
!!$   end function get_gridcomp
!!$
!!$   type(ESMF_GridComp) function get_user_gridcomp(this) result(gridcomp)
!!$      class(OuterMetaComponent), intent(in) :: this
!!$
!!$      gridcomp = this%user_gc
!!$      
!!$   end function get_user_gridcomp

   subroutine set_esmf_config(this, config)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Config), intent(in) :: config

      this%config%esmf_cfg = config

   end subroutine set_esmf_config

   subroutine set_yaml_config(this, config)
      class(OuterMetaComponent), intent(inout) :: this
      type(Configuration), intent(in) :: config

      this%config%yaml_config = config

   end subroutine set_yaml_config

   subroutine set_user_setservices(this, user_setservices)
      class(OuterMetaComponent), intent(inout) :: this
      class(AbstractUserSetServices), intent(in) :: user_setservices
      this%user_setservices = user_setservices
   end subroutine set_user_setservices


   subroutine initialize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, unusable, phase_name, rc)
      use :: esmf, only: ESMF_METHOD_RUN
      use :: esmf, only: ESMF_GridCompRun
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      integer :: phase_idx


      if (present(phase_name)) then
         _ASSERT(this%phases_map%count(ESMF_METHOD_RUN) > 0, "No phases registered for ESMF_METHOD_RUN.")
         phase_idx = get_phase_index(this%phases_map%of(ESMF_METHOD_RUN), phase_name, _RC)
      else
         phase_idx = 1
      end if

      call ESMF_GridCompRun(this%self_gc, importState=importState, exportState=exportState, &
           clock=clock, phase=phase_idx, userRC=userRC, _RC)
      _VERIFY(userRC)


      _RETURN(ESMF_SUCCESS)
   end subroutine run

   subroutine finalize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      _RETURN(ESMF_SUCCESS)
   end subroutine finalize

   subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      _RETURN(ESMF_SUCCESS)
   end subroutine read_restart


   subroutine write_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      _RETURN(ESMF_SUCCESS)
   end subroutine write_restart


end module mapl3g_OuterMetaComponent
