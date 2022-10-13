#include "MAPL_Generic.h"

module mapl3g_OuterMetaComponent
   use mapl3g_UserSetServices,   only: AbstractUserSetServices
   use mapl3g_GenericConfig
   use mapl3g_ComponentSpec
   use mapl3g_ChildComponent
   use mapl3g_Validation, only: is_valid_name
!!$   use mapl3g_CouplerComponentVector
   use mapl3g_InnerMetaComponent
   use mapl3g_MethodPhasesMap
   use mapl3g_ChildComponentMap, only: ChildComponentMap
   use mapl3g_ChildComponentMap, only: ChildComponentMapIterator
   use mapl3g_ChildComponentMap, only: operator(/=)
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPoint
   use mapl3g_ConnectionSpec
   use mapl3g_ESMF_Interfaces, only: I_Run
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
      type(ESMF_Grid), allocatable                :: primary_grid
      type(GenericConfig)                         :: config
      type(ChildComponentMap)                     :: children
      logical                                     :: is_root_ = .false.

      type(ESMF_GridComp)                         :: user_gridcomp
      type(MethodPhasesMap)                       :: phases_map
      type(InnerMetaComponent), allocatable       :: inner_meta

      class(Logger), pointer :: lgr  ! "MAPL.Generic" // name

      type(ComponentSpec)                         :: component_spec
      type(OuterMetaComponent), pointer           :: parent_private_state


   contains
      procedure :: set_esmf_config
      procedure :: set_yaml_config
      generic   :: set_config => set_esmf_config, set_yaml_config

      procedure :: get_phases
!!$      procedure :: get_gridcomp
!!$      procedure :: get_user_gridcomp
      procedure :: set_user_setServices
      procedure :: set_entry_point

      ! Generic methods
      procedure :: setServices

      procedure :: initialize ! main/any phase
      procedure :: initialize_user
      procedure :: initialize_grid
      procedure :: initialize_all

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

      ! Specs
      procedure :: add_state_item_spec
      procedure :: add_connection

      procedure :: traverse

      procedure :: set_grid
      procedure :: get_name
      procedure :: get_gridcomp
      procedure :: is_root

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

      recursive module subroutine SetServices(this, rc)
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


contains


   ! Keep the constructor simple
   type(OuterMetaComponent) function new_outer_meta(gridcomp, set_services, config) result(outer_meta)
      type(ESMF_GridComp), intent(in) :: gridcomp
      class(AbstractUserSetServices), intent(in) :: set_services
      type(GenericConfig), intent(in) :: config

      outer_meta%self_gridcomp = gridcomp
      outer_meta%user_setservices = set_services
      outer_meta%config = config
      !TODO: this may be able to move outside of constructor
      call initialize_phases_map(outer_meta%phases_map)

   end function new_outer_meta

   subroutine initialize_meta(this, gridcomp)
      class(OuterMetaComponent), intent(out) :: this
      type(ESMF_GridComp), intent(inout) :: gridcomp

      character(ESMF_MAXSTR) :: name

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

      child_component = this%children%at(child_name, _RC)

      _RETURN(_SUCCESS)
   end function get_child_by_name

   subroutine run_child_by_name(this, child_name, clock, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      type(ESMF_Clock), intent(inout) :: clock
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
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
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
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

      call ESMF_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
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
!!$   type(ESMF_GridComp) function get_user_gridcomp(this) result(gridcomp)
!!$      class(OuterMetaComponent), intent(in) :: this
!!$
!!$      gridcomp = this%user_gridcomp
!!$      
!!$   end function get_user_gridcomp

   subroutine set_esmf_config(this, config)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Config), intent(in) :: config

      this%config%esmf_cfg = config

   end subroutine set_esmf_config

   subroutine set_yaml_config(this, config)
      class(OuterMetaComponent), intent(inout) :: this
      class(YAML_Node), intent(in) :: config

      this%config%yaml_cfg = config

   end subroutine set_yaml_config

   subroutine set_user_setservices(this, user_setservices)
      class(OuterMetaComponent), intent(inout) :: this
      class(AbstractUserSetServices), intent(in) :: user_setservices
      this%user_setServices = user_setservices
   end subroutine set_user_setservices


   ! ESMF initialize methods

   ! initialize_grid() is responsible for passing grid down to
   ! children.  User component can insert a different grid using
   ! GENERIC_INIT_GRID phase in their component.
   recursive subroutine initialize_grid(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(ChildComponent), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ChildComponentMapIterator) :: iter

      associate (phase => get_phase_index(this%phases_map%of(ESMF_METHOD_INITIALIZE), phase_name='GENERIC_INIT_GRID', rc=status))
        if (status == _SUCCESS) then
           call ESMF_GridCompInitialize(this%user_gridcomp, importState=importState, exportState=exportState, &
                clock=clock, phase=phase, userRC=userRC, _RC)
           _VERIFY(userRC)
        end if
      end associate

      if (allocated(this%primary_grid)) then
         associate(b => this%children%begin(), e => this%children%end())
           iter = b
           do while (iter /= e)
              child => iter%second()
              child_meta => get_outer_meta(child%gridcomp)
              _ASSERT(.not. allocated(child_meta%primary_grid), 'premature definition of grid in gridcomp <'//iter%first()//'>')
              call child_meta%set_grid(this%primary_grid)
              call child%initialize(clock, phase_name='GENERIC_INIT_GRID', _RC)
              call iter%next()
           end do
         end associate
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize_grid

   recursive subroutine initialize_user(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(ChildComponent), pointer :: child
      type(ChildComponentMapIterator) :: iter

      associate (phase => get_phase_index(this%phases_map%of(ESMF_METHOD_INITIALIZE), phase_name='DEFAULT', rc=status))
        if (status == _SUCCESS) then
           call ESMF_GridCompInitialize(this%user_gridcomp, importState=importState, exportState=exportState, &
                clock=clock, userRC=userRC, phase=phase, _RC)
           _VERIFY(userRC)
        end if
      end associate
      
      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           call child%initialize(clock, phase_name='DEFAULT', _RC)
           call iter%next()
        end do
      end associate

      _RETURN(ESMF_SUCCESS)
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
      type(ChildComponent), pointer :: child

      if (present(phase_name)) then
         _ASSERT(this%phases_map%count(ESMF_METHOD_RUN) > 0, "No phases registered for ESMF_METHOD_RUN.")
         select case (phase_name)
         case ('GENERIC_INIT_GRID')
            call this%initialize_grid(importState, exportState, clock, _RC)
         case ('DEFAULT')
            call this%initialize_user(importState, exportState, clock, _RC)
         case default
            _FAIL('unsupported initialize phase: '// phase_name)
         end select
      else
         call this%initialize_user(importState, exportState, clock, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize

   recursive subroutine initialize_all(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      type(ESMF_State), optional :: importState
      type(ESMF_State), optional :: exportState
      type(ESMF_Clock), optional :: clock
      integer, optional, intent(out) :: rc

      integer :: status, userRC
      type(ChildComponent), pointer :: child

      call this%initialize_grid(importState, exportState, clock, _RC)
      call this%initialize_user(importState, exportState, clock, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine initialize_all

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

      if (present(phase_name)) then
         _ASSERT(this%phases_map%count(ESMF_METHOD_RUN) > 0, "No phases registered for ESMF_METHOD_RUN.")
         phase_idx = get_phase_index(this%phases_map%of(ESMF_METHOD_RUN), phase_name=phase_name, _RC)
      else
         phase_idx = 1
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
           call child%finalize(clock, _RC)
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

      integer :: status, userRC

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

      integer :: status, userRC

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


   subroutine add_state_item_spec(this, state_intent, short_name, spec, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(AbstractStateItemSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      _ASSERT(count(state_intent == ['import  ' ,'export  ', 'internal']) == 1, 'invalid state intent')
      _ASSERT(is_valid_name(short_name), 'Short name <' // short_name //'> does not conform to GEOS standards.')

      associate(comp_name => this%get_name())
      
        associate (conn_pt => ConnectionPoint(comp_name, state_intent, short_name))
          call this%component_spec%add_state_item_spec(conn_pt, spec)
        end associate

      end associate

      _RETURN(_SUCCESS)
   end subroutine add_state_item_spec

   subroutine add_connection(this, connection, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ConnectionSpec), intent(in) :: connection
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(is_valid(connection),'unsupported connection type')
      call this%component_spec%add_connection(connection)
      _RETURN(_SUCCESS)
   end subroutine add_connection

   pure logical function is_root(this)
      class(OuterMetaComponent), intent(in) :: this
      is_root = this%is_root_
   end function is_root

   pure subroutine set_grid(this, primary_grid)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Grid), intent(in) :: primary_grid

      this%primary_grid = primary_grid
   end subroutine set_grid

end module mapl3g_OuterMetaComponent
