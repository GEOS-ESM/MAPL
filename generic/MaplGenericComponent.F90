#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_MaplGenericComponent
   use ESMF
   use mapl_AbstractFrameworkComponent
   use mapl_BaseFrameworkComponent
   use mapl_keywordenforcermod
   use mapl_AbstractComponent
   use mapl_CompositeComponent
   use mapl_ConcreteComposite
   use mapl_MaplComponent
   use mapl_ErrorHandlingMod
   use pFlogger
   use mapl_OpenMP_Support
   use mapl_MaplGrid
   use mapl_RunEntryPoint
   use mapl_EntryPointVector
   implicit none
   private

   public :: MaplGenericComponent
   public :: get_grid

   type SubComponent
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: internal_state
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(MaplGrid) :: grid
   end type SubComponent

   abstract interface
      subroutine UserMethod(gc, import, export, clock, rc)
         use ESMF
         implicit none
         type (ESMF_GridComp), intent(inout) :: gc     ! Gridded component
         type (ESMF_State),    intent(inout) :: import ! Import state
         type (ESMF_State),    intent(inout) :: export ! Export state
         type (ESMF_Clock),    intent(inout) :: clock  ! The clock
         integer, optional,    intent(  out) :: rc     ! Error code:
      end subroutine UserMethod
   end interface

   type, extends(BaseFrameworkComponent) :: MaplGenericComponent
!!$      private
      type(ESMF_GridComp) :: gridcomp
      type(SubComponent), allocatable :: subcomponents(:)
      type(EntryPointVector) :: run_entry_points
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_State) :: internal_state

      logical :: threading_active = .FALSE.
      logical :: use_threads = .FALSE.


   contains
      procedure :: initialize => stub
      procedure :: run => stub
      procedure :: finalize => stub

      procedure :: initialize_child => stub_child
      procedure :: run_child => stub_child
      procedure :: finalize_child => stub_child

      procedure :: add_child_component

      procedure :: activate_threading
      procedure :: deactivate_threading

      procedure :: create_subobjects

      ! accessors
      procedure :: get_logger
      procedure :: set_logger
      procedure :: set_use_threads
      procedure :: get_use_threads
      procedure :: is_threading_active
      procedure :: get_internal_state
      procedure :: get_import_state
      procedure :: get_export_state
      procedure :: get_grid
      procedure :: get_gridcomp
   end type MaplGenericComponent

contains



   subroutine stub(this, clock, phase, unusable, rc)
      class(MaplGenericComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractComponent), pointer :: user_component

      __UNUSED_DUMMY(unusable)

      user_component => this%get_component()
      call user_component%run(this%import_state, this%export_state, clock, phase, __RC)

      __RETURN(__SUCCESS)
   end subroutine stub

   subroutine stub_child(this, name, clock, phase, unusable, rc)
      class(MaplGenericComponent), intent(inout) :: this
      character(*), intent(in) :: name
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class(AbstractFrameworkComponent), pointer :: child
      integer :: status

      __UNUSED_DUMMY(unusable)

      child => this%get_child(name)
      call child%run(clock, phase, __RC)

      __RETURN(__SUCCESS)
   end subroutine stub_child


   function add_child_component(this, name, user_component) result(child)
      class(AbstractFrameworkComponent), pointer :: child
      class(MaplGenericComponent), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(AbstractComponent), intent(in) :: user_component

      type(MaplGenericComponent) :: tmp

      child => this%add_child(name, tmp)
      call child%set_component(user_component)

   end function add_child_component


   subroutine generic_entry_point(gridcomp, import, export, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: import
      type(ESMF_State), intent(inout) :: export
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR) :: name
      integer :: phase
      type(ESMF_Method_Flag) :: method
      integer :: status

      class(MaplComponent), pointer :: component
      type :: ComponentWrapper
         class(MaplComponent), pointer :: component
      end type ComponentWrapper

      type(ComponentWrapper) :: wrapper
      character(:), pointer :: phase_name

      call ESMF_UserCompGetInternalState(gridcomp, "MaplComponent", wrapper, status)
      __VERIFY(status)
      component => wrapper%component

      call ESMF_GridCompGet( gridcomp, name=name, currentPhase=phase, currentMethod=method, __RC)

      if (method == ESMF_METHOD_INITIALIZE) then

!!$         phase_name => component%init_phase_map%at(phase)
!!$         __ASSERT(associated(phase_name),'no such phase')
         call component%initialize(import, export, clock, phase_name, __RC)

      else if (method == ESMF_METHOD_RUN) then

!!$         phase_name = component%run_phase_map%at(phase)
!!$         __ASSERT(associated(phase_name),'no such phase')
         call component%run(import, export, clock, phase_name, __RC)

      else if (method == ESMF_METHOD_FINALIZE) then

!!$         phase_name = component%finalize_phase_map%at(phase)
!!$         __ASSERT(associated(phase_name),'no such phase')
         call component%finalize(import, export, clock, phase_name, __RC)

      else
         __FAIL('unknown value for ESMF_METHOD_FLAG')
      end if

      __RETURN(__SUCCESS)
   end subroutine generic_entry_point

   function get_logger(this) result(lgr)
      class(Logger), pointer :: lgr
      class(MaplGenericComponent), intent(in) :: this
      class(AbstractComponent), pointer :: component

      component => this%get_component()
      lgr => component%get_logger()

   end function get_logger

   subroutine set_logger(this, lgr)
      class(MaplGenericComponent), intent(inout) :: this
      class(Logger), target :: lgr
      class(AbstractComponent), pointer :: component

      component => this%get_component()
      call component%set_logger(lgr)

   end subroutine set_logger

   subroutine set_use_threads(this, use_threads)
      class(MaplGenericComponent), intent(inout) :: this
      logical, intent(in) :: use_threads

      this%use_threads = use_threads

   end subroutine set_use_threads

   function get_use_threads(this) result(use_threads)
      class(MaplGenericComponent), intent(in) :: this
      logical :: use_threads

      use_threads = this%use_threads

   end function get_use_threads

   function is_threading_active(this) result(threading_active)
     class(MaplGenericComponent), intent(in) :: this
     logical :: threading_active

     threading_active = this%threading_active
   end function is_threading_active

   function get_internal_state(this) result(internal_state)
      class(MaplGenericComponent), target, intent(in) :: this
      type(ESMF_State), pointer :: internal_state
      integer :: thread

      if (.NOT. this%is_threading_active()) then
        internal_state => this%internal_state
     else
        thread = get_current_thread()
        internal_state => this%subcomponents(thread+1)%internal_state
     end if
   end function get_internal_state

   function get_import_state(this) result(import_state)
     class(MaplGenericComponent), target, intent(in) :: this
     type(ESMF_State), pointer :: import_state
     integer :: thread

     if (.NOT. this%is_threading_active()) then
        import_state => this%import_state
     else
        thread = get_current_thread()
        import_state => this%subcomponents(thread+1)%import_state
     end if
   end function get_import_state

   function get_export_state(this) result(export_state)
     class(MaplGenericComponent), target, intent(in) :: this
     type(ESMF_State), pointer :: export_state
     integer :: thread

     if (.NOT. this%is_threading_active()) then
        export_state => this%export_state
     else
        thread = get_current_thread()
        export_state => this%subcomponents(thread+1)%export_state
     end if
   end function get_export_state

   function get_grid(this) result(grid)
     class(MaplGenericComponent), target, intent(in) :: this
     type(MaplGrid), pointer :: grid
     integer :: thread

     if (.NOT. this%is_threading_active()) then
        grid => this%grid
     else
        thread = get_current_thread()
        grid => this%subcomponents(thread+1)%grid ! subgrids is of type ESMF_Grid because of the return type of make_subgrids
     end if
   end function get_grid


   recursive subroutine activate_threading(this, num_threads, unusable, rc)
     class(MaplGenericComponent), intent(inout) :: this
     integer, intent(in) :: num_threads
     class(KeywordEnforcer), optional :: unusable
     integer, optional, intent(out) :: rc
     class(AbstractFrameworkComponent), pointer :: child
     integer :: num_children, i, status

     this%threading_active = .TRUE.
     num_children = this%get_num_children()

     if (.NOT. allocated(this%subcomponents)) then
        call this%create_subobjects(num_threads, __RC)
     end if

     do i = 1, num_children
        child => this%get_child(i)
        select type (child)
        class is (MaplGenericComponent)
           call child%activate_threading(num_threads)
        class default
           __FAIL('illegal type for child')
        end select
     end do
     __RETURN(0)
     __UNUSED_DUMMY(unusable)
   end subroutine activate_threading

   subroutine create_subobjects(this, num_threads, unusable, rc)
     class(MaplGenericComponent), intent(inout) :: this
     integer, intent(in) :: num_threads
     class(KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
     integer :: i, status
     type(ESMF_Grid), allocatable :: subgrids(:)

     allocate(this%subcomponents(num_threads))

     this%subcomponents(:)%import_state = make_substates(this%import_state, num_threads, __RC)
     this%subcomponents(:)%export_state = make_substates(this%export_state, num_threads, __RC)
     this%subcomponents(:)%internal_state = make_substates(this%internal_state, num_threads, __RC)

     subgrids = make_subgrids(this%grid%ESMFGrid, num_threads, __RC) ! make_subgrids requires grid of type ESMF_Grid
     do i = 1, size(subgrids)
        call this%subcomponents(i)%grid%set(subgrids(i), __RC)
     end do
     deallocate(subgrids)

     this%subcomponents(:)%gridcomp = make_subgridcomps(this%gridcomp, this%run_entry_points, num_threads, __RC)

     __RETURN(0)
     __UNUSED_DUMMY(unusable)
   end subroutine create_subobjects

   recursive subroutine deactivate_threading(this, unusable, rc)
     class(MaplGenericComponent), intent(inout) :: this
     class(KeywordEnforcer), optional :: unusable
     integer, optional, intent(out) :: rc
     class(AbstractFrameworkComponent), pointer :: child
     integer :: num_children, i

     num_children = this%get_num_children()
     do i = 1, num_children
        child => this%get_child(i)
        select type (child)
        class is (MaplGenericComponent)
           call child%deactivate_threading()
        class default
           __FAIL('illegal type for child')
        end select
     end do

     this%threading_active = .FALSE.
     __RETURN(0)
     __UNUSED_DUMMY(unusable)
   end subroutine deactivate_threading



   function get_gridcomp(this) result(gridcomp)
      class(MaplGenericComponent), target, intent(in) :: this
      type(ESMF_GridComp), pointer :: gridcomp

      integer :: thread

      if (.NOT. this%is_threading_active()) then
         gridcomp => this%gridcomp
     else
        thread = get_current_thread()
        gridcomp => this%subcomponents(thread+1)%gridcomp
     end if

  end function get_gridcomp

end module mapl_MaplGenericComponent
