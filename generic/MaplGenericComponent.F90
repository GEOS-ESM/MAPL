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
   !$ use omp_lib
   implicit none
   private

   public :: MaplGenericComponent
   public :: get_grid

   procedure(), pointer :: user_method1 => null()
   procedure(), pointer :: user_method2 => null()

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

   type :: runEntryPoint
      procedure(), pointer, nopass :: run_entry_point => null()
   end type

   type, extends(BaseFrameworkComponent) :: MaplGenericComponent
!!$      private
      type(ESMF_GridComp) :: gridcomp
      type(SubComponent), allocatable :: subcomponents(:)
      type(runEntryPoint) :: run_entry_points(2)
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_State) :: internal_state
      !type(MaplGrid) :: grid

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

      procedure :: make_subgridcomps
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

      _UNUSED_DUMMY(unusable)

      user_component => this%get_component()
      call user_component%run(this%import_state, this%export_state, clock, phase, __RC__)

      _RETURN(_SUCCESS)
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

      _UNUSED_DUMMY(unusable)

      child => this%get_child(name)
      call child%run(clock, phase, __RC__)

      _RETURN(_SUCCESS)
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
      _VERIFY(status)
      component => wrapper%component

      call ESMF_GridCompGet( gridcomp, name=name, currentPhase=phase, currentMethod=method, __RC__)

      if (method == ESMF_METHOD_INITIALIZE) then

!!$         phase_name => component%init_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%initialize(import, export, clock, phase_name, __RC__)

      else if (method == ESMF_METHOD_RUN) then

!!$         phase_name = component%run_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%run(import, export, clock, phase_name, __RC__)

      else if (method == ESMF_METHOD_FINALIZE) then
         
!!$         phase_name = component%finalize_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%finalize(import, export, clock, phase_name, __RC__)

      else
         _FAIL('unknown value for ESMF_METHOD_FLAG')
      end if
         
      _RETURN(_SUCCESS)
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
        thread = 0
        !$ thread = omp_get_thread_num()
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
        thread = 0
        !$ thread = omp_get_thread_num()
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
        thread = 0
        !$ thread = omp_get_thread_num()
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
        thread = 0
        !$ thread = omp_get_thread_num()
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
        call this%create_subobjects(num_threads, __RC__)
     end if

     do i = 1, num_children
        child => this%get_child(i) 
        select type (child)
        class is (MaplGenericComponent)
           call child%activate_threading(num_threads)
        class default
           _FAIL('illegal type for child')
        end select
     end do
     _RETURN(0)
   end subroutine activate_threading

   subroutine create_subobjects(this, num_threads, unusable, rc)
     class(MaplGenericComponent), intent(inout) :: this
     integer, intent(in) :: num_threads
     class(KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc
     integer :: i, status
     type(ESMF_Grid), allocatable :: subgrids(:)

     allocate(this%subcomponents(num_threads))
     
     this%subcomponents(:)%import_state = make_substates(this%import_state, num_threads, __RC__)
     this%subcomponents(:)%export_state = make_substates(this%export_state, num_threads, __RC__)
     this%subcomponents(:)%internal_state = make_substates(this%internal_state, num_threads, __RC__)

     subgrids = make_subgrids(this%grid%ESMFGrid, num_threads, __RC__) ! make_subgrids requires grid of type ESMF_Grid
     do i = 1, size(subgrids)
        call this%subcomponents(i)%grid%set(subgrids(i), __RC__)
     end do
     deallocate(subgrids)

     this%subcomponents(:)%gridcomp = this%make_subgridcomps(num_threads, __RC__)

     _RETURN(0)
   end subroutine create_subobjects

   subroutine deactivate_threading(this, unusable, rc)
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
           _FAIL('illegal type for child')
        end select
     end do
     
     this%threading_active = .FALSE.
     _RETURN(0)
   end subroutine deactivate_threading



   function get_gridcomp(this) result(gridcomp)
      class(MaplGenericComponent), target, intent(in) :: this
      type(ESMF_GridComp), pointer :: gridcomp

      integer :: thread 

      if (.NOT. this%is_threading_active()) then
         gridcomp => this%gridcomp
     else
        thread = 0
        !$ thread = omp_get_thread_num()
        gridcomp => this%subcomponents(thread+1)%gridcomp
     end if

  end function get_gridcomp

  function make_subgridcomps(this, num_grids, unusable, rc) result(subgridcomps)
      type(ESMF_GridComp), allocatable :: subgridcomps(:)
      class(MaplGenericComponent), intent(inout) :: this
      integer, intent(in) :: num_grids
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, user_status
      type(ESMF_VM) :: vm
      integer :: myPet, i, ilabel
      logical :: has_private_state

      type MAPL_GenericWrap
         type(ESMF_Clock), pointer :: dummy
      end type MAPL_GenericWrap

      type (MAPL_GenericWrap) :: wrap !, wrap_private
      character(len=ESMF_MAXSTR) :: comp_name
      character(len=:), allocatable :: labels(:)

      allocate(subgridcomps(num_grids))

      call ESMF_VMGetCurrent(vm, __RC__)
      call ESMF_VMGet(vm, localPET=myPET, __RC__)

      call ESMF_GridCompGet(this%gridcomp, name=comp_name, __RC__)
      call ESMF_InternalStateGet(this%gridcomp, labelList=labels, __RC__)
      if(myPET==0) print*,__FILE__,__LINE__, 'internal states labels : <',trim(comp_name), (trim(labels(i)),i=1,size(labels)), '>'
      print*,__FILE__,__LINE__, 'splitting component: <',trim(comp_name),'>'
      do i = 1, num_grids
        associate (gc => subgridcomps(i) )
          gc = ESMF_GridCompCreate(name=trim(comp_name), petlist=[myPet], &
               & contextflag=ESMF_CONTEXT_OWN_VM, __RC__)
             if(associated(this%run_entry_points(1)%run_entry_point)) &
                user_method1 => this%run_entry_points(1)%run_entry_point
             if(associated(this%run_entry_points(2)%run_entry_point)) &
                user_method2 => this%run_entry_points(2)%run_entry_point
          !do phase = 1,2
          !   print *, __FILE__, __LINE__, 'phase =', phase
          !   if(associated(this%run_entry_points(phase)%run_entry_point)) then
          !      user_method => this%run_entry_points(phase)%run_entry_point
             
                !call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, phase=phase, userroutine=user_method, __RC__)
          !    end if
          !end do
          call ESMF_GridCompSetServices(gc, set_services, userrc=user_status, __RC__)
          _VERIFY(user_status)
          !_VERIFY(status)
        end associate
      end do

      do ilabel = 1, size(labels)
         call ESMF_UserCompGetInternalState(this%gridcomp, trim(labels(ilabel)), wrap, status)
         has_private_state = (status == ESMF_SUCCESS)
         do i = 1, num_grids
            associate (gc => subgridcomps(i) )
              if (has_private_state) then
                 call ESMF_UserCompSetInternalState(gc, trim(labels(ilabel)), wrap, status)
                 _VERIFY(status)
              end if
            end associate
         end do
      end do

      _RETURN(ESMF_SUCCESS)
   end function make_subgridcomps

   subroutine set_services(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out):: rc

      integer :: status

      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, phase=1, userroutine=user_method1, __RC__)
      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, phase=2, userroutine=user_method2, __RC__)
      _RETURN(ESMF_SUCCESS)
   end subroutine set_services

end module mapl_MaplGenericComponent
