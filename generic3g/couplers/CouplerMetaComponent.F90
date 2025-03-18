#include "MAPL_Generic.h"

module mapl3g_CouplerMetaComponent

   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE, GENERIC_COUPLER_INVALIDATE
   use mapl3g_ComponentDriver, only: ComponentDriver, ComponentDriverPtr
   use mapl3g_GriddedComponentDriver, only: GriddedComponentDriver
   use mapl3g_ComponentDriverVector, only: ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector, only: ComponentDriverPtrVector
   use mapl3g_ExtensionTransform
   use mapl3g_VerticalRegridTransform
   use mapl_ErrorHandlingMod
   use mapl3g_ESMF_Interfaces
   use esmf

   implicit none
   private

   ! Class
   public :: CouplerMetaComponent

   ! non TBF procedures
   public :: get_coupler_meta
   public :: attach_coupler_meta
   public :: free_coupler_meta

   type :: CouplerMetaComponent
      private
      class(ExtensionTransform), allocatable :: transform
      type(ComponentDriverPtrVector) :: sources
      type(ComponentDriverVector) :: consumers
      logical :: stale = .true.
   contains
      ! ESMF methods
      procedure :: initialize
      procedure :: update
      procedure :: invalidate
      procedure :: clock_advance

      ! Helper procedures
      procedure :: update_sources
      procedure :: invalidate_consumers
      procedure :: add_source
      procedure :: add_consumer

      ! Accessors
      procedure, non_overridable :: is_up_to_date
      procedure, non_overridable :: is_stale
      procedure, non_overridable :: set_up_to_date
      procedure, non_overridable :: set_stale
   end type CouplerMetaComponent

   character(len=*), parameter :: COUPLER_META_PRIVATE_STATE = "CouplerMetaComponent Private State"

   type CouplerMetaWrapper
      type(CouplerMetaComponent), pointer :: coupler_meta
   end type CouplerMetaWrapper

   interface CouplerMetaComponent
      procedure :: new_CouplerMetaComponent
   end interface CouplerMetaComponent

contains

   function new_CouplerMetaComponent(transform, source) result (this)
      type(CouplerMetaComponent) :: this
      class(ExtensionTransform), intent(in) :: transform
      class(ComponentDriver), target, optional, intent(in) :: source

      type(ComponentDriverPtr) :: source_wrapper

      this%transform = transform
      if (present(source)) then
         source_wrapper%ptr => source
         call this%sources%push_back(source_wrapper)
      end if

   end function new_CouplerMetaComponent

   recursive subroutine initialize(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      call this%transform%initialize(importState, exportState, clock, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine initialize

   ! Check if export item has been updated and update import item
   ! accordingly.
   recursive subroutine update_time_varying(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

!#      _RETURN_UNLESS(this%export_is_time_varying())
      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

!#      call FieldUpdate(f_in, from=f_out, ignore=this%transform%get_ignore(), _RC)
      
      _RETURN(_SUCCESS)
   end subroutine update_time_varying

   recursive subroutine update(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      _RETURN_IF(this%is_up_to_date())

      !# call this%propagate_attributes(_RC)
      call this%update_sources(_RC)
      
      call this%transform%update(importState, exportState, clock, _RC)
      call this%set_up_to_date()

      _RETURN(_SUCCESS)
   end subroutine update

   recursive subroutine update_sources(this, rc)
      class(CouplerMetaComponent) :: this
      integer, intent(out) :: rc

      integer :: status
      integer :: i
      type(ComponentDriverPtr), pointer :: source_wrapper

      do i = 1, this%sources%size()
         source_wrapper => this%sources%of(i)
         call source_wrapper%ptr%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine update_sources

   ! Check if export item has been updated and update import item
   ! accordingly.
   recursive subroutine invalidate_time_varying(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

!#      _RETURN_UNLESS(this%import_is_time_varying())
      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

!#      call FieldUpdate(f_out, from=f_in, ignore=this%transform%get_ignore(), _RC)
      
      _RETURN(_SUCCESS)
   end subroutine invalidate_time_varying

   recursive subroutine invalidate(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      if(this%transform%runs_invalidate()) then
         call this%update_sources(_RC)
         call this%transform%invalidate(importState, exportState, clock, _RC)
      end if
      _RETURN_IF(this%is_stale())

      call this%invalidate_consumers(_RC)
      call this%set_stale()
  
      _RETURN(_SUCCESS)

   end subroutine invalidate
      
   recursive subroutine invalidate_consumers(this, rc)
      class(CouplerMetaComponent), target :: this
      integer, intent(out) :: rc

      integer :: status
      class(ComponentDriver), pointer :: consumer
      integer :: i

      do i = 1, this%consumers%size()
         consumer => this%consumers%of(i)
         call consumer%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine invalidate_consumers

   recursive subroutine clock_advance(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Alarm) :: alarm
      logical :: is_ringing

      call ESMF_ClockGetAlarm(clock, "MAPL::RUN_ALARM", alarm, _RC)
      is_ringing = ESMF_AlarmIsRinging(alarm, _RC)
      _RETURN_UNLESS(is_ringing)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(importState)
   end subroutine clock_advance
      
   function add_consumer(this) result(consumer)
      class(ComponentDriver), pointer :: consumer
      class(CouplerMetaComponent), target, intent(inout) :: this

      call this%consumers%resize(this%consumers%size() + 1)
      consumer => this%consumers%back()
   end function add_consumer

   subroutine add_source(this, source)
      class(CouplerMetaComponent), target, intent(inout) :: this
      type(GriddedComponentDriver), pointer, intent(in) :: source

      type(ComponentDriverPtr) :: source_wrapper
      source_wrapper%ptr => source

      call this%sources%push_back(source_wrapper)
   end subroutine add_source

   function get_coupler_meta(gridcomp, rc) result(meta)
      type(CouplerMetaComponent), pointer :: meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      !TODO: This block is a workaround for weird link error with NAG
      !      7.2 Appears to be a collision in numbering of local
      !      scopes.
      block
      end block
      _GET_NAMED_PRIVATE_STATE(gridcomp, CouplerMetaComponent, COUPLER_META_PRIVATE_STATE, meta)

      _RETURN(_SUCCESS)
   end function get_coupler_meta

   subroutine attach_coupler_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _SET_NAMED_PRIVATE_STATE(gridcomp, CouplerMetaComponent, COUPLER_META_PRIVATE_STATE)

      _RETURN(_SUCCESS)
   end subroutine attach_coupler_meta

   subroutine free_coupler_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(CouplerMetaWrapper) :: wrapper

      call MAPL_UserCompGetInternalState(gridcomp, COUPLER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "CouplerMetaComponent not created for this gridcomp")

      deallocate(wrapper%coupler_meta)

      _RETURN(_SUCCESS)
   end subroutine free_coupler_meta

   pure subroutine set_up_to_date(this)
      class(CouplerMetaComponent), intent(inout) :: this
      this%stale = .false.
   end subroutine set_up_to_date

   pure subroutine set_stale(this)
      class(CouplerMetaComponent), intent(inout) :: this
      this%stale = .true.
    end subroutine set_stale

    pure logical function is_up_to_date(this)
      class(CouplerMetaComponent), intent(in) :: this
      is_up_to_date = .not. this%stale
   end function is_up_to_date

   pure logical function is_stale(this)
      class(CouplerMetaComponent), intent(in) :: this
      is_stale = this%stale
   end function is_stale

end module mapl3g_CouplerMetaComponent
