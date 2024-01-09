#include "Generic.h"

module mapl3g_CouplerMetaComponent
   implicit none
   private

   ! Class
   public :: CouplerMetaComponent

   ! non TBF procedures
   public :: get_coupler_meta
   public :: attach_coupler_meta
   public :: free_coupler_meta

   ! Phase indices
   public :: GENERIC_COUPLER_UPDATE
   public :: GENERIC_COUPLER_INVALIDATE
   public :: GENERIC_COUPLER_CLOCK_ADVANCE

   type :: CouplerMetaComponent
      private
      class(ExtensionAction), allocatable :: action
      type(ComponentHandler), pointer :: source => null()
      type(ComponentHandlerVector) :: consumers
      logical :: stale = .true.
   contains
      ! ESMF methods
      procedure :: update
      procedure :: invalidate
      procedure :: advance

      ! Helper procedures
      procedure :: update_source
      procedure :: invalidate_consumers
      procedure :: set_source
      procedure :: add_consumer

      ! Accessors
      procedure, non_overridable :: is_up_to_date
      procedure, non_overridable :: is_stale
      procedure, non_overridable :: set_up_to_date
      procedure, non_overridable :: set_stale
   end type CouplerMetaComponentComponent

   enum, bind(c)
      enumerator :: GENERIC_CPLR_UPDATE = 1
      enumerator :: GENERIC_CPLR_INVALIDATE = 1
   end enum

   character(len=*), parameter :: COUPLER_META_PRIVATE_STATE = "CouplerMetaComponent Private State"

   type CouplerMetaWrapper
      type(CouplerMetaComponent), pointer :: coupler_meta
   end type CouplerMetaWrapper

contains


   function new_CouplerMetaComponent(action, source_coupler) result (this)
      type(CouplerMetaComponent) :: this
      class(ExtensionAction), intent(in) :: action
      type(ComponentHandler), pointer, optional, intent(in) :: source_coupler

      this%aciton = action
      this%source_coupler => source_coupler

   end function new_CouplerMetaComponent


   subroutine update(this, sourceState, exportState, clock, rc)
      type(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: sourceState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      up_to_date = this%is_up_to_date(_RC)
      _RETURN_IF(up_to_date)

      call this%update_source(_RC)
      call this%action%update(_RC)
      call this%set_up_to_date()`

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine update_source(this, rc)
      type(CouplerMetaComponent) :: this
      integer, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(associated(this%source_coupler)
      call this%source_coupler%run(GENERIC_CPLR_UPDATE, _RC)

      _RETURN(_SUCCESS)
   end subroutine update_source

   subroutine invalidate(this, sourceState, exportState, clock, rc)
        type(CouplerMetaComponent) :: this
        type(ESMF_State) :: sourceState
        type(ESMF_State) :: exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        stale = this%is_stale(_RC)
        _RETURN_IF(stale)

        call this%action%invalidate(_RC) ! eventually needs access to clock
        call this%invalidate_consumers(_RC)
        call this%set_stale()
  
        _RETURN(_SUCCESS)
    end subroutine invalidate
      
   subroutine invalidate_consumers(this, rc)
      type(CouplerMetaComponent), target :: this
      integer, intent(out) :: rc

      integer :: status
      type(ComponentHandler), pointer :: consumer
      integer :: i

      do i = 1, this%export_couplers%size()
         consumer => this%consumers%of(i)
         call consumer%run(GENERIC_CPLR_INVALIDATE, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine update_consumers

   subroutine advance(this, sourceState, exportState, clock, rc)
        type(CouplerMetaComponent), intent(inout) :: this
        type(ESMF_State), intent(inout) :: sourceState
        type(ESMF_State), intent(inout) :: exportState
        type(ESMF_Clock), intent(inout) :: clock
        integer, optional, intent(out) :: rc

        integer :: status
        type(ESMF_Alarm) :: alarm

        call ESMF_ClockGetAlarm(clock, "MAPL::RUN_ALARM", alarm, _RC)
        is_ringing = ESMF_AlarmIsRinging(alarm, _RC)
        _RETURN_UNLESS(is_ringing)

        call this%action%advance(_RC) ! eventually needs access to clock

        _RETURN(_SUCCESS)
    end subroutine invalidate

      
   function add_consumer(this) result(consumer)
      type(ComponentHandler), pointer :: consumer
      class(CouplerMetaComponent), target, intent(inout) :: this

      call this%consumers%resize(this%export_couplers%size() + 1)
      consumer => this%consumers%back()
      
   end subroutine add_consumer

   subroutine set_source(this, source)
      class(CouplerMetaComponent), target, intent(inout) :: this
      type(ComponentHandler), pointer, intent(in) :: source

      this%source => source
   end subroutine set_source


   function get_coupler_meta(gridcomp, rc) result(meta)
      type(CouplerMetaComponent), pointer :: meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _GET_NAMED_PRIVATE_STATE(gridcomp, CouplerMetaComponent, COUPLER_META_PRIVATE_STATE, meta)

      _RETURN(_SUCCESS)
   end function get_coupler_meta

   subroutine attach_coupler_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: meta

      _SET_NAMED_PRIVATE_STATE(gridcomp, CouplerMetaComponent, COUPLER_META_PRIVATE_STATE, meta)

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

   subroutine free_coupler_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(CouplerMetaWrapper) :: wrapper
      type(ESMF_GridComp) :: user_gridcomp

      call MAPL_UserCompGetInternalState(gridcomp, COUPLER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "CouplerMetaComponent not created for this gridcomp")

      deallocate(wrapper%coupler_meta)

      _RETURN(_SUCCESS)
   end subroutine free_coupler_meta


   pure subroutine set_up_to_date(this)
      class(Observer), intent(inout) :: this
      this%up_to_date = .true
   end subroutine set_up_to_date

   pure subroutine set_stale(this)
      class(Observer), intent(inout) :: this
      this%up_to_date = .false
    end subroutine set_stale

    pure logical function is_up_to_date(this)
      class(Observer), intent(in) :: this
      is_up_to_date = this%up_to_date
   end function is_up_to_date

   pure logical function is_stale(this)
      class(Observer), intent(in) :: this
      is_stale = .not. this%up_to_date
   end function is_up_to_date

end module mapl3g_CouplerMetaComponent
