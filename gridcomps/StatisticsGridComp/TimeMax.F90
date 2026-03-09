#include "MAPL.h"

module mapl3g_TimeMax

   use mapl3g_AbstractTimeStatistic
   use mapl3
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type,external)
   private

   public :: TimeMax

   type, extends(AbstractTimeStatistic) :: TimeMax
      private
      type(esmf_Alarm) :: alarm
      type(esmf_Field) :: f      ! input
      type(esmf_Field) :: max_f  ! output
      integer, allocatable :: counts(:)
   contains
      procedure :: initialize
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
   end type TimeMax

   interface TimeMax
      module procedure new_TimeMax
   end interface TimeMax

contains

   function new_TimeMax(unusable, f, max_f, alarm) result(stat)
      type(TimeMax) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(in) :: max_f
      type(esmf_Alarm), intent(in) :: alarm

      stat%f = f
      stat%max_f = max_f
      stat%alarm = alarm

      _UNUSED_DUMMY(unusable)
   end function new_TimeMax

   subroutine initialize(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: name
      integer, allocatable :: localElementCount(:)
      integer :: rank

      call mapl_FieldGet(this%f, short_name=name, _RC)

      call esmf_FieldGet(this%f, rank=rank, _RC)
      allocate(localElementCount(rank))
      call esmf_FieldGet(this%f, localElementCount=localElementCount, _RC)
      allocate(this%counts(product(localElementCount)))

      call this%reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine destroy(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%max_f, _RC)

      deallocate(this%counts)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: max_f(:)

      call MAPL_AssignFptr(this%max_f, max_f, _RC)
      max_f = MAPL_UNDEF
      this%counts = 0

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      logical :: is_ringing

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call update_r4(this, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call update_r8(this, _RC)
      end if

      is_ringing = esmf_AlarmWillRingNext(this%alarm, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%compute_result(_RC)
      call this%reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine update_r4(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: f(:), max_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%max_f, max_f, _RC)

      where (f /= MAPL_UNDEF)
         where (max_f == MAPL_UNDEF)
            max_f = f
         elsewhere
            max_f = max(max_f, f)
         end where
         this%counts = this%counts + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: f(:), max_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%max_f, max_f, _RC)

      where (f /= MAPL_UNDEF)
         where (max_f == MAPL_UNDEF)
            max_f = f
         elsewhere
            max_f = max(max_f, f)
         end where
         this%counts = this%counts + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      ! For maximum, computation happens during updates
      ! This is a no-op subroutine to satisfy the abstract interface

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine add_to_state(this, state, rc)
      class(TimeMax), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: was_ringing

      was_ringing = ESMF_AlarmWasPrevRinging(this%alarm, _RC)
      _RETURN_UNLESS(was_ringing)

      call esmf_StateAdd(state, [this%max_f], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

end module mapl3g_TimeMax
