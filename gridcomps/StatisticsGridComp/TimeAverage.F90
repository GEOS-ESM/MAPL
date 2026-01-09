#include "MAPL.h"
module mapl3g_TimeAverage
   use mapl3g_AbstractTimeStatistic
   use mapl3
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   implicit none(type,external)
   private

   public :: TimeAverage

   type, extends(AbstractTimeStatistic) :: TimeAverage
      private
      type(esmf_Alarm) :: alarm
      type(esmf_Field) :: f      ! input
      type(esmf_Field) :: avg_f  ! output
      type(esmf_Field) :: sum_f
      integer, allocatable :: counts(:)
   contains
      procedure :: initialize
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: compute_result
      procedure :: add_to_state
   end type TimeAverage

   interface TimeAverage
      module procedure new_TimeAverage
   end interface TimeAverage

contains

   function new_TimeAverage(unusable, f, avg_f, alarm) result(stat)
      type(TimeAverage) :: stat
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(in) :: avg_f
      type(esmf_Alarm), intent(in) :: alarm

      integer :: state

      stat%f = f
      stat%avg_f = avg_f
      stat%alarm = alarm

   end function new_TimeAverage


   subroutine initialize(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: name
      integer, allocatable :: localElementCount(:)
      integer :: rank

      call mapl_FieldGet(this%f, short_name=name, _RC)
      call mapl_FieldClone(this%f, this%sum_f, _RC)

      call esmf_FieldSet(this%sum_f, name='sum_'//name, _RC)

      call esmf_FieldGet(this%f, rank=rank, _RC)
      allocate(localElementCount(rank))
      call esmf_FieldGet(this%f, localElementCount=localElementCount, _RC)
      allocate(this%counts(product(localElementCount)))

      call this%reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine destroy(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%sum_f, _RC)
      call esmf_FieldDestroy(this%avg_f, _RC)

      deallocate(this%counts)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldFill(this%sum_f, dataFillScheme='const', const1=0.d0, _RC)
      this%counts = 0

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, rc)
      class(TimeAverage), intent(inout) :: this
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
      class(TimeAverage), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: f(:), sum_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%sum_f, sum_f, _RC)

      where (f /= MAPL_UNDEF)
         sum_f = sum_f + f
         this%counts = this%counts + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: f(:), sum_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%sum_f, sum_f, _RC)

      where (f /= MAPL_UNDEF)
         sum_f = sum_f + f
         this%counts = this%counts + 1
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call compute_result_r4(this, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call compute_result_r8(this, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine compute_result_r4(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: f(:), sum_f(:), avg_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%sum_f, sum_f, _RC)
      call MAPL_AssignFptr(this%avg_f, avg_f, _RC)

      where (this%counts > 0)
         avg_f = sum_f / this%counts
      elsewhere
         avg_f = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)
   end subroutine compute_result_r4

   subroutine compute_result_r8(this, rc)
      class(TimeAverage), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: f(:), sum_f(:), avg_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%sum_f, sum_f, _RC)

      where (this%counts > 0)
         avg_f = sum_f / this%counts
      elsewhere
         avg_f = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)
    end subroutine compute_result_r8
      
      
  subroutine add_to_state(this, state, rc)
      class(TimeAverage), intent(inout) :: this
      type(esmf_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: was_ringing

      was_ringing = ESMF_AlarmWasPrevRinging(this%alarm, _RC)
      _RETURN_UNLESS(was_ringing)

      call esmf_StateAdd(state, [this%avg_f], _RC)
      
      _RETURN(_SUCCESS)
   end subroutine add_to_state

end module mapl3g_TimeAverage
