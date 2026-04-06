#include "MAPL.h"

module mapl3g_TimeMax

   use ESMF
   use mapl3g_AbstractTimeStatistic
   use MAPL
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
      type(esmf_Field) :: temp_max_f
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

      call mapl_FieldGet(this%f, short_name=name, _RC)
      call mapl_FieldClone(this%f, this%temp_max_f, _RC)

      call esmf_FieldSet(this%temp_max_f, name='temp_max'//name, _RC)

      call this%reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine destroy(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldDestroy(this%max_f, _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R4), pointer :: f4(:)
      real(kind=ESMF_KIND_R8), pointer :: f8(:)

      call mapl_FieldGet(this%temp_max_f, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call MAPL_AssignFptr(this%temp_max_f, f4,  _RC)
         f4 = MAPL_UNDEFINED_REAL
      else if (typekind == ESMF_TYPEKIND_R8) then
         call MAPL_AssignFptr(this%temp_max_f, f8,  _RC)
         f8 = MAPL_UNDEFINED_REAL64
      end if

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
      real(kind=ESMF_KIND_R4), pointer :: f(:), temp_max_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%temp_max_f, temp_max_f, _RC)

      where (f /= MAPL_UNDEFINED_REAL)
         where (temp_max_f == MAPL_UNDEFINED_REAL)
            temp_max_f = f
         elsewhere
            temp_max_f = max(temp_max_f, f)
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r4

   subroutine update_r8(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: f(:), temp_max_f(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%temp_max_f, temp_max_f, _RC)

      where (f /= MAPL_UNDEFINED_REAL64)
         where (temp_max_f == MAPL_UNDEFINED_REAL64)
            temp_max_f = f
         elsewhere
            temp_max_f = max(temp_max_f, f)
         end where
      end where

      _RETURN(_SUCCESS)
   end subroutine update_r8

   subroutine compute_result(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call compute_result_r4(this, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call compute_result_r8(this, _RC)
      end if
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine compute_result_r4(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: temp_max_f(:), max_f(:)

      call MAPL_AssignFptr(this%temp_max_f, temp_max_f, _RC)
      call MAPL_AssignFptr(this%max_f, max_f, _RC)

      max_f = temp_max_f

      _RETURN(_SUCCESS)
   end subroutine compute_result_r4

   subroutine compute_result_r8(this, rc)
      class(TimeMax), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: temp_max_f(:), max_f(:)

      call MAPL_AssignFptr(this%temp_max_f, temp_max_f, _RC)
      call MAPL_AssignFptr(this%max_f, max_f, _RC)

      max_f = temp_max_f

      _RETURN(_SUCCESS)
   end subroutine compute_result_r8

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
