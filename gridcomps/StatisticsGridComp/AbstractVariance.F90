#include "MAPL.h"
module mapl3g_AbstractVariance
   use mapl3g_AbstractTimeStatistic
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   implicit none(type, external)
   private
   public :: Variance

   type, abstract, extends(AbstractTimeStatistic) :: Variance
      private
      type(ESMF_Alarm) :: alarm
      type(ESMF_Field) :: f      ! input
      type(ESMF_Field) :: fvariance
      integer, allocatable :: counts(:)
      logical :: biased_ = .TRUE.
   contains
      procedure :: initialize
      procedure :: destroy
      procedure :: reset
      procedure :: update
      procedure :: reset
      procedure :: compute_result
      procedure :: biased
      procedure, private :: set_common
      procedure(VarianceAction), deferred :: post_initialize
      procedure(VarianceAction), deferred :: post_destroy
      procedure(VarianceAction), deferred :: post_reset
      procedure(VarianceAction), deferred :: update_r4
      procedure(VarianceAction), deferred :: update_r8
      procedure(VarianceAction), deferred :: compute_r4
      procedure(VarianceAction), deferred :: compute_r8
   end type Variance

   abstract interface
      subroutine VarianceAction(this, rc)
         import :: Variance
         class(Variance), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine VarianceAction
   end interface

contains

   subroutine initialize(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      integer, allocatable :: localElementCount(:)
      integer :: rank
      character(len=:), allocatable :: name

      call MAPL_FieldClone(this%f, this%fvariance, _RC)
      call MAPL_FieldGet(this%f, short_name=name, _RC)
      call ESMF_FieldSet(this%fvariance, name='variance_' // name, _RC)
      call ESMF_FieldGet(this%f, rank=rank, _RC)
      allocate(localElementCount(rank))
      call ESMF_FieldGet(this%f, localElementCount=localElementCount, _RC)
      allocate(this%counts(product(localElementCount)))
      call this%post_initialize(_RC)
      call this%reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine destroy(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_FieldDestroy(this%fvariance, _RC)
      deallocate(this%counts)
      call this%post_destroy(_RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine reset(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_FieldFill(this%fvariance, dataFillScheme='const', const1=0.d0, _RC)
      this%counts = 0
      call this%post_reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine reset

   subroutine update(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call this%update_r4(_RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call this%update_r8(_RC)
      end if

      is_ringing = esmf_AlarmWillRingNext(this%alarm, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%compute_result(_RC)
      call this%reset(_RC)

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine compute_result(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_TypeKind_Flag) :: typekind

      call mapl_FieldGet(this%f, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call this%compute_result_r4(_RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call this%compute_result_r8(_RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine compute_result

   subroutine add_to_state(this, state, rc)
      class(Variance), intent(inout) :: this
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: was_ringing

      was_ringing = ESMF_AlarmWasPrevRinging(this%alarm, _RC)
      _RETURN_UNLESS(was_ringing)

      call ESMF_StateAdd(state, [this%fvariance], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   logical function biased(this)
      class(Variance), intent(in) :: this

      biased = this%biased_

   end function biased

   subroutine set_common(var, unusable, f, variance_field, alarm, biased)
      class(Variance), intent(inout) :: var
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), intent(in) :: f
      type(ESMF_Field), intent(in) :: variance_field
      type(ESMF_Alarm), intent(in) :: alarm
      character(len=*), optional, intent(in) :: biased

      var%f = f
      var%fvariance = variance_field
      var%alarm = alarm
      if(present(biased)) var%biased_ = biased

      _UNUSED_DUMMY(unusable)
   end subroutine set_common

end module mapl3g_AbstractVariance
