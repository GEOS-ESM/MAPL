#include "MAPL.h"

module mapl3g_ShiftedVariance
   use mapl3g_AbstractVariance, only: Variance
   use mapl3
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type,external)
   private

   public :: ShiftedVariance

   type, extends(Variance) :: ShiftedVariance 
      private
      type(ESMF_Field) :: k
      type(ESMF_Field) :: ex
      type(ESMF_Field) :: ex2
   contains
      procedure :: post_initialize
      procedure :: post_destroy
      procedure :: post_reset
      procedure :: update_r4
      procedure :: update_r8
      procedure :: compute_r4
      procedure :: compute_r8
   end type ShiftedVariance 

   interface ShiftedVariance
      module procedure new_ShiftedVariance
   end interface ShiftedVariance

contains

!   function new_ShiftedVariance(unusable, f, variance_field, alarm, biased) result(var)
!      type(ShiftedVariance) :: var
!      class(KeywordEnforcer), optional, intent(in) :: unusable
!      type(ESMF_Field), intent(in) :: f
!      type(ESMF_Field), intent(in) :: variance_field
!      type(ESMF_Alarm), intent(in) :: alarm
!      character(len=*), optional, intent(in) :: biased

!      call set_common(var, f=f, variance_field=variance_field, alarm=alarm, biased=biased)
!      _UNUSED_DUMMY(unusable)

!   end function new_ShiftedVariance

   subroutine post_initialize(this, rc)
      class(ShiftedVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
     
      call MAPL_FieldClone(this%f, this%k, _RC)
      call MAPL_FieldClone(this%f, this%ex, _RC)
      call MAPL_FieldClone(this%f, this%ex2, _RC)
      _RETURN(_SUCCESS)

   end subroutine post_initialize

   subroutine post_destroy(this, rc)
      class(ShiftedVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_FieldDestroy(this%k, _RC)
      call ESMF_FieldDestroy(this%ex, _RC)
      call ESMF_FieldDestroy(this%ex2, _RC)
      _RETURN(_SUCCESS)

   end subroutine post_destroy

   subroutine post_reset(this, rc)
      class(ShiftedVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_FieldFill(this%k, dataFillScheme='const', const1=0.d0, _RC)
      call ESMF_FieldFill(this%ex, dataFillScheme='const', const1=0.d0, _RC)
      call ESMF_FieldFill(this%ex2, dataFillScheme='const', const1=0.d0, _RC)
      _RETURN(_SUCCESS)

   end subroutine post_reset

   subroutine update_r4(this, rc)
      class(ShiftedVariance), intent(inout) :: this
      integer, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: f(:), k(:), ex(:), ex2(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%k, k, _RC)
      call MAPL_AssignFptr(this%ex, ex, _RC)
      call MAPL_AssignFptr(this%ex2, ex2, _RC)

      where ((f /= MAPL_UNDEF) .and. (this%counts == 0))
         k = f
      end where

      where (f /= MAPL_UNDEF)
         this%counts = this%counts + 1
         ex = ex + (f - k)
         ex2 = ex2 + (f - k) * (f - k)
      end where

      _RETURN(_SUCCESS)

   end subroutine update_r4

   subroutine update_r8(this, rc)
      class(Variance), intent(inout) :: this
      integer, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: f(:), k(:), ex(:), ex2(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%k, k, _RC)
      call MAPL_AssignFptr(this%ex, ex, _RC)
      call MAPL_AssignFptr(this%ex2, ex2, _RC)

      where ((f /= MAPL_UNDEF) .and. (this%counts == 0))
         k = f
      end where

      where (f /= MAPL_UNDEF)
         this%counts = this%counts + 1
         ex = ex + (f - k)
         ex2 = ex2 + (f - k) * (f - k)
      end where

      _RETURN(_SUCCESS)

   end subroutine update_r8

   subroutine compute_r4(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: ex(:), ex2, var(:)
      integer :: counts_offset

      counts_offset = 1
      if(this%biased()) counts_offset = 0
      call MAPL_AssignFptr(this%ex, ex, _RC)
      call MAPL_AssignFptr(this%ex2, ex2, _RC)
      call MAPL_AssignFptr(this%fvariance, var, _RC)

      where (this%counts > counts_offset)
         var = (ex - ex2 / this%counts ) / (this%counts-counts_offset)
      elsewhere
         var = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)

   end subroutine compute_r4

   subroutine compute_r8(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: ex(:), ex2(:), var(:)
      integer :: counts_offset

      counts_offset = 1
      if(this%biased()) counts_offset = 0
      call MAPL_AssignFptr(this%ex, ex, _RC)
      call MAPL_AssignFptr(this%ex2, ex2, _RC)
      call MAPL_AssignFptr(this%fvariance, var, _RC)

      where (this%counts > counts_offset)
         var = (ex - ex2 / this%counts ) / (this%counts-counts_offset)
      elsewhere
         var = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)

   end subroutine compute_r8

end module mapl3g_ShiftedVariance
