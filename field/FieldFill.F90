#include "MAPL.h"

module mapl3g_FieldFill
   use mapl_FieldPointerUtilities, only: assign_fptr
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: MAPL_FieldFill

   interface MAPL_FieldFill
      procedure :: field_fill
   end interface MAPL_FieldFill

contains

   subroutine field_fill(field, rc)
      use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_signaling_nan
      use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT32, INT64
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      real(REAL32), pointer :: ptr_r4(:)
      real(REAL64), pointer :: ptr_r8(:)
      integer(INT32), pointer :: ptr_i4(:)
      integer(INT64), pointer :: ptr_i8(:)
      integer :: status
      real(REAL32) :: snan_r4
      real(REAL64) :: snan_r8
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_TypeKind_Flag) :: typekind

      ! Verify field is complete (has allocated data)
      call ESMF_FieldGet(field, status=field_status, _RC)
      _ASSERT(field_status == ESMF_FIELDSTATUS_COMPLETE, 'Field must be completed prior to fill')

      ! Get typekind from the field
      call ESMF_FieldGet(field, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         snan_r4 = ieee_value(snan_r4, ieee_signaling_nan)
         call assign_fptr(field, ptr_r4, _RC)
         ptr_r4 = snan_r4
      else if (typekind == ESMF_TYPEKIND_R8) then
         snan_r8 = ieee_value(snan_r8, ieee_signaling_nan)
         call assign_fptr(field, ptr_r8, _RC)
         ptr_r8 = snan_r8
      else if (typekind == ESMF_TYPEKIND_I4) then
         call assign_fptr(field, ptr_i4, _RC)
         ptr_i4 = -huge(1_INT32)
      else if (typekind == ESMF_TYPEKIND_I8) then
         call assign_fptr(field, ptr_i8, _RC)
         ptr_i8 = -huge(1_INT64)
      end if

      _RETURN(_SUCCESS)
   end subroutine field_fill

end module mapl3g_FieldFill
