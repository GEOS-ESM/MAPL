#include "MAPL.h"

module mapl3g_FieldFill
   use mapl3g_FieldFillDefault
   use mapl_FieldPointerUtilities, only: assign_fptr
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: FieldFill

   interface FieldFill
      procedure :: field_fill
   end interface FieldFill

contains

   subroutine field_fill(field, rc)
      use, intrinsic :: iso_fortran_env, only: INT32, INT64
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: ptr_r4(:)
      real(ESMF_KIND_R8), pointer :: ptr_r8(:)
      integer(INT32), pointer :: ptr_i4(:)
      integer(INT64), pointer :: ptr_i8(:)
      integer :: status
      type(ESMF_FieldStatus_Flag) :: field_status
      type(ESMF_TypeKind_Flag) :: typekind

       ! Verify field is complete (has allocated data)
       call ESMF_FieldGet(field, status=field_status, _RC)
       _ASSERT(field_status == ESMF_FIELDSTATUS_COMPLETE, 'Field must be completed prior to fill')

       ! Get typekind from the field
       call ESMF_FieldGet(field, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr(field, ptr_r4, _RC)
         ptr_r4 = get_field_fill_default_r4()
      else if (typekind == ESMF_TYPEKIND_R8) then
         call assign_fptr(field, ptr_r8, _RC)
         ptr_r8 = get_field_fill_default_r8()
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
