#include "MAPL.h"

module mapl_ExtensionTransformUtils_mod
   use mapl_field_bundle_api
   use mapl_enums_api
   use mapl_ErrorHandling_mod
   use esmf, only: ESMF_FieldBundle
   implicit none(type,external)
   private

   public :: bundle_types_valid

contains

   subroutine bundle_types_valid(b1, b2, rc)
      type(ESMF_FieldBundle), intent(inout) :: b1, b2
      integer, intent(out) :: rc
      integer :: status
      type(MAPL_FieldBundleType_Flag) :: bt1, bt2
      type(MAPL_FieldBundleType_Flag), parameter :: ALLOWED_BUNDLE_TYPES(*) = [&
         & MAPL_FIELDBUNDLETYPE_BASIC, &
         & MAPL_FIELDBUNDLETYPE_BRACKET, &
         & MAPL_FIELDBUNDLETYPE_VECTOR, &
         & MAPL_FIELDBUNDLETYPE_VECTORBRACKET&
         &]
      character(len=:), allocatable :: msg

      call MAPL_FieldBundleGet(b1, fieldBundleType=bt1, _RC)
      msg = bt1%to_string()
      _ASSERT(any(ALLOWED_BUNDLE_TYPES == bt1), 'FieldBundleType ' // msg // ' is not supported.')
      call MAPL_FieldBundleGet(b2, fieldBundleType=bt2, _RC)
      msg = '(' // msg // ', ' // bt2%to_string() // ')'
      _ASSERT(bt1 == bt2, 'FieldBundleType values ' // msg // ' do not match.')
      _RETURN(_SUCCESS)

   end subroutine bundle_types_valid

end module mapl_ExtensionTransformUtils_mod
