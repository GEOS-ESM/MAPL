#include "MAPL.h"

module mapl3g_ExtensionTransformUtils
   use mapl3g_FieldBundle_API
   use mapl_ErrorHandling
   use esmf, only: ESMF_FieldBundle
   implicit none(type, external)
   private

   public :: bundle_types_valid

contains

   subroutine bundle_types_valid(b1, b2, rc)
      type(ESMF_FieldBundle), intent(inout) :: b1, b2
      integer, intent(out) :: rc
      integer :: status
      type(FieldBundleType_Flag) :: bt1, bt2
      type(FieldBundleType_Flag), parameter :: ALLOWED_BUNDLE_TYPES(*) = [&
         & FIELDBUNDLETYPE_BASIC, &
         & FIELDBUNDLETYPE_BRACKET, &
         & FIELDBUNDLETYPE_VECTOR, &
         & FIELDBUNDLETYPE_VECTOR_BRACKET&
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

end module mapl3g_ExtensionTransformUtils
