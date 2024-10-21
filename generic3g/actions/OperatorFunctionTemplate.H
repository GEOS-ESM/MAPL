#define _NAME field
#include "function_overload.macro"
#if defined(_KIND)
#   undef _KIND
#endif
#define _KIND R4
   elemental function _SUBKIND(x, y) result(z)
      real(kind=ESMF_KIND_R4) :: z
      real(kind=ESMF_KIND_R4), intent(in) :: x, y
      z = x OP_ y
   end function add_r4
#   undef _KIND
#define _KIND R8
   elemental function _FUNC_r8(x, y) result(z)
      real(kind=ESMF_KIND_R8) :: z
      real(kind=ESMF_KIND_R8), intent(in) :: x, y
      z = x OP_ y
   end function add_r8
#   undef _KIND

!vim: ft=fortran
