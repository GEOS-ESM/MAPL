! THIS WILL PROBABLY GO AWAY
#define _NAME field
#if defined(_KIND)
#   undef _KIND
#endif
#define _KIND R4
#include "function_overload.macro"
   elemental function _SUBKIND(x, y) result(z)
      real(kind=ESMF_KIND_R4) :: z
      real(kind=ESMF_KIND_R4), intent(in) :: x, y
      z = x OP_ y
   end function add_r4
#include "undo_function_overload.macro"
#undef _KIND
#define _KIND R8
   elemental function _SUBKIND(x, y) result(z)
#include "function_overload.macro"
      real(kind=ESMF_KIND_R8) :: z
      real(kind=ESMF_KIND_R8), intent(in) :: x, y
      z = x OP_ y
   end function add_r8
#include "undo_function_overload.macro"
#undef _KIND
#undef _NAME
!vim: ft=fortran
