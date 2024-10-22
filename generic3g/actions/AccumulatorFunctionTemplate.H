#if !defined(_NAME)
#  define _NAME f
#endif
#include "function_overload.macro"
#if defined(_RIGHT_ARG)
   function _FUNC (_LEFT_ARG, _RIGHT_ARG) result(val)
      real(kind=_RIGHT_KIND), intent(in) :: _RIGHT_ARG
#else
   function _FUNC (_LEFT_ARG) result(val)
#endif
      real(kind=_VAL_KIND) :: val
      real(kind=_LEFT_KIND), intent(in) :: _LEFT_ARG

      val = _EXPRESSION

   end function _FUNC
#include "undo_function_overload.macro"
#undef _NAME
!vim: ft=fortran
