! THIS WILL PROBABLY GO AWAY

#define _LEFT_KIND _KIND(_LEFTK)
#define _VAL_KIND _LEFT_KIND
#if defined(_RIGHTK)
#   define _RIGHT_KIND _KIND(_RIGHTK)
#   define _FUNCTION_ARGS _FUNCTION_ARGS_(_LEFT_ARG, _RIGHT_ARG)
   elemental function _FUNC _PAR(_FUNCTION_ARGS) result(val)
      real(kind=_RIGHT_KIND), intent(in) :: _RIGHT_ARG
#else
#   define _RIGHT_KIND
#   define _FUNCTION_ARGS _LEFT_ARG
   elemental function _FUNC _PAR(_FUNCTION_ARGS) result(val)
#endif
      real(kind=_VAL_KIND) :: val
      real(kind=_LEFT_KIND), intent(in) :: _LEFT_ARG

      val = _EXPRESSION

   end function _FUNC
#undef _LEFT_KIND
#undef _RIGHT_KIND
#undef _VAL_KIND
#undef _FUNCTION_ARGS
!vim: ft=fortran
