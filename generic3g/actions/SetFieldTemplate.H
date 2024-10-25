! THIS WILL PROBABLY GO AWAY

!_LEFT_ARG, _LEFT_KIND, _FUNC
!_RIGHT_ARG, _RIGHT_KIND
!_CHECK_ZEROES, _SETUNDEF


#define _WHERE_LEFT ptr_left /= undef_left 

#if defined(_RIGHT_ARG)
#  define _WHERE_RIGHT .and. ptr_right /= undef_right
#  define _FUNCTION_ARGS _FUNCTION_ARGS_(ptr_left, ptr_right)
#else
#  define _WHERE_RIGHT
#  define _FUNCTION_ARGS ptr_left
#endif

#define _OUT_VAL real(_FUNC(_FUNCTION_ARGS), kind=_LEFT_KIND)

#if defined(_CHECK_ZEROES)
#  define _WHERE_ZERO .and. ptr_right /= 0
#else
#  define _WHERE_ZERO
#endif

#if defined(_SETUNDEF)
#  define _OUT_VAL_UNDEF undef_out
#else
#  define _OUT_VAL_UNDEF ptr_out
#endif

#define _WHERE where(_WHERE_LEFT _WHERE_RIGHT _WHERE_ZERO)

   integer, optional, intent(out) :: rc
   integer :: status

   real(kind=_LEFT_KIND), pointer :: ptr_left(:) => null()
   real(kind=kind(ptr_left)) :: undef_left
   real(kind=_LEFT_KIND), pointer :: ptr_out(:) => null()
   real(kind=kind(ptr_left)) :: undef_out
#if defined(_RIGHT_ARG)
   real(kind=_RIGHT_KIND), pointer :: ptr_right(:) => null()
   real(kind=kind(ptr_right)) :: undef_right
   call GetFieldUndef(_RIGHT_ARG,undef_right,_RC)
   call assign_fptr(_RIGHT_ARG,ptr_right,_RC)
#endif
   call GetFieldUndef(_LEFT_ARG,undef_left,_RC)
   call assign_fptr(_LEFT_ARG,ptr_left,_RC)
   call GetFieldUndef(field_out, undef_out, _RC)
   call assign_fptr(field_out,ptr_out,_RC)
   where(_WHERE)
      ptr_out = _OUT_VAL
   elsewhere
      ptr_out = _OUT_VAL_UNDEF
   end where
#undef _WHERE_ZERO
#undef _WHERE_RIGHT
#undef _WHERE_LEFT
#undef _WHERE
#undef _OUT_VAL
#undef _OUT_VAL_UNDEF
! vim:ft=fortran
