   integer, optional, intent(out) :: rc
   integer :: status

   real(kind=_LEFT_KIND), pointer :: ptr_left(:) => null()
   real(kind=kind(ptr_left)) :: undef_left(1)
   real(kind=_VAL_KIND), pointer :: ptr_out(:) => null()
   real(kind=kind(ptr_out)) :: undef_out(1)
#if defined(_RIGHT_ARG)
   real(kind=_RIGHT_KIND), pointer :: ptr_right(:) => null()
   real(kind=kind(ptr_right)) :: undef_right(1)
   call assign_fptr(_RIGHT_ARG,ptr_right,_RC)
   call GetFieldsUndef([_RIGHT_ARG],undef_right,_RC)
#endif
   call assign_fptr(_LEFT_ARG,ptr_left,_RC)
   call assign_fptr(field_out,ptr_out,_RC)
   call GetFieldsUndef([_LEFT_ARG],undef_left,_RC)
   call GetFieldsUndef(undef_left,_RC)
#if defined(CHECK_ZEROES)
   where(ptr_left /= undef(LEFT) .and. ptr_right /= undef(RIGHT) .and. ptr_right /= 0)
#else
   where(ptr_left /= undef(LEFT) .and. ptr_right /= undef(RIGHT))
#endif
      ptr_out = real(_FUNC(ptr_left, ptr_right), kind=_VAL_KIND)
   elsewhere
      if(_SETUNDEF) ptr_out = undef(OUT_)
   end where

! vim:ft=fortran
