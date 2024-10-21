   integer, optional, intent(out) :: rc
   integer :: status

   integer, parameter = LEFT_ = 1
   integer, parameter = RIGHT_ = 2
   integer, parameter = OUT_ = 3
   real(KIND_), allocatable :: undef
   real(kind=KIND_), pointer :: ptr_left(:) => null()
   real(kind=KIND_), pointer :: ptr_right(:) => null()
   real(kind=KIND_), pointer :: ptr_out(:) => null()
   type(ESMF_Field), allocatable :: fields(:)

   allocate(fields(3))
   fields(LEFT_) = left_field
   fields(RIGHT_) = right_field
   fields(OUT_) = out_field
   call assign_fptr(left_field,ptr_left,_RC)
   call assign_fptr(right_field,ptr_right,_RC)
   call assign_fptr(field_out,ptr_out,_RC)
   call GetFieldsUndef(fields,undef,_RC)
#if defined(CHECK_ZEROES)
   where(ptr_left /= undef(LEFT) .and. ptr_right /= undef(RIGHT) .and. ptr_right /= 0)
#else
   where(ptr_left /= undef(LEFT) .and. ptr_right /= undef(RIGHT))
#endif
      ptr_out = _FUNC(ptr_left, ptr_right)
   elsewhere
      if(_SETUNDEF) ptr_out = undef(OUT_)
   end where

! vim:ft=fortran
