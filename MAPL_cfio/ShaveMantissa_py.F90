subroutine Shave32 ( a_shaved, a, n, xbits, has_undef, undef, chunksize, rc )

!
!  Simple cover for f2py.
!
   use iso_fortran_env, only: REAL32
   implicit NONE
   integer,           intent(in) :: n              ! array size
   real(kind=REAL32), intent(in) :: a(n)           ! array to be shaved, usually 2D
   integer,           intent(in) :: xbits          ! number of mantissa bits to zero (out of 24)
   integer,           intent(in) :: has_undef      ! set to 1 if undef is present, 0 otherwise
   real(kind=REAL32), intent(in) :: undef          ! undef value
   integer,           intent(in) :: chunksize      ! find mid-range over chunksizes

   real(kind=REAL32), intent(out) :: a_shaved(n)  ! shaved array
   integer,           intent(out) :: rc            ! error code

!                            ---

   integer, external :: ShaveMantissa32

   rc = ShaveMantissa32(a_shaved,a,n,xbits,has_undef,undef,chunksize)

end subroutine Shave32


