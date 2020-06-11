#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return

module pFIO_DownbitMod

   implicit none
   private

   public :: pFIO_DownBit

   interface pFIO_DownBit
      module procedure pFIO_DownBit2D
      module procedure pFIO_DownBit3D 
   end interface pFIO_DownBit

contains

   subroutine pFIO_DownBit3D ( x, xr, nbits, undef, flops, rc )

     implicit NONE

!
! !INPUT PARAMETERS:
!
     real, intent(in)    ::  x(:,:,:)       ! input array 
     integer, intent(in) :: nbits           ! number of bits per word to retain
                                            ! - no action if nbits<1
     real, OPTIONAL, intent(in) :: undef    ! missing value
     logical, OPTIONAL, intent(in) :: flops ! if true, uses slower float point
                                            !  based algorithm
!
! !OUTPUT PARAMETERS:
!
     real, intent(out)   :: xr(:,:,:) ! precision reduced array; can
!                                       ! share storage with input array
                                        ! if it has same kind
     integer, intent(out)  :: rc        ! error code
                                        !  = 0 - all is well
                                        ! /= 0 - something went wrong 
!
! !DESCRIPTION:  
!
!  This routine returns a lower precision version of the input array
!  {\tt x} which retains {\tt nbits} of precision. See routine
!  {\tt ESMF\_CFIODownBit2D} for additional details. This version for
!  rank 3 arrays, calls {\tt ESMF\_CFIODownBit2D()} for each vertical
!  level.
!
! !REVISION HISTORY:
!
!  06Dec2006  da Silva  Initial version.
!
!EOP
!------------------------------------------------------------------------------

   integer :: k

   do k = lbound(x,3), ubound(x,3)
      call pFIO_DownBit2D ( x(:,:,k), xr(:,:,k), nbits, &
                                 undef=undef, flops=flops, rc=rc )
   end do

   end subroutine pFIO_DownBit3D

   subroutine pFIO_DownBit2D ( x, xr, nbits, undef, flops, rc )

     implicit NONE

!
! !INPUT PARAMETERS:
!
     real, intent(in)    ::  x(:,:)         ! input array 
     integer, intent(in) :: nbits           ! number of bits per word to retain
     real, OPTIONAL, intent(in) :: undef    ! missing value
     logical, OPTIONAL, intent(in) :: flops ! if true, uses slower float point
                                            !  based algorithm
!
! !OUTPUT PARAMETERS:
!
     real, intent(out)   :: xr(:,:)   ! precision reduced array; can
!                                       !  share storage with input array
!                                       !  if it has same kind
     integer, intent(out)  :: rc        ! error code
                                        !  = 0 - all is well
                                        ! /= 0 - something went wrong 
!
! !DESCRIPTION:  
!
!
!  This routine returns a lower precision version of the input array
!  {\tt x} which retains {\tt nbits} of precision. Two algorithms are
!  implemented: 1) a fast one writen in C which downgrades precision
!  by shifting {\tt xbits = 24 - nbits} bits of the mantissa, and 2) a slower
!  float point based algorithm which is the same algorithm as GRIB 
!  with fixed number of bits packing. Notice that as in GRIB the scaling 
!  factor is forced to be a power of 2 rather than a generic float.  
!  Using this power of 2 binary scaling has the advantage of improving 
!  the GZIP compression rates.
!
!  This routine returns an array of the same type and kind as the input array, 
!  so no data compression has taken place. The goal here is to reduce the
!  entropy in the input array, thereby improving compression rates 
!  by the lossless algorithms implemented internally by HDF-4/5 when writing 
!  these data to a file. In fact, these GZIP'ed and pre-conditioned files 
!  have sizes comparable to the equivalent GRIB file, while being a bonafide 
!  self-describing HDF/NetCDF file.
!
! !TO DO:
!
!  Perhaps implement GRIB decimal scaling (variable number of bits).
!
! !REVISION HISTORY:
!
!  06Dec2006  da Silva  Initial version.
!
!EOP
!------------------------------------------------------------------------------
    integer   :: E, xbits, has_undef
    real    :: scale, xmin, xmax, tol, undef_
    logical   :: shave_mantissa
    integer, external :: pFIO_ShaveMantissa32

    rc = 0

!   Defaults for optinal arguments
!   ------------------------------
    if ( present(undef) ) then
         undef_ = undef
         has_undef = 1
    else
         undef_ = 1.0
         undef_ = huge(undef_)   ! why not?
         has_undef = 0
    endif
    if ( present(flops) ) then
         shave_mantissa = .not. flops
    else
         shave_mantissa = .true.
    endif

!   Fast, bit shifting in C
!   -----------------------
    if ( shave_mantissa ) then

       xr = x   ! compiled r8 this will convert to r4.
       xbits = 24 - nbits
       rc = pFIO_ShaveMantissa32 ( xr, xr, size(x), xbits, has_undef, undef_, size(x) )
       return

!   Slow, flops in FORTRAN (GRIB inspired)
!   --------------------------------------
    else

       if ( nbits < 1 ) then
          xr = x
          rc = 1
          return
       end if

       tol = 0.0001 * undef_
       xmin = minval(x,mask=(abs(undef_-x)>tol))
       xr = x - xmin     ! As in GRIB, force non-negative values 
       xmax = maxval(xr,mask=(abs(undef_-x)>tol)) ! max of positive

       if ( xmax <= 0.0 ) then
            xr = x
            rc = 0
            return  ! this means field is constant 
       end if

       E = nint(log(xmax)/log(2.)) - nbits ! GRIB binary scale factor
       scale = 2.**E                       ! GRIB requires power of 2

       if ( present(undef) ) then
          where ( abs(x - undef_) > tol )
             xr  = xmin + nint(xr/scale) * scale
          endwhere
       else
          xr  = xmin + nint(xr/scale) * scale
       end if

    end if

   end subroutine pFIO_DownBit2D

end module pFIO_DownbitMod
