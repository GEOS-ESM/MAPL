#include "MAPL_Generic.h"
module MAPL_DownbitMod
   use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc, c_ptr
   use mpi
   use MAPL_ExceptionHandling

   implicit none
   private

   public :: DownBit

   interface DownBit
      module procedure DownBit1D
      module procedure DownBit2D
      module procedure DownBit3D
   end interface DownBit

contains

!--------------------------------------------------------------------------
!>
! The routine `DownBit3D` returns a lower precision version of the input array
! `x` which retains `nbits_to_keep` of precision. 
! See routine `ESMF_CFIODownBit2D` or additional details. This version for
! rank 3 arrays, calls `ESMF_CFIODownBit2D()` for each vertical level.
!
!### History
!- 06Dec2006  da Silva  Initial version.
!
   subroutine DownBit3D ( x, xr, nbits_to_keep, undef, flops, mpi_comm, rc )

     implicit NONE

!
! !INPUT PARAMETERS:
!
     real, intent(in)    ::  x(:,:,:)       !! input array
     integer, intent(in) :: nbits_to_keep   !! number of bits per word to retain
                                            !! - no action if nbits_to_keep<1
     real, OPTIONAL, intent(in) :: undef    !! missing value
     logical, OPTIONAL, intent(in) :: flops !! if true, uses slower float point
                                            !!  based algorithm
     integer, optional, intent(in) :: mpi_comm
!
! !OUTPUT PARAMETERS:
!
     real, intent(out)   :: xr(:,:,:)       !! precision reduced array; can
                                            !! share storage with input array
                                            !! if it has same kind
     integer, optional, intent(out)  :: rc  !! error code
                                            !!  = 0 - all is well
                                            !! /= 0 - something went wrong
!
!------------------------------------------------------------------------------

   integer :: k

   do k = lbound(x,3), ubound(x,3)
      call DownBit2D ( x(:,:,k), xr(:,:,k), nbits_to_keep, &
                                 undef=undef, flops=flops, mpi_comm=mpi_comm, rc=rc )
   end do

   end subroutine DownBit3D

!---------------------------------------------------------------------------
!>
! This routine returns a lower precision version of the input array
! `x` which retains `nbits_to_keep` of precision. Two algorithms are
! implemented: 1) a fast one writen in C which downgrades precision
! by shifting `xbits = 24 - nbits_to_keep` bits of the mantissa, and 2) a slower
! float point based algorithm which is the same algorithm as GRIB
! with fixed number of bits packing. Notice that as in GRIB the scaling
! factor is forced to be a power of 2 rather than a generic float.
! Using this power of 2 binary scaling has the advantage of improving
! the GZIP compression rates.
!
! This routine returns an array of the same type and kind as the input array,
! so no data compression has taken place. The goal here is to reduce the
! entropy in the input array, thereby improving compression rates
! by the lossless algorithms implemented internally by HDF-4/5 when writing
! these data to a file. In fact, these GZIP'ed and pre-conditioned files
! have sizes comparable to the equivalent GRIB file, while being a bonafide
! self-describing HDF/NetCDF file.
!
! @todo
! Perhaps implement GRIB decimal scaling (variable number of bits).
!@endtodo
!
!#### History
!- 06Dec2006  da Silva  Initial version.
! 
   subroutine DownBit2D ( x, xr, nbits_to_keep, undef, flops, mpi_comm, rc )

     implicit NONE

!
! !INPUT PARAMETERS:
!
     real, intent(in)    ::  x(:,:)         !! input array
     integer, intent(in) :: nbits_to_keep   !! number of bits per word to retain
     real, OPTIONAL, intent(in) :: undef    !! missing value
     logical, OPTIONAL, intent(in) :: flops !! if true, uses slower float point
                                            !!  based algorithm
     integer, optional, intent(in) :: mpi_comm
!
! !OUTPUT PARAMETERS:
!
     real, intent(out)   :: xr(:,:)          !! precision reduced array; can
                                             !!  share storage with input array
                                             !!  if it has same kind
     integer, optional, intent(out)  :: rc   !! error code
                                             !!  = 0 - all is well
                                             !! /= 0 - something went wrong
!
!------------------------------------------------------------------------------
    integer   :: E, xbits, has_undef, passed_minmax
    real    :: scale, xmin, xmax, tol, undef_
    logical   :: shave_mantissa
    integer, external :: MAPL_ShaveMantissa32
    real :: min_value, max_value
    integer :: useable_mpi_comm,status

    rc = 0

    if (present(mpi_comm)) then
       useable_mpi_comm = mpi_comm
    else
       useable_mpi_comm = MPI_COMM_NULL
    end if
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
       xbits = 24 - nbits_to_keep
       call compute_min_max(xr,min_value,max_value,undef_,useable_mpi_comm,_rc)
       if (useable_mpi_comm/=MPI_COMM_NULL) passed_minmax = 1
       rc = MAPL_ShaveMantissa32 ( xr, xr, size(x), xbits, has_undef, undef_, size(x), passed_minmax, min_value, max_value )
       return

!   Slow, flops in FORTRAN (GRIB inspired)
!   --------------------------------------
    else

       if ( nbits_to_keep < 1 ) then
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

       E = nint(log(xmax)/log(2.)) - nbits_to_keep ! GRIB binary scale factor
       scale = 2.**E                       ! GRIB requires power of 2

       if ( present(undef) ) then
          where ( abs(x - undef_) > tol )
             xr  = xmin + nint(xr/scale) * scale
          endwhere
       else
          xr  = xmin + nint(xr/scale) * scale
       end if

    end if

   end subroutine DownBit2D

   subroutine DownBit1D ( x, xr, nbits_to_keep, undef, flops, mpi_comm, rc )
     implicit NONE
!
! !INPUT PARAMETERS:
!
     real,target, intent(in)    ::  x(:)         ! input array
     integer, intent(in) :: nbits_to_keep           ! number of bits per word to retain
     real, OPTIONAL, intent(in) :: undef    ! missing value
     logical, OPTIONAL, intent(in) :: flops ! if true, uses slower float point
                                            !  based algorithm
     integer, optional, intent(in) :: mpi_comm
!
! !OUTPUT PARAMETERS:
!
     real, target, intent(inout)   :: xr(:)   ! precision reduced array; can
     integer, optional, intent(out) :: rc


     real, pointer :: x_tmp(:,:)
     real, pointer :: xr_tmp(:,:)
     type(c_ptr) :: x_ptr, xr_ptr

     x_ptr = c_loc(x(1))
     call c_f_pointer(x_ptr,  x_tmp,[size(x),1])
     xr_ptr = c_loc(xr(1))
     call c_f_pointer(xr_ptr, xr_tmp,[size(x),1])

     call Downbit2d(x_tmp(:,:), xr_tmp(:,:), nbits_to_keep, undef=undef, flops=flops, mpi_comm=mpi_comm, rc=rc)

   end subroutine Downbit1D

   subroutine compute_min_max(array,min_value,max_value,undef_value,mpi_comm,rc)
      real, intent(in)  :: array(:,:)
      real, intent(out) :: min_value
      real, intent(out) :: max_value
      real, intent(in ) :: undef_value
      integer, intent(in ) :: mpi_comm
      integer, optional, intent(out) :: rc

      real :: local_min_value, local_max_value
      logical, allocatable :: undef_mask(:,:)
      integer :: status

      allocate(undef_mask(size(array,1),size(array,2)))
      undef_mask = .false.
      where(array /= undef_value) undef_mask = .true.

      local_min_value = minval(array,mask=undef_mask)
      local_max_value = maxval(array,mask=undef_mask)
      if (mpi_comm /= MPI_COMM_NULL) then
         call MPI_AllReduce(local_min_value,min_value,1,MPI_FLOAT,MPI_MIN,mpi_comm,status)
         _verify(status)
         call MPI_AllReduce(local_max_value,max_value,1,MPI_FLOAT,MPI_MAX,mpi_comm,status)
         _verify(status)
      else
         min_value = local_min_value
         max_value = local_max_value
      end if
      _return(_success)
   end subroutine 

end module MAPL_DownbitMod
