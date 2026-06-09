!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!>
!### MODULE: `mapl_ArrayReductions`
!
! Author: GMAO SI-Team
!
! `mapl_ArrayReductions` --- Parallel collective reductions over arrays
!
! This module provides MPI-based global reduction operations over Fortran
! arrays:
!
!   - `MaxMin`: global parallel max and min of an array
!   - `AreaMean`: global parallel area-weighted mean of a 2-D array
!
! Previously these lived in the single-entity modules `MaxMin` and
! `AreaMean` (in `utils/arrays/`). They were consolidated here because
! both are MPI-dependent parallel reductions and belong in `MAPL.mp_utils`,
! and because single-public-symbol modules create naming conflicts under
! Intel Fortran's case-insensitive module namespace rules (#4944).

#include "MAPL.h"

module mapl_ArrayReductions_mod

   use mpi
   use, intrinsic :: iso_fortran_env, only: real32, real64

   use mapl_ErrorHandling_mod, only: MAPL_Verify, MAPL_Assert, MAPL_Return
   use MAPL_Constants, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64

   implicit none
   private

   public :: MaxMin
   public :: AreaMean

   !> Compute the global parallel max and min of a Fortran array.
   !! Returns a 2-element array [max, min].
   interface MaxMin
      module procedure pmaxmin1d_r4
      module procedure pmaxmin2d_r4
      module procedure pmaxmin3d_r4
      module procedure pmaxmin1d_r8
      module procedure pmaxmin2d_r8
      module procedure pmaxmin3d_r8
   end interface MaxMin

   !> Compute the global parallel area-weighted mean of a 2-D array.
   interface AreaMean
      module procedure AreaMean_2d_r8
   end interface AreaMean

contains

   ! ---------------------------------------------------------------------------
   ! MaxMin implementations
   ! ---------------------------------------------------------------------------

   function pmaxmin1d_r4(p, comm, rc) result(pmaxmin)
      real, intent(in) :: p(:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real :: pmaxmin(2) ! [pmax, pmin]

      integer :: im, jt, status

      im = size(p)
      jt = 1
      pmaxmin = pmaxmin2d_r4(reshape(p, [im, jt]), comm, _RC)

      _RETURN(_SUCCESS)
   end function pmaxmin1d_r4

   function pmaxmin2d_r4(p, comm, rc) result(pmaxmin)
      real, intent(in)  :: p(:,:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real :: pmaxmin(2) ! [pmax, pmin]

      real :: pm_send(2), pm_recv(2)
      integer, parameter :: TWO=2
      logical :: has_nans
      integer :: status

      has_nans = any(p /= p)
      _ASSERT(.not. has_nans, "input array contains NaNs")

      pm_send = [maxval(p), -minval(p)]
      call MPI_AllReduce(pm_send, pm_recv, TWO, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)
      pmaxmin = [pm_recv(1), -pm_recv(2)]

      _RETURN(_SUCCESS)
   end function pmaxmin2d_r4

   function pmaxmin3d_r4(p, comm, rc) result(pmaxmin)
      real, intent(in) :: p(:,:,:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real :: pmaxmin(2) ! [pmax, pmin]

      integer :: im, jt, status

      im = size(p, 1) * size(p,2)
      jt = size(p, 3)
      pmaxmin = pmaxmin2d_r4(reshape(p, [im, jt]), comm, _RC)

      _RETURN(_SUCCESS)
   end function pmaxmin3d_r4

   function pmaxmin1d_r8(p, comm, rc) result(pmaxmin)
      real(real64), intent(in) :: p(:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: pmaxmin(2) ! [pmax, pmin]

      real(real32) :: pmaxmin_r4(2)
      integer :: status

      pmaxmin_r4 = pmaxmin1d_r4(real(p, kind=real32), comm, _RC)
      pmaxmin = pmaxmin_r4

      _RETURN(_SUCCESS)
   end function pmaxmin1d_r8

   function pmaxmin2d_r8(p, comm, rc) result(pmaxmin)
      real(real64), intent(in) :: p(:,:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: pmaxmin(2) ! [pmax, pmin]

      real(real32) :: pmaxmin_r4(2)
      integer :: status

      pmaxmin_r4 = pmaxmin2d_r4(real(p, kind=real32), comm, _RC)
      pmaxmin = pmaxmin_r4

      _RETURN(_SUCCESS)
   end function pmaxmin2d_r8

   function pmaxmin3d_r8(p, comm, rc) result(pmaxmin)
      real(real64), intent(in) :: p(:,:,:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: pmaxmin(2) ! [pmax, pmin]

      real(real32) :: pmaxmin_r4(2)
      integer :: status

      pmaxmin_r4 = pmaxmin3d_r4(real(p, kind=real32), comm, _RC)
      pmaxmin = pmaxmin_r4

      _RETURN(_SUCCESS)
   end function pmaxmin3d_r8

   ! ---------------------------------------------------------------------------
   ! AreaMean implementation
   ! ---------------------------------------------------------------------------

   function AreaMean_2d_r8(q, area, comm, rc) result(qmean)
      real, intent(in) :: q(:,:)
      real, intent(in) :: area(:,:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: qmean

      real(kind=real64) :: sum_local(2), sum_global(2)
      integer, parameter :: TWO = 2
      integer :: im, jm, i, j, status

      _ASSERT(all(shape(q) == shape(area)), "q and area need to be of the same shape")

      im = size(area, 1)
      jm = size(area, 2)

      sum_local = 0.0d+0
      do j = 1, jm
         do i = 1, im
            if (q(i, j) == MAPL_UNDEFINED_REAL) cycle
            sum_local(1) = sum_local(1) + q(i, j) * area(i, j)
            sum_local(2) = sum_local(2) + area(i,j)
         enddo
      end do

      call MPI_AllReduce(sum_local, sum_global, TWO, MPI_DOUBLE, MPI_SUM, comm, status)
      _VERIFY(status)

      if (sum_global(2) /= 0.0d+0) then
         qmean = sum_global(1) / sum_global(2)
      else
         qmean = MAPL_UNDEFINED_REAL64
      end if

      _RETURN(_SUCCESS)
   end function AreaMean_2d_r8

end module mapl_ArrayReductions_mod
