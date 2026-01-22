!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!>
!### MODULE: `mapl3g_MaxMin`
!
! Author: GMAO SI-Team
!
! `mapl3g_MaxMin` --- Global Max/Min of Arrays
!
! This module implements functions for calculating/printing out the global min/max
! of fortran arrays. Derived from GEOS-4 pmaxmin() functions.

#include "MAPL.h"

module mapl3g_MaxMin

   use mpi
   use, intrinsic :: iso_fortran_env, only: real32, real64

   use MAPL_ErrorHandling, only: MAPL_Verify, MAPL_Assert, MAPL_Return

   implicit none
   private

   public ::  MaxMin

   interface MaxMin
      ! real32
      module procedure pmaxmin1d_r4
      module procedure pmaxmin2d_r4
      module procedure pmaxmin3d_r4
      ! real64
      module procedure pmaxmin1d_r8
      module procedure pmaxmin2d_r8
      module procedure pmaxmin3d_r8
   end interface MaxMin

contains

   function pmaxmin1d_r4(p, comm, rc) result(pmaxmin)
      real, intent(in) :: p(:) ! input array
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real :: pmaxmin(2) !  [pmax, pmin]

      integer :: im, jt, status

      im = size(p)
      jt = 1
      pmaxmin = pmaxmin2d_r4(reshape(p, [im, jt]), comm, _RC)

      _RETURN(_SUCCESS)
   end function pmaxmin1d_r4

   function pmaxmin2d_r4(p, comm, rc) result(pmaxmin)
      real, intent(in)  :: p(:,:) ! input array
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real :: pmaxmin(2) ! [pmax, pmin]

      real :: pmax, pmin, pm_send(2), pm_recv(2)
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
      real, intent(in) :: p(:,:,:) ! input array
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
      real(real64), intent(in) :: p(:) ! input array
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: pmaxmin(2) ! [pmax, pmin]

      real(real32) :: pmaxmin_r4(2)
      integer :: status

      pmaxmin_r4 =  pmaxmin1d_r4(real(p, kind=real32), comm, _RC)
      pmaxmin = pmaxmin_r4

      _RETURN(_SUCCESS)
   end function pmaxmin1d_r8

   function pmaxmin2d_r8(p, comm, rc) result(pmaxmin)
      real(real64), intent(in) :: p(:,:) ! input array
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
      real(real64), intent(in) :: p(:,:,:) ! input array
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: pmaxmin(2) ! [pmax, pmin]

      real(real32) :: pmaxmin_r4(2)
      integer :: status

      pmaxmin_r4 =  pmaxmin3d_r4(real(p, kind=real32), comm, _RC)
      pmaxmin = pmaxmin_r4

      _RETURN(_SUCCESS)
   end function pmaxmin3d_r8

end module mapl3g_MaxMin
