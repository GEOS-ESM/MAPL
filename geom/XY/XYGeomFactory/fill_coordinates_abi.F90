#include "MAPL.h"

! Fill lon/lat centre coordinates for an ABI/GOES-style XY grid.
! The file contains 1-D scan-angle arrays (x, y) and a satellite
! sub-longitude attribute.  Each PET computes its own tile of
! (lon, lat) from those inputs using the standard ABI fixed-grid
! projection (GOES-R series product definition).
!
! Shmem optimisation is intentionally omitted (see issue #4685).

submodule (mapl3g_XYGeomFactory) fill_coordinates_abi_smod
   use mapl_ErrorHandlingMod
   use mapl_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
   use mapl3g_Comms,   only: am_i_root, ROOT_PROCESS_ID
   use MAPL_EarthConstants, only: R_EQ  => MAPL_SEMIMAJOR_AXIS, &
                                  R_POL => MAPL_SEMIMINOR_AXIS, &
                                  H_SAT => MAPL_GEO_ORBIT_RADIUS
   use NetCDF
   use esmf
   use mpi
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none (type, external)

   real(REAL64), parameter :: E2 = 1.0_REAL64 - (R_POL/R_EQ)**2

contains

   module subroutine fill_coordinates_abi(spec, grid, unusable, rc)
      type(XYGeomSpec), intent(in) :: spec
      type(ESMF_Grid),  intent(inout) :: grid
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: xdim_true, ydim_true, thin_factor
      integer :: i1, in, j1, jn, i, j, ix, jx
      integer :: ncid, varid, comm
      integer :: cLBound(2), cUBound(2)
      ! ESMF_VMBroadcast has no REAL32/REAL64 array interface; use MPI_Bcast.
      real(REAL64), allocatable :: x_scan(:), y_scan(:), lambda0(:)
      real(REAL64) :: lambda0_deg, x0, y0, lam_sat
      real(REAL64), pointer :: fptr_x(:,:), fptr_y(:,:)
      character(len=:), allocatable :: fn, key_x, key_y, key_p, key_p_att
      type(ESMF_VM) :: vm

      xdim_true  = spec%get_xdim_true()
      ydim_true  = spec%get_ydim_true()
      thin_factor = spec%get_thin_factor()
      fn         = spec%get_grid_file_name()
      key_x      = spec%get_var_name_x()
      key_y      = spec%get_var_name_y()
      key_p      = spec%get_var_name_proj()
      key_p_att  = spec%get_att_name_proj()

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)

      allocate(x_scan(xdim_true), y_scan(ydim_true), lambda0(1))

      if (am_i_root()) then
         status = nf90_open(fn, NF90_NOWRITE, ncid) ; _VERIFY(status)

         status = nf90_inq_varid(ncid, key_x, varid) ; _VERIFY(status)
         status = nf90_get_var(ncid, varid, x_scan)  ; _VERIFY(status)

         status = nf90_inq_varid(ncid, key_y, varid) ; _VERIFY(status)
         status = nf90_get_var(ncid, varid, y_scan)  ; _VERIFY(status)

         status = nf90_inq_varid(ncid, key_p, varid) ; _VERIFY(status)
          status = nf90_get_att(ncid, varid, key_p_att, lambda0_deg) ; _VERIFY(status)
          lambda0(1) = lambda0_deg * MAPL_DEGREES_TO_RADIANS_R8

         status = nf90_close(ncid) ; _VERIFY(status)
      end if

      call MPI_Bcast(x_scan,  xdim_true, MPI_DOUBLE_PRECISION, ROOT_PROCESS_ID, comm, status) ; _VERIFY(status)
      call MPI_Bcast(y_scan,  ydim_true, MPI_DOUBLE_PRECISION, ROOT_PROCESS_ID, comm, status) ; _VERIFY(status)
      call MPI_Bcast(lambda0, 1,         MPI_DOUBLE_PRECISION, ROOT_PROCESS_ID, comm, status) ; _VERIFY(status)
      lam_sat = lambda0(1)

      ! Get local tile bounds
      call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
           exclusiveLBound=cLBound, exclusiveUBound=cUBound, _RC)
      i1 = cLBound(1) ;  in = cUBound(1)
      j1 = cLBound(2) ;  jn = cUBound(2)

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr_x, _RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr_y, _RC)

      do i = i1, in
         ix = i - i1 + 1
         do j = j1, jn
            jx = j - j1 + 1
             x0 = x_scan(i * thin_factor)
             y0 = y_scan(j * thin_factor)
            call abi_xy_to_lonlat(x0, y0, lam_sat, fptr_x(ix,jx), fptr_y(ix,jx))
         end do
      end do

      deallocate(x_scan, y_scan, lambda0)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine fill_coordinates_abi

   ! Standard ABI fixed-grid projection inverse (scan angle -> geodetic lon/lat).
   ! Reference: GOES-R Series Product Definition and Users' Guide (PUG), Vol. 3.
   pure subroutine abi_xy_to_lonlat(x, y, lambda0, lon, lat)
      real(REAL64), intent(in)  :: x, y, lambda0   ! rad
      real(REAL64), intent(out) :: lon, lat          ! rad

      real(REAL64) :: a, b, c, rs, Sx, Sy, Sz

      a  = sin(x)**2 + cos(x)**2 * (cos(y)**2 + (R_EQ/R_POL)**2 * sin(y)**2)
      b  = -2.0_REAL64 * H_SAT * cos(x) * cos(y)
      c  = H_SAT**2 - R_EQ**2

      if (b**2 - 4.0_REAL64*a*c < 0.0_REAL64) then
         lon = huge(1.0_REAL64)
         lat = huge(1.0_REAL64)
         return
      end if

      rs = (-b - sqrt(b**2 - 4.0_REAL64*a*c)) / (2.0_REAL64*a)
      Sx = rs * cos(x) * cos(y)  - H_SAT
      Sy = -rs * sin(x)
      Sz = rs * cos(x) * sin(y)

      lat = atan((R_EQ/R_POL)**2 * Sz / sqrt((-Sx)**2 + Sy**2))
      lon = lambda0 - atan(Sy / (-Sx))
   end subroutine abi_xy_to_lonlat

end submodule fill_coordinates_abi_smod
