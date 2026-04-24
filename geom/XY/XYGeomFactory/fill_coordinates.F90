#include "MAPL.h"

! Fill lon/lat centre (and optionally corner) coordinates for a standard
! XY grid whose coordinate arrays are stored as 2-D variables ('lons',
! 'lats', 'corner_lons', 'corner_lats') in a NetCDF file.
!
! Shmem optimisation is intentionally omitted here (see issue #4685).
! Root reads the full global arrays, then broadcasts to all PETs via
! MPI_Bcast (ESMF_VMBroadcast has no R8 or 2-D interface), and each
! PET copies its local tile into the ESMF coordinate arrays.

submodule (mapl3g_XYGeomFactory) fill_coordinates_smod
   use mapl_ErrorHandlingMod
   use mapl_InternalConstants, only: MAPL_UNDEFINED_REAL64
   use mapl_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
   use mapl3g_Comms,   only: am_i_root, ROOT_PROCESS_ID
   use NetCDF
   use esmf
   use mpi
   implicit none

contains

   module subroutine fill_coordinates(spec, grid, unusable, rc)
      type(XYGeomSpec), intent(in) :: spec
      type(ESMF_Grid),  intent(inout) :: grid
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im_world, jm_world
      integer :: i1, in, j1, jn, ic1, icn, jc1, jcn
      integer :: ncid, varid
      integer :: comm
      real(ESMF_KIND_R8), pointer     :: fptr(:,:)
      real(ESMF_KIND_R8), allocatable :: centers(:,:), corners(:,:)
      type(ESMF_VM) :: vm

      im_world = spec%get_im_world()
      jm_world = spec%get_jm_world()

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)

      ! ---- centres ----
      allocate(centers(im_world, jm_world))

      call get_interior_bounds(grid, i1, in, j1, jn, _RC)
      ! corner bounds: last PE row owns the extra corner row
      ic1 = i1 ;  icn = in
      jc1 = j1
      if (jn == jm_world) then
         jcn = jn + 1
      else
         jcn = jn
      end if

      ! longitudes
      if (am_i_root()) then
         status = nf90_open(spec%get_grid_file_name(), NF90_NOWRITE, ncid)
         _VERIFY(status)
         status = nf90_inq_varid(ncid, 'lons', varid) ; _VERIFY(status)
         status = nf90_get_var(ncid, varid, centers)  ; _VERIFY(status)
         where (centers /= MAPL_UNDEFINED_REAL64) &
              centers = centers * MAPL_DEGREES_TO_RADIANS_R8
      end if
      call MPI_Bcast(centers, im_world*jm_world, MPI_DOUBLE_PRECISION, &
           ROOT_PROCESS_ID, comm, status)
      _VERIFY(status)

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr, _RC)
      fptr = centers(i1:in, j1:jn)

      ! latitudes
      if (am_i_root()) then
         status = nf90_inq_varid(ncid, 'lats', varid) ; _VERIFY(status)
         status = nf90_get_var(ncid, varid, centers)  ; _VERIFY(status)
         where (centers /= MAPL_UNDEFINED_REAL64) &
              centers = centers * MAPL_DEGREES_TO_RADIANS_R8
      end if
      call MPI_Bcast(centers, im_world*jm_world, MPI_DOUBLE_PRECISION, &
           ROOT_PROCESS_ID, comm, status)
      _VERIFY(status)

      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr, _RC)
      fptr = centers(i1:in, j1:jn)

      deallocate(centers)

      ! ---- corners (optional) ----
      if (spec%get_has_corners()) then
         allocate(corners(im_world+1, jm_world+1))

         ! corner longitudes
         if (am_i_root()) then
            status = nf90_inq_varid(ncid, 'corner_lons', varid) ; _VERIFY(status)
            status = nf90_get_var(ncid, varid, corners)         ; _VERIFY(status)
            where (corners /= MAPL_UNDEFINED_REAL64) &
                 corners = corners * MAPL_DEGREES_TO_RADIANS_R8
         end if
         call MPI_Bcast(corners, (im_world+1)*(jm_world+1), MPI_DOUBLE_PRECISION, &
              ROOT_PROCESS_ID, comm, status)
         _VERIFY(status)

         call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=fptr, _RC)
         fptr = corners(ic1:icn, jc1:jcn)

         ! corner latitudes
         if (am_i_root()) then
            status = nf90_inq_varid(ncid, 'corner_lats', varid) ; _VERIFY(status)
            status = nf90_get_var(ncid, varid, corners)         ; _VERIFY(status)
            where (corners /= MAPL_UNDEFINED_REAL64) &
                 corners = corners * MAPL_DEGREES_TO_RADIANS_R8
         end if
         call MPI_Bcast(corners, (im_world+1)*(jm_world+1), MPI_DOUBLE_PRECISION, &
              ROOT_PROCESS_ID, comm, status)
         _VERIFY(status)

         call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=fptr, _RC)
         fptr = corners(ic1:icn, jc1:jcn)

         deallocate(corners)
      end if

      if (am_i_root()) then
         status = nf90_close(ncid) ; _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine fill_coordinates

   ! Helper: get local interior bounds from the ESMF grid
   subroutine get_interior_bounds(grid, i1, in, j1, jn, rc)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out) :: i1, in, j1, jn
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: cLBound(2), cUBound(2)

      call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
           exclusiveLBound=cLBound, exclusiveUBound=cUBound, _RC)
      i1 = cLBound(1) ;  in = cUBound(1)
      j1 = cLBound(2) ;  jn = cUBound(2)

      _RETURN(_SUCCESS)
   end subroutine get_interior_bounds

end submodule fill_coordinates_smod
