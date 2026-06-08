#include "MAPL_Generic.h"

program create_stretched_cs_grid

   use ESMF
   use mpi
   use MAPL_BaseMod,               only: MAPL_GetHorzIJIndex, MAPL_GridGetCorners
   use mapl_MaplGrid,              only: MAPL_GridGet
   use MAPL_CubedSphereGridFactoryMod, only: CubedSphereGridFactory
   use mapl_ErrorHandling_mod
   use MAPL_Constants,             only: MAPL_DEGREES_TO_RADIANS_R8
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none

   ! Grid parameters
   integer,      parameter :: im_world       = 48
   real(REAL32), parameter :: stretch_factor = 2.1
   real(REAL32), parameter :: target_lat     = 44.0
   real(REAL32), parameter :: target_lon     = 263.0

   ! Number of query points
   integer, parameter :: npts = 5

   type(CubedSphereGridFactory)     :: factory
   type(ESMF_Grid)                  :: grid
   type(ESMF_VM)                    :: vm
   integer                          :: status, my_pet, n_pets
   integer                          :: nx, ny, k

   ! Grid coordinate pointers (for reporting local range)
   real(kind=ESMF_KIND_R8), pointer :: cell_lons(:,:) => null()
   real(kind=ESMF_KIND_R8), pointer :: cell_lats(:,:) => null()

   ! Corner arrays for the cache warm-up
   integer                   :: local_dims(3)
   real(REAL64), allocatable :: corner_lons(:,:), corner_lats(:,:)

   ! Five query lat/lon pairs (degrees), spread globally
   real(REAL64), parameter :: deg2rad = MAPL_DEGREES_TO_RADIANS_R8
   real(REAL64), parameter :: rad2deg = 1.0d0 / deg2rad

   real(REAL64), parameter :: query_lon_deg(npts) = &
        [ 263.0d0,  10.0d0, 120.0d0, 200.0d0, 310.0d0 ]
   real(REAL64), parameter :: query_lat_deg(npts) = &
        [  44.0d0,  60.0d0, -20.0d0,  30.0d0, -55.0d0 ]

   real(REAL64) :: query_lon_rad(npts), query_lat_rad(npts)
   integer      :: II(npts), JJ(npts)

   ! ----------------------------------------------------------------
   ! Initialize ESMF (also initializes MPI)
   ! ----------------------------------------------------------------
   call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, vm=vm, rc=status)
   if (status /= ESMF_SUCCESS) stop 'ESMF_Initialize failed'

   call ESMF_VMGet(vm, localPET=my_pet, petCount=n_pets, rc=status)
   if (status /= ESMF_SUCCESS) stop 'ESMF_VMGet failed'

   nx = 1
   ny = max(1, n_pets / 6)

   ! ----------------------------------------------------------------
   ! Create the stretched cubed-sphere grid
   ! ----------------------------------------------------------------
   if (my_pet == 0) then
      write(*, '(a)')         '============================================'
      write(*, '(a)')         ' Creating stretched cubed-sphere grid'
      write(*, '(a)')         '============================================'
      write(*, '(a,i0)')      '  im_world       = ', im_world
      write(*, '(a,f6.2)')    '  stretch_factor = ', stretch_factor
      write(*, '(a,f7.3)')    '  target_lat     = ', target_lat
      write(*, '(a,f7.3)')    '  target_lon     = ', target_lon
      write(*, '(a,i0,a,i0)') '  decomposition  = ', nx, ' x ', ny*6
      write(*, '(a)')         '--------------------------------------------'
   end if

   factory = CubedSphereGridFactory( &
        im_world       = im_world,       &
        nx             = nx,             &
        ny             = ny,             &
        stretch_factor = stretch_factor, &
        target_lat     = target_lat,     &
        target_lon     = target_lon,     &
        rc             = status)
   if (status /= ESMF_SUCCESS) stop 'CubedSphereGridFactory constructor failed'

   grid = factory%make_new_grid(rc=status)
   if (status /= ESMF_SUCCESS) stop 'make_new_grid failed'

   if (my_pet == 0) then
      write(*, '(a)')    '  Grid created successfully.'
      write(*, '(a,i0)') '  Total horizontal cells : ', 6 * im_world * im_world
      write(*, '(a)')    ''
   end if

   ! ----------------------------------------------------------------
   ! Report local coordinate range
   ! ----------------------------------------------------------------
   call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=cell_lons, rc=status)
   if (status /= ESMF_SUCCESS) stop 'ESMF_GridGetCoord (lons) failed'

   call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=cell_lats, rc=status)
   if (status /= ESMF_SUCCESS) stop 'ESMF_GridGetCoord (lats) failed'

   if (my_pet == 0) then
      write(*, '(a)') '  PET 0 local domain (degrees):'
      write(*, '(a,2(f9.3,1x))') '    lon range : ', &
           minval(cell_lons)*rad2deg, maxval(cell_lons)*rad2deg
      write(*, '(a,2(f9.3,1x))') '    lat range : ', &
           minval(cell_lats)*rad2deg, maxval(cell_lats)*rad2deg
      write(*, '(a)') '============================================'
      write(*, '(a)') ''
   end if

   ! ----------------------------------------------------------------
   ! Warm the corner-coordinate cache required by MAPL_GetHorzIJIndex.
   ! MAPL_GridGetCorners stores R8 corner lons/lats as grid attributes
   ! the first time it is called; the internal grid validator in
   ! MAPL_GetGlobalHorzIJIndex reads those cached attributes.
   ! ----------------------------------------------------------------
   call MAPL_GridGet(grid, localCellCountPerDim=local_dims, rc=status)
   if (status /= ESMF_SUCCESS) stop 'MAPL_GridGet failed'

   allocate(corner_lons(local_dims(1)+1, local_dims(2)+1))
   allocate(corner_lats(local_dims(1)+1, local_dims(2)+1))

   call MAPL_GridGetCorners(grid, corner_lons, corner_lats, rc=status)
   if (status /= ESMF_SUCCESS) stop 'MAPL_GridGetCorners failed'

   deallocate(corner_lons, corner_lats)

   ! ----------------------------------------------------------------
   ! Convert query points to radians and call MAPL_GetHorzIJIndex
   ! ----------------------------------------------------------------
   query_lon_rad = query_lon_deg * deg2rad
   query_lat_rad = query_lat_deg * deg2rad

   II = -1
   JJ = -1

   call MAPL_GetHorzIJIndex(npts, II, JJ, &
        lonR8=query_lon_rad, latR8=query_lat_rad, &
        Grid=grid, rc=status)
   if (status /= ESMF_SUCCESS) stop 'MAPL_GetHorzIJIndex failed'

   ! ----------------------------------------------------------------
   ! Each PET prints only the points that fall in its local domain.
   ! Points with II=-1 are outside this PET's subdomain (handled by
   ! another PET); -1 on ALL PETs would mean the point is off-grid.
   ! ----------------------------------------------------------------
   call ESMF_VMBarrier(vm, rc=status)

   do k = 1, npts
      if (II(k) /= -1) then
         write(*, '(a,i0,a,f8.3,a,f7.3,a,i4,a,i4,a,i0,a)') &
              '  point ', k, &
              ' : lon=', query_lon_deg(k), &
              '  lat=', query_lat_deg(k), &
              '  =>  I=', II(k), '  J=', JJ(k), &
              '  (PET ', my_pet, ')'
      end if
   end do

   call ESMF_VMBarrier(vm, rc=status)

   ! Use a global reduction to detect truly missing points
   block
      integer :: global_II(npts), ierr
      call MPI_Allreduce(II, global_II, npts, MPI_INTEGER, MPI_MAX, &
                         MPI_COMM_WORLD, ierr)
      if (my_pet == 0) then
         write(*, '(a)') ''
         do k = 1, npts
            if (global_II(k) == -1) then
               write(*, '(a,i0,a,f8.3,a,f7.3,a)') &
                    '  WARNING: point ', k, &
                    ' : lon=', query_lon_deg(k), &
                    '  lat=', query_lat_deg(k), &
                    '  =>  not found on any PET'
            end if
         end do
      end if
   end block

   ! ----------------------------------------------------------------
   ! Clean up
   ! ----------------------------------------------------------------
   call ESMF_GridDestroy(grid, rc=status)
   if (status /= ESMF_SUCCESS) stop 'ESMF_GridDestroy failed'

   call ESMF_Finalize(rc=status)

end program create_stretched_cs_grid
