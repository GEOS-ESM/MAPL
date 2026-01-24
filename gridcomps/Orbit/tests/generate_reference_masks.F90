program generate_reference_masks
   use ESMF
   use MAPL_NominalOrbitsMod, only: orbits_track, orbits_swath
   use MAPL_OrbGridCompMod, only: orb_mask_lonlat, orb_swath_mask_lonlat
   implicit none
   
   integer, parameter :: dp = kind(1.0d0)
   integer :: rc, localPet, petCount
   type(ESMF_VM) :: vm
   
   ! Initialize ESMF
   call ESMF_Initialize(vm=vm, defaultlogfilename='generate_ref.log', &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
   if (rc /= ESMF_SUCCESS) stop 'ESMF_Initialize failed'
   
   call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
   if (rc /= ESMF_SUCCESS) stop 'ESMF_VMGet failed'
   
   ! Generate reference files based on PE count
   if (petCount == 1) then
      if (localPet == 0) print *, 'Generating lat-lon reference files (1 PE)...'
      call generate_latlon_8x6_equatorial()
      call generate_latlon_8x6_diagonal()
      call generate_latlon_12x8_equatorial()
      call generate_latlon_12x8_diagonal()
      call generate_latlon_180x91_equatorial_swath()
      call generate_latlon_180x91_diagonal_swath()
      if (localPet == 0) print *, 'Lat-lon reference files generated.'
   else if (petCount == 6) then
      if (localPet == 0) print *, 'Generating cubed-sphere reference files (6 PEs)...'
      call generate_cs_c12_equatorial()
      call generate_cs_c12_diagonal()
      call generate_cs_c24_equatorial()
      call generate_cs_c24_diagonal()
      call generate_cs_c48_equatorial_swath()
      call generate_cs_c48_diagonal_swath()
      if (localPet == 0) print *, 'Cubed-sphere reference files generated.'
   else
      if (localPet == 0) then
         print *, 'Error: This program must be run with 1 PE (for lat-lon) or 6 PEs (for cubed-sphere)'
      end if
   end if
   
   ! Finalize ESMF
   call ESMF_Finalize(rc=rc)
   
contains

   !---------------------------------------------------------------------------
   ! Generic: Capture reference mask for any ESMF grid
   !---------------------------------------------------------------------------
   subroutine capture_reference_mask(grid, orbit_name, orbit_dates, orbit_times, &
                                      orbit_dt, filename)
      use mpi
      type(ESMF_Grid), intent(in) :: grid
      character(len=*), intent(in) :: orbit_name
      integer, intent(in) :: orbit_dates(2), orbit_times(2), orbit_dt
      character(len=*), intent(in) :: filename
      
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer(ESMF_KIND_I4), pointer :: mask(:,:)
      integer(ESMF_KIND_I4), allocatable :: global_mask(:,:,:)
      real(dp), pointer :: tlons(:), tlats(:)
      integer :: status, im, jm, ntiles, localPet, petCount
      integer :: lb(2), ub(2)
      character(len=32) :: grid_type
      type(ESMF_VM) :: vm
      
      ! Get VM info
      call ESMF_VMGetCurrent(vm, rc=status)
      if (status /= 0) return
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=status)
      if (status /= 0) return
      
      ! Get grid properties
      call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if (status /= 0) return
      
      call ESMF_GridGet(grid, tileCount=ntiles, rc=status)
      if (status /= 0) return
      
      im = ub(1) - lb(1) + 1
      jm = ub(2) - lb(2) + 1
      
      ! Get coordinate and mask pointers
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=status)
      if (status /= 0) return
      
      ! Get orbit
      call orbits_track(tlons, tlats, orbit_name, orbit_dates, orbit_times, orbit_dt, status)
      if (status /= 0) return
      
      ! Compute mask (convert R8 coords to R4)
      mask = 0
      call orb_mask_lonlat(mask, im, jm, real(lons, ESMF_KIND_R4), real(lats, ESMF_KIND_R4), &
                           tlons, tlats, size(tlons), 10, -180.0, 180.0)
      
      ! Determine grid type
      if (ntiles == 1) then
         grid_type = 'lat-lon'
      else if (ntiles == 6) then
         grid_type = 'cubed-sphere'
      else
         grid_type = 'unknown'
      end if
      
      ! For CS grids with multiple PEs, gather all tiles to PE 0
      if (ntiles == 6 .and. petCount == 6) then
         if (localPet == 0) then
            allocate(global_mask(im, jm, 6))
         else
            allocate(global_mask(1, 1, 1))  ! Dummy allocation for non-root PEs
         end if
         
         ! Gather data to PE 0
         call MPI_Gather(mask, im*jm, MPI_INTEGER, global_mask, im*jm, MPI_INTEGER, &
                        0, MPI_COMM_WORLD, status)
         
         ! Only PE 0 writes the file
         if (localPet == 0) then
            call write_reference_mask(filename, im, jm, ntiles, global_mask, &
                                      grid_type, orbit_name, petCount)
         end if
         
         deallocate(global_mask)
      else
         ! For lat-lon grids, only PE 0 writes
         if (localPet == 0) then
            allocate(global_mask(im, jm, 1))
            global_mask(:,:,1) = mask(:,:)
            call write_reference_mask(filename, im, jm, ntiles, global_mask, &
                                      grid_type, orbit_name, petCount)
            deallocate(global_mask)
         end if
      end if
   end subroutine capture_reference_mask

   !---------------------------------------------------------------------------
   ! Write reference mask to file
   !---------------------------------------------------------------------------
   subroutine write_reference_mask(filename, im, jm, ntiles, mask, &
                                    grid_type, orbit_name, npes)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: im, jm, ntiles
      integer(ESMF_KIND_I4), intent(in) :: mask(:,:,:)
      character(len=*), intent(in) :: grid_type, orbit_name
      integer, intent(in) :: npes
      
      integer :: unit, i, j, tile, status
      character(len=19) :: timestamp
      
      open(newunit=unit, file=filename, status='replace', action='write', iostat=status)
      if (status /= 0) then
         print *, 'Error opening file: ', trim(filename)
         return
      end if
      
      ! Write metadata as comments
      call date_and_time(date=timestamp(1:8), time=timestamp(10:19))
      timestamp(9:9) = ' '
      write(unit, '(A)') '# MAPL OrbGridComp Regression Test Reference Data'
      write(unit, '(A,A)') '# Grid type: ', trim(grid_type)
      if (ntiles == 1) then
         write(unit, '(A,I0,A,I0)') '# Resolution: ', im, 'x', jm
      else
         write(unit, '(A,I0)') '# Resolution: C', im
      end if
      write(unit, '(A,A)') '# Orbit: ', trim(orbit_name)
      write(unit, '(A,I0)') '# PEs: ', npes
      write(unit, '(A,A)') '# Generated: ', timestamp
      write(unit, '(A)') '#'
      
      ! Write mask data - for CS, write all 6 tiles
      do tile = 1, size(mask, 3)
         do j = 1, jm
            write(unit, '(*(I0,1X))') (mask(i, j, tile), i=1, im)
         end do
      end do
      
      close(unit)
   end subroutine write_reference_mask

   !---------------------------------------------------------------------------
   ! Generate lat-lon 8x6 equatorial reference
   !---------------------------------------------------------------------------
   subroutine generate_latlon_8x6_equatorial()
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer :: status, i, j
      integer, parameter :: IM = 8, JM = 6
      
      grid = ESMF_GridCreate1PeriDim(maxIndex=[IM, JM], &
         regDecomp=[1,1], coordSys=ESMF_COORDSYS_SPH_DEG, &
         indexflag=ESMF_INDEX_GLOBAL, rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      do j = 1, JM
         do i = 1, IM
            lons(i,j) = -135.0_ESMF_KIND_R8 + (i-1) * 45.0_ESMF_KIND_R8
            lats(i,j) = -50.0_ESMF_KIND_R8 + (j-1) * 20.0_ESMF_KIND_R8
         end do
      end do
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'EQUATORIAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_latlon_8x6_equatorial_1pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_latlon_8x6_equatorial

   !---------------------------------------------------------------------------
   ! Generate lat-lon 8x6 diagonal reference
   !---------------------------------------------------------------------------
   subroutine generate_latlon_8x6_diagonal()
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer :: status, i, j
      integer, parameter :: IM = 8, JM = 6
      
      grid = ESMF_GridCreate1PeriDim(maxIndex=[IM, JM], &
         regDecomp=[1,1], coordSys=ESMF_COORDSYS_SPH_DEG, &
         indexflag=ESMF_INDEX_GLOBAL, rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      do j = 1, JM
         do i = 1, IM
            lons(i,j) = -135.0_ESMF_KIND_R8 + (i-1) * 45.0_ESMF_KIND_R8
            lats(i,j) = -50.0_ESMF_KIND_R8 + (j-1) * 20.0_ESMF_KIND_R8
         end do
      end do
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'DIAGONAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_latlon_8x6_diagonal_1pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_latlon_8x6_diagonal

   !---------------------------------------------------------------------------
   ! Generate lat-lon 12x8 equatorial reference
   !---------------------------------------------------------------------------
   subroutine generate_latlon_12x8_equatorial()
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer :: status, i, j
      integer, parameter :: IM = 12, JM = 8
      
      grid = ESMF_GridCreate1PeriDim(maxIndex=[IM, JM], &
         regDecomp=[1,1], coordSys=ESMF_COORDSYS_SPH_DEG, &
         indexflag=ESMF_INDEX_GLOBAL, rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      do j = 1, JM
         do i = 1, IM
            lons(i,j) = -180.0_ESMF_KIND_R8 + (i-1) * 30.0_ESMF_KIND_R8
            lats(i,j) = -52.5_ESMF_KIND_R8 + (j-1) * 15.0_ESMF_KIND_R8
         end do
      end do
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'EQUATORIAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_latlon_12x8_equatorial_1pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_latlon_12x8_equatorial

   !---------------------------------------------------------------------------
   ! Generate lat-lon 12x8 diagonal reference
   !---------------------------------------------------------------------------
   subroutine generate_latlon_12x8_diagonal()
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer :: status, i, j
      integer, parameter :: IM = 12, JM = 8
      
      grid = ESMF_GridCreate1PeriDim(maxIndex=[IM, JM], &
         regDecomp=[1,1], coordSys=ESMF_COORDSYS_SPH_DEG, &
         indexflag=ESMF_INDEX_GLOBAL, rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      do j = 1, JM
         do i = 1, IM
            lons(i,j) = -180.0_ESMF_KIND_R8 + (i-1) * 30.0_ESMF_KIND_R8
            lats(i,j) = -52.5_ESMF_KIND_R8 + (j-1) * 15.0_ESMF_KIND_R8
         end do
      end do
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'DIAGONAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_latlon_12x8_diagonal_1pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_latlon_12x8_diagonal

   !---------------------------------------------------------------------------
   ! Generate cubed-sphere C12 equatorial reference
   !---------------------------------------------------------------------------
   subroutine generate_cs_c12_equatorial()
      type(ESMF_Grid) :: grid
      integer :: status
      integer, parameter :: IM = 12
      
      grid = ESMF_GridCreateCubedSphere(IM, &
           staggerLocList=[ESMF_STAGGERLOC_CENTER], &
           coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      if (status /= 0) return
      
      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'EQUATORIAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_cs_c12_equatorial_6pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_cs_c12_equatorial

   !---------------------------------------------------------------------------
   ! Generate cubed-sphere C12 diagonal reference
   !---------------------------------------------------------------------------
   subroutine generate_cs_c12_diagonal()
      type(ESMF_Grid) :: grid
      integer :: status
      integer, parameter :: IM = 12
      
      grid = ESMF_GridCreateCubedSphere(IM, &
           staggerLocList=[ESMF_STAGGERLOC_CENTER], &
           coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      if (status /= 0) return
      
      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'DIAGONAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_cs_c12_diagonal_6pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_cs_c12_diagonal

   !---------------------------------------------------------------------------
   ! Generate cubed-sphere C24 equatorial reference
   !---------------------------------------------------------------------------
   subroutine generate_cs_c24_equatorial()
      type(ESMF_Grid) :: grid
      integer :: status
      integer, parameter :: IM = 24
      
      grid = ESMF_GridCreateCubedSphere(IM, &
           staggerLocList=[ESMF_STAGGERLOC_CENTER], &
           coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      if (status /= 0) return
      
      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'EQUATORIAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_cs_c24_equatorial_6pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_cs_c24_equatorial

   !---------------------------------------------------------------------------
   ! Generate cubed-sphere C24 diagonal reference
   !---------------------------------------------------------------------------
   subroutine generate_cs_c24_diagonal()
      type(ESMF_Grid) :: grid
      integer :: status
      integer, parameter :: IM = 24
      
      grid = ESMF_GridCreateCubedSphere(IM, &
           staggerLocList=[ESMF_STAGGERLOC_CENTER], &
           coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      if (status /= 0) return
      
      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_mask(grid, 'DIAGONAL', [20240101, 20240101], &
                                  [0, 120000], 3600, 'ref_cs_c24_diagonal_6pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_cs_c24_diagonal

   !---------------------------------------------------------------------------
   ! SWATH TESTS - Lat-lon 180x91
   !---------------------------------------------------------------------------
   subroutine generate_latlon_180x91_equatorial_swath()
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer :: status, i, j
      integer, parameter :: IM = 180, JM = 91
      
      grid = ESMF_GridCreate1PeriDim(maxIndex=[IM, JM], &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           regDecomp=[1,1], indexflag=ESMF_INDEX_GLOBAL, rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      if (status /= 0) return
      
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      ! Populate coordinates: 180x91 -> 2° longitude, 2° latitude in DEGREES
      do j = 1, JM
         do i = 1, IM
            lons(i,j) = -180.0_ESMF_KIND_R8 + (i-1) * 2.0_ESMF_KIND_R8  ! -180 to +178°
            lats(i,j) = -90.0_ESMF_KIND_R8 + (j-1) * 2.0_ESMF_KIND_R8  ! -90 to 90°
         end do
      end do
      
      call ESMF_AttributeSet(grid, 'GridType', 'LatLon', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_swath(grid, 'EQUATORIAL', [20240101, 20240101], &
                                   [0, 120000], 3600, 500.0_dp, &
                                   'ref_latlon_180x91_equatorial_swath_1pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_latlon_180x91_equatorial_swath

   subroutine generate_latlon_180x91_diagonal_swath()
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer :: status, i, j
      integer, parameter :: IM = 180, JM = 91
      
      grid = ESMF_GridCreate1PeriDim(maxIndex=[IM, JM], &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           regDecomp=[1,1], indexflag=ESMF_INDEX_GLOBAL, rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      if (status /= 0) return
      
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      ! Populate coordinates: 180x91 -> 2° longitude, 2° latitude in DEGREES
      do j = 1, JM
         do i = 1, IM
            lons(i,j) = -180.0_ESMF_KIND_R8 + (i-1) * 2.0_ESMF_KIND_R8  ! -180 to +178°
            lats(i,j) = -90.0_ESMF_KIND_R8 + (j-1) * 2.0_ESMF_KIND_R8  ! -90 to 90°
         end do
      end do
      
      call ESMF_AttributeSet(grid, 'GridType', 'LatLon', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_swath(grid, 'DIAGONAL', [20240101, 20240101], &
                                   [0, 120000], 3600, 500.0_dp, &
                                   'ref_latlon_180x91_diagonal_swath_1pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_latlon_180x91_diagonal_swath

   !---------------------------------------------------------------------------
   ! SWATH TESTS - Cubed-sphere C48
   !---------------------------------------------------------------------------
   subroutine generate_cs_c48_equatorial_swath()
      type(ESMF_Grid) :: grid
      integer :: status
      integer, parameter :: IM = 48
      
      grid = ESMF_GridCreateCubedSphere(IM, &
           staggerLocList=[ESMF_STAGGERLOC_CENTER], &
           coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      if (status /= 0) return
      
      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_swath(grid, 'EQUATORIAL', [20240101, 20240101], &
                                   [0, 120000], 3600, 500.0_dp, &
                                   'ref_cs_c48_equatorial_swath_6pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_cs_c48_equatorial_swath

   subroutine generate_cs_c48_diagonal_swath()
      type(ESMF_Grid) :: grid
      integer :: status
      integer, parameter :: IM = 48
      
      grid = ESMF_GridCreateCubedSphere(IM, &
           staggerLocList=[ESMF_STAGGERLOC_CENTER], &
           coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      if (status /= 0) return
      
      call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
      if (status /= 0) return
      
      call ESMF_GridAddItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, rc=status)
      if (status /= 0) return
      
      call capture_reference_swath(grid, 'DIAGONAL', [20240101, 20240101], &
                                   [0, 120000], 3600, 500.0_dp, &
                                   'ref_cs_c48_diagonal_swath_6pe.txt')
      
      call ESMF_GridDestroy(grid, rc=status)
   end subroutine generate_cs_c48_diagonal_swath

   !---------------------------------------------------------------------------
   ! Generic: Capture reference swath mask for any ESMF grid
   !---------------------------------------------------------------------------
   subroutine capture_reference_swath(grid, orbit_name, orbit_dates, orbit_times, &
                                      orbit_dt, swath_width_km, filename)
      use mpi
      type(ESMF_Grid), intent(inout) :: grid
      character(len=*), intent(in) :: orbit_name, filename
      integer, intent(in) :: orbit_dates(2), orbit_times(2), orbit_dt
      real(dp), intent(in) :: swath_width_km
      
      real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
      integer(ESMF_KIND_I4), pointer :: mask(:,:)
      real(dp), pointer :: slons(:,:), slats(:,:)
      integer :: status, im, jm, ntiles, nobs
      integer :: localPet, petCount
      integer(ESMF_KIND_I4), allocatable :: global_mask(:,:,:)
      character(len=32) :: grid_type
      type(ESMF_VM) :: vm
      
      ! Get VM info
      call ESMF_VMGetCurrent(vm, rc=status)
      if (status /= 0) return
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=status)
      if (status /= 0) return
      
      ! Get coordinates
      call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lons, rc=status)
      if (status /= 0) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=lats, rc=status)
      if (status /= 0) return
      
      ! Get mask
      call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=status)
      if (status /= 0) return
      
      ! Get dimensions and grid type
      im = size(lons, 1)
      jm = size(lons, 2)
      ntiles = 1
      call ESMF_AttributeGet(grid, 'GridType', value=grid_type, rc=status)
      if (status == 0 .and. trim(grid_type) == 'Cubed-Sphere') ntiles = 6
      
      ! Generate swath orbit
      call orbits_swath(slons, slats, orbit_name, orbit_dates, orbit_times, orbit_dt, &
                       [swath_width_km, swath_width_km], rc=status)
      if (status /= 0) return
      nobs = size(slons, 2)
      
      ! Apply swath masking
      mask = 0
      call orb_swath_mask_lonlat(mask, im, jm, real(lons, ESMF_KIND_R4), &
                                real(lats, ESMF_KIND_R4), slons, slats, nobs, &
                                10, 10, -180.0, 180.0)
      
      ! For cubed-sphere with 6 PEs, gather all tiles to PE 0
      if (ntiles == 6 .and. petCount == 6) then
         if (localPet == 0) then
            allocate(global_mask(im, jm, 6))
         else
            allocate(global_mask(1, 1, 1))  ! Dummy allocation for non-root PEs
         end if
         call MPI_Gather(mask, im*jm, MPI_INTEGER, global_mask, im*jm, MPI_INTEGER, &
                        0, MPI_COMM_WORLD, status)
         if (status /= 0) return
         
         if (localPet == 0) then
            call write_reference_mask(filename, im, jm, 6, global_mask, &
                                      grid_type, orbit_name, petCount)
         end if
         deallocate(global_mask)
      else
         ! For lat-lon, only PE 0 writes
         if (localPet == 0) then
            allocate(global_mask(im, jm, 1))
            global_mask(:,:,1) = mask
            call write_reference_mask(filename, im, jm, 1, global_mask, &
                                      grid_type, orbit_name, petCount)
            deallocate(global_mask)
         end if
      end if
      
      deallocate(slons, slats)
   end subroutine capture_reference_swath

end program generate_reference_masks
