! Standalone reproducer for SIGFPE in ESMF_FieldRegridStore.
!
! Replicates what MAPL's LatLonGeomFactory does for:
!   src: im_world=8,  jm_world=5,  pole=PC, dateline=DC  (8x5)
!   dst: im_world=16, jm_world=11, pole=PC, dateline=DC  (16x11)
!
! PC pole: cell centers at -90 and +90; delta = 180/(jm-1)
! DC dateline: periodic longitude grid via ESMF_GridCreate1PeriDim
!
! Build (example with gfortran + ESMF):
!   gfortran -o reproduce_sigfpe reproduce_sigfpe.F90 \
!       $(esmf_config --f90compileopts) $(esmf_config --f90linkopts)
! Or via cmake using an ESMF_add_executable target.

program reproduce_sigfpe
   use ESMF
   implicit none

   integer :: rc
   type(ESMF_Grid)        :: grid_src, grid_dst
   type(ESMF_Field)       :: field_src, field_dst
   type(ESMF_Routehandle) :: routehandle

   call ESMF_Initialize(defaultLogFilename='reproduce_sigfpe.Log', rc=rc)
   if (rc /= ESMF_SUCCESS) stop 'ESMF_Initialize failed'

   grid_src = make_pc_grid(im=8,  jm=5,  name='src')
   grid_dst = make_pc_grid(im=16, jm=11, name='dst')

   field_src = ESMF_FieldCreate(grid_src, typekind=ESMF_TYPEKIND_R4, name='src', rc=rc)
   if (rc /= ESMF_SUCCESS) stop 'FieldCreate src failed'

   field_dst = ESMF_FieldCreate(grid_dst, typekind=ESMF_TYPEKIND_R4, name='dst', rc=rc)
   if (rc /= ESMF_SUCCESS) stop 'FieldCreate dst failed'

   write(*,*) 'Calling ESMF_FieldRegridStore ...'
   call ESMF_FieldRegridStore(field_src, field_dst, &
        regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
        routeHandle=routehandle, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) stop 'FieldRegridStore failed'

   write(*,*) 'Success.'

   call ESMF_FieldDestroy(field_src, rc=rc)
   call ESMF_FieldDestroy(field_dst, rc=rc)
   call ESMF_RouteHandleDestroy(routehandle, rc=rc)
   call ESMF_GridDestroy(grid_src, rc=rc)
   call ESMF_GridDestroy(grid_dst, rc=rc)
   call ESMF_Finalize(rc=rc)

contains

   ! Build a PC-pole, DC-dateline LatLon grid.
   ! PC pole: centers at -90..+90, delta = 180/(jm-1)
   !          corners span [-90-d/2, +90+d/2]
   ! DC dateline: periodic in longitude via ESMF_GridCreate1PeriDim
   function make_pc_grid(im, jm, name) result(grid)
      type(ESMF_Grid) :: grid
      integer, intent(in) :: im, jm
      character(len=*), intent(in) :: name

      integer :: i, j, rc
      real(ESMF_KIND_R8) :: delta_lon, delta_lat
      real(ESMF_KIND_R8), pointer :: lon_ctr(:,:), lat_ctr(:,:)
      real(ESMF_KIND_R8), pointer :: lon_cor(:,:), lat_cor(:,:)
      real(ESMF_KIND_R8), parameter :: PI = acos(-1.d0)
      real(ESMF_KIND_R8), parameter :: DEG2RAD = PI / 180.d0

      delta_lon = 360.d0 / im
      delta_lat = 180.d0 / (jm - 1)   ! PC pole

      grid = ESMF_GridCreate1PeriDim( &
           countsPerDEDim1=[im], &
           countsPerDEDim2=[jm], &
           indexFlag=ESMF_INDEX_DELOCAL, &
           gridEdgeLWidth=[0, 0], &
           gridEdgeUWidth=[0, 1], &
           coordDep1=[1, 2], &
           coordDep2=[1, 2], &
           coordSys=ESMF_COORDSYS_SPH_RAD, &
           name=name, &
           rc=rc)
      if (rc /= ESMF_SUCCESS) stop 'GridCreate1PeriDim failed'

      call ESMF_GridAddCoord(grid, rc=rc)
      if (rc /= ESMF_SUCCESS) stop 'GridAddCoord center failed'
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if (rc /= ESMF_SUCCESS) stop 'GridAddCoord corner failed'

      ! --- fill centers ---
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lon_ctr, rc=rc)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lat_ctr, rc=rc)

      do j = 1, jm
         do i = 1, im
            lon_ctr(i,j) = ((i - 0.5d0) * delta_lon) * DEG2RAD
            lat_ctr(i,j) = (-90.d0 + (j-1) * delta_lat) * DEG2RAD
         end do
      end do

      ! --- fill corners ---
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=lon_cor, rc=rc)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=lat_cor, rc=rc)

      do j = 1, jm+1
         do i = 1, im
            lon_cor(i,j) = ((i - 1.d0) * delta_lon) * DEG2RAD
            lat_cor(i,j) = (-90.d0 - delta_lat/2.d0 + (j-1)*delta_lat) * DEG2RAD
         end do
      end do

   end function make_pc_grid

end program reproduce_sigfpe
