#define I_AM_MAIN
#include "MAPL.h"
program main
   use esmf
   use mapl_ErrorHandling_mod
   implicit none

   type(esmf_Grid) :: cs_grid
   type(esmf_Grid) :: tile_grid
   type(esmf_LocStream) :: tile_locstream
   type(esmf_RouteHandle) :: rh
   type(esmf_Field) :: f_cs, f_tile
   integer :: status
   real, parameter :: MAPL_PI = 4*atan(1.0)

   integer, parameter :: N_TILES = 100
   integer, parameter :: N_TILES_GLOBAL = N_TILES * 6 ! faces

   integer :: counts(1,6)

   counts = 12

   call esmf_initialize(_RC)

   cs_grid = esmf_GridCreateCubedSphere(name='cubed', &
        tileSize=12, countsPerDEDim1PTile=counts, countsPerDEDim2PTile=counts, &
        staggerLocList=[ESMF_STAGGERLOC_CENTER], coordSys=ESMF_COORDSYS_SPH_RAD, _RC)

!#   tile_grid = make_tile_grid(_RC)
   tile_locstream = make_tile_locstream(_RC)

   f_cs = esmf_FieldCreate(grid=cs_grid, typekind=ESMF_TYPEKIND_R4, _RC)
!#   f_tile = esmf_FieldCreate(grid=tile_grid, typekind=ESMF_TYPEKIND_R4, _RC)
   f_tile = esmf_FieldCreate(locstream=tile_locstream, typekind=ESMF_TYPEKIND_R4, _RC)
   

!#   call esmf_FieldRegridStore(f_tile, f_cs, regridMethod=ESMF_REGRIDMETHOD_BILINEAR, routehandle=rh, _RC)
   call esmf_FieldRegridStore(f_tile, f_cs, regridMethod=ESMF_REGRIDMETHOD_NEAREST_STOD, routehandle=rh, _RC)

   call esmf_Finalize(_RC)
   
contains

#undef I_AM_MAIN
#include "MAPL.h"
   function make_tile_grid(rc) result(tile_grid)
      type(esmf_Grid) :: tile_grid
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: lons(:), lats(:)
      integer, allocatable :: local_id(:), arb_index(:,:)
      integer :: i
      type(esmf_DistGrid) :: distgrid
      integer :: status
      integer :: localPet
      type(esmf_VM) :: vm
      integer, allocatable :: index_array(:,:)

      call esmf_VMgetCurrent(vm, _RC)
      call esmf_VMget(vm, localPet=localPet, _RC)
      
      local_id = [(i, i = 1, N_TILES)] + (localPet * n_TILES)
      distgrid = ESMF_DistGridCreate( arbSeqIndexList=local_id, _RC)
 
      tile_grid = ESMF_GridEmptyCreate(_RC)
!#      arb_index = spread(local_id, ncopies=1, dim=1)
      allocate(arb_index(N_TILES,1))
      arb_index(:,1) = local_id

      allocate(index_array(2,1))
      index_array(:,1) = [1, N_TILES_GLOBAL]

      tile_grid = ESMF_GridCreateNoPeriDim( &
            minIndex=[1,1], &
            maxIndex=[N_TILES_GLOBAL,1], &
            coordDep1=[ESMF_DIM_ARB,1], &
            coordDep2=[ESMF_DIM_ARB,1], &
            arbIndexCount=N_TILES, &
            arbIndexList=arb_index, &
            coordTypeKind=ESMF_TYPEKIND_R8, &
            coordSys=ESMF_COORDSYS_SPH_RAD, &
            name='tile', _RC)

      call esmf_GridAddCoord(tile_grid, _RC)
      call esmf_GridGetCoord(tile_grid, coordDim=1, localDE=0, farrayPtr=lons, _RC)
      call esmf_GridGetCoord(tile_grid, coordDim=2, localDE=0, farrayPtr=lats, _RC)

      call random_number(lons)
      lons = lons * (2*MAPL_PI)

      call random_number(lats)
      lats = (lats-0.5)* MAPL_PI

   end function make_tile_grid

   function make_tile_locstream(rc) result(locstream)
      type(esmf_LocStream) :: locstream
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8) :: lats(N_TILES), lons(N_TILES)
      integer :: status

      call random_number(lons)
      lons = lons * (2*MAPL_PI)

      call random_number(lats)
      lats = (lats-0.5)* MAPL_PI

      locstream = ESMF_LocStreamCreate(localCount=N_TILES, coordSys=ESMF_COORDSYS_SPH_RAD, _RC)

      call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lat", farray=lats, datacopyflag=ESMF_DATACOPY_VALUE, &
           keyUnits="Radians", keyLongName="Latitude", _RC)
      call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lon", farray=lons, datacopyflag=ESMF_DATACOPY_VALUE, &
           keyUnits="Radians", keyLongName="Longitude", _RC)

      _RETURN(_SUCCESS)
   end function make_tile_locstream

end program main
