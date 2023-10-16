#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) LatLonGeomFactory_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL_StringVector
   use esmf


contains


   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_LatLonGeomSpec(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig


   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_LatLonGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata


   logical module function supports_spec(this, geom_spec) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(LatLonGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)

   end function supports_spec

   logical module function supports_hconfig(this, hconfig, rc) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(LatLonGeomSpec) :: spec

      supports = spec%supports(hconfig, _RC)
      
      _RETURN(_SUCCESS)
   end function supports_hconfig

   logical module function supports_metadata(this, file_metadata, rc) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LatLonGeomSpec) :: spec

      supports = spec%supports(file_metadata, _RC)
      
      _RETURN(_SUCCESS)
   end function supports_metadata


   module function make_geom(this, geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (geom_spec)
      type is (LatLonGeomSpec)
         geom = typesafe_make_geom(geom_spec, _RC)
      class default
         _FAIL("geom_spec type not supported")
      end select

      _RETURN(_SUCCESS)
   end function make_geom


   function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(LatLonGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid

      grid = create_basic_grid(spec, _RC)
      call fill_coordinates(spec, grid, _RC)
      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function typesafe_make_geom


   module function create_basic_grid(spec, unusable, rc) result(grid)
      use mapl_KeywordEnforcer
      type(ESMF_Grid) :: grid
      type(LatLonGeomSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LatLonDecomposition) :: decomp

      lon_axis = spec%get_lon_axis()
      lat_axis = spec%get_lat_axis()
      decomp = spec%get_decomposition()

      if (lon_axis%is_periodic()) then
         grid = ESMF_GridCreate1PeriDim( &
              & countsPerDEDim1=decomp%get_lon_distribution(), &
              & countsPerDEDim2=decomp%get_lat_distribution(), &
              & indexFlag=ESMF_INDEX_DELOCAL, &
              & gridEdgeLWidth=[0,0], &
              & gridEdgeUWidth=[0,1], &
              & coordDep1=[1,2], &
              & coordDep2=[1,2], &
              & coordSys=ESMF_COORDSYS_SPH_RAD, &
              & _RC)
       else
         grid = ESMF_GridCreateNoPeriDim( &
              & countsPerDEDim1=decomp%get_lon_distribution(), &
              & countsPerDEDim2=decomp%get_lat_distribution(), &
              & indexFlag=ESMF_INDEX_DELOCAL, &
              & gridEdgeLWidth=[0,0], &
              & gridEdgeUWidth=[1,1], &
              & coordDep1=[1,2], &
              & coordDep2=[1,2], &
              & coordSys=ESMF_COORDSYS_SPH_RAD, &
              & _RC)
      end if

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, _RC)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_basic_grid


   module subroutine fill_coordinates(spec, grid, unusable, rc)
      use mapl_KeywordEnforcer
      type(LatLonGeomSpec), intent(in) :: spec
      type(ESMF_Grid), intent(inout) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), pointer :: corners(:,:)
      integer :: i, j
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LonAxis) :: local_lon_axis
      type(LatAxis) :: local_lat_axis
      type(LatLonDecomposition) :: decomp
      integer :: nx, ny, ix, iy

      lon_axis = spec%get_lon_axis()
      lat_axis = spec%get_lat_axis()
      decomp = spec%get_decomposition()

      nx = size(decomp%get_lon_distribution())
      ny = size(decomp%get_lat_distribution())
      call get_ranks(nx, ny, ix, iy, _RC)
 
     ! First we handle longitudes:
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, _RC)
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=corners, _RC)

      lon_axis = spec%get_lon_axis()
      local_lon_axis = decomp%get_lon_subset(lon_axis, rank=ix)
      do j = 1, size(centers,2)
         centers(:,j) = local_lon_axis%get_centers()
      end do
      do j = 1, size(corners,2)
         corners(:,j) = local_lon_axis%get_corners()
      end do
      centers = centers * MAPL_DEGREES_TO_RADIANS_R8
      corners = corners * MAPL_DEGREES_TO_RADIANS_R8


      ! Now latitudes
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, _RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=corners, _RC)

      local_lat_axis = decomp%get_lat_subset(lat_axis, rank=iy)
      do i = 1, size(centers,1)
         centers(i,:) = local_lat_axis%get_centers()
      end do
      do i = 1, size(corners,1)
         corners(i,:) = local_lat_axis%get_corners()
      end do

      centers = centers * MAPL_DEGREES_TO_RADIANS_R8
      corners = corners * MAPL_DEGREES_TO_RADIANS_R8

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine fill_coordinates


   module subroutine get_ranks(nx, ny, ix, iy, rc)
      integer, intent(in) :: nx, ny
      integer, intent(out) :: ix, iy
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: petCount, localPet
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, _RC)

      ix = mod(localPet, nx)
      iy = localPet / nx

      _RETURN(_SUCCESS)
   end subroutine get_ranks

   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      select type (geom_spec)
      type is (LatLonGeomSpec)
         call gridded_dims%push_back('lon')
         call gridded_dims%push_back('lat')
      class default
         _FAIL('geom_spec is not of dynamic type LatLonGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_gridded_dims


   module function make_file_metadata(this, geom_spec, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      file_metadata = FileMetadata()

      select type (geom_spec)
      type is (LatLonGeomSpec)
         file_metadata = typesafe_make_file_metadata(geom_spec, rc)
      class default
         _FAIL('geom_spec is not of dynamic type LatLonGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_file_metadata

   function typesafe_make_file_metadata(geom_spec, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(LatLonGeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(Variable) :: v

      lon_axis = geom_spec%get_lon_axis()
      lat_axis = geom_spec%get_lat_axis()
      
      call file_metadata%add_dimension('lon', lon_axis%get_extent())
      call file_metadata%add_dimension('lat', lat_axis%get_extent())

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='lon')
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(lon_axis%get_centers()))
      
      call file_metadata%add_variable('lon', v)

      v = Variable(type=PFIO_REAL64, dimensions='lat')
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(lat_axis%get_centers()))
      call file_metadata%add_variable('lat', v)

      _RETURN(_SUCCESS)
   end function typesafe_make_file_metadata

end submodule LatLonGeomFactory_smod
