#include "MAPL_ErrLog.h"
submodule (mapl3g_CubedSphereGeomFactory) CubedSphereGeomFactory_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_CubedSphereDecomposition
   use mapl3g_CubedSphereGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none
   real(ESMF_TypeKind_R8) :: undef_schmit = 1d15

contains


   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(CubedSphereGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_CubedSphereGeomSpec(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig


   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(CubedSphereGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_CubedSphereGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata


   logical module function supports_spec(this, geom_spec) result(supports)
      class(CubedSphereGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(CubedSphereGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)

   end function supports_spec

   logical module function supports_hconfig(this, hconfig, rc) result(supports)
      class(CubedSphereGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(CubedSphereGeomSpec) :: spec

      supports = spec%supports(hconfig, _RC)
      
      _RETURN(_SUCCESS)
   end function supports_hconfig

   logical module function supports_metadata(this, file_metadata, rc) result(supports)
      class(CubedSphereGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(CubedSphereGeomSpec) :: spec

      supports = spec%supports(file_metadata, _RC)
      
      _RETURN(_SUCCESS)
   end function supports_metadata


   module function make_geom(this, geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(CubedSphereGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (geom_spec)
      type is (CubedSphereGeomSpec)
         geom = typesafe_make_geom(geom_spec, _RC)
      class default
         _FAIL("geom_spec type not supported")
      end select

      _RETURN(_SUCCESS)
   end function make_geom


   function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(CubedSphereGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid

      grid = create_basic_grid(spec, _RC)
      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function typesafe_make_geom


   module function create_basic_grid(spec, unusable, rc) result(grid)
      type(ESMF_Grid) :: grid
      type(CubedSphereGeomSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, im_world, ntiles, i
      type(CubedSphereDecomposition) :: decomp
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      logical :: is_stretched
      integer, allocatable :: ims(:,:), jms(:,:), face_ims(:), face_jms(:)

      ntiles = 6

      decomp = spec%get_decomposition()
      schmidt_parameters = spec%get_schmidt_parameters
      im_world = spec%get_im_world
      is_stretched = All(schmidt_parameters = undef_schmit)
      face_ims = decomp%get_x_distribution()
      face_jms = decomp%get_y_distribution()
      allocate(ims(ntiles,size(face_ims)))
      allocate(ims(ntiles,size(face_jms)))
      do i=1,ntiles
         ims(:,i) = face_ims 
         hms(:,i) = face_jms 
      enddo

      if (is_stretched) then
         grid = ESMF_GridCreateCubedSPhere(this%im_world,countsPerDEDim1PTile=ims, &
            countsPerDEDim2PTile=jms  &
            staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, &
            transformArgs=schmidt_parameters, _RC)
      else
         grid = ESMF_GridCreateCubedSPhere(this%im_world,countsPerDEDim1PTile=ims, &
            countsPerDEDim2PTile=jms, &
            staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, _RC)
      end if

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, _RC)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_basic_grid

   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(CubedSphereGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      select type (geom_spec)
      type is (CubedSphereGeomSpec)
         call gridded_dims%push_back('lon')
         call gridded_dims%push_back('lat')
      class default
         _FAIL('geom_spec is not of dynamic type CubedSphereGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_gridded_dims


   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(CubedSphereGeomFactory), intent(in) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      file_metadata = FileMetadata()

      select type (geom_spec)
      type is (CubedSphereGeomSpec)
         file_metadata = typesafe_make_file_metadata(geom_spec, chunksizes=chunksizes, _RC)
      class default
         _FAIL('geom_spec is not of dynamic type CubedSphereGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_file_metadata

   function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(CubedSphereGeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(Variable) :: v

      lon_axis = geom_spec%get_lon_axis()
      lat_axis = geom_spec%get_lat_axis()
      
      call file_metadata%add_dimension('lon', lon_axis%get_extent())
      call file_metadata%add_dimension('lat', lat_axis%get_extent())

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='lon', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(lon_axis%get_centers()))
      
      call file_metadata%add_variable('lon', v)

      v = Variable(type=PFIO_REAL64, dimensions='lat', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(lat_axis%get_centers()))
      call file_metadata%add_variable('lat', v)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function typesafe_make_file_metadata

end submodule CubedSphereGeomFactory_smod
