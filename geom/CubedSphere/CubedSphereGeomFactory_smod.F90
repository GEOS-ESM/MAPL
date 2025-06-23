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
   implicit none(type,external)

   real(kind=ESMF_Kind_R8), parameter :: UNDEF_SCHMIDT = 1d15

contains


   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(CubedSphereGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_CubedSphereGeomSpec(hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_geom_spec_from_hconfig


   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(CubedSphereGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_CubedSphereGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_geom_spec_from_metadata


   logical module function supports_spec(this, geom_spec) result(supports)
      class(CubedSphereGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(CubedSphereGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)

      _UNUSED_DUMMY(this)
   end function supports_spec

   logical module function supports_hconfig(this, hconfig, rc) result(supports)
      class(CubedSphereGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(CubedSphereGeomSpec) :: spec

      supports = spec%supports(hconfig, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_hconfig

   logical module function supports_metadata(this, file_metadata, rc) result(supports)
      class(CubedSphereGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(CubedSphereGeomSpec) :: spec

      supports = spec%supports(file_metadata, _RC)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
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
      _UNUSED_DUMMY(this)
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
      logical :: not_stretched
      integer, allocatable :: ims(:,:), jms(:,:), face_ims(:), face_jms(:)

      ntiles = 6

      decomp = spec%get_decomposition()
      schmidt_parameters = spec%get_schmidt_parameters()
      im_world = spec%get_im_world()
      not_stretched = .not. is_stretched_cube(schmidt_parameters) 
      face_ims = decomp%get_x_distribution()
      face_jms = decomp%get_y_distribution()
      allocate(ims(size(face_ims),ntiles))
      allocate(jms(size(face_jms),ntiles))
      do i=1,ntiles
         ims(:,i) = face_ims 
         jms(:,i) = face_jms 
      enddo

      if (not_stretched) then
         grid = ESMF_GridCreateCubedSPhere(im_world,countsPerDEDim1PTile=ims, &
            countsPerDEDim2PTile=jms, &
            staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, _RC)
      else
         grid = ESMF_GridCreateCubedSPhere(im_world,countsPerDEDim1PTile=ims, &
            countsPerDEDim2PTile=jms,  &
            staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, &
            transformArgs=schmidt_parameters, _RC)
      end if
      
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
         call gridded_dims%push_back('Xdim')
         call gridded_dims%push_back('Ydim')
         call gridded_dims%push_back('nf')
      class default
         _FAIL('geom_spec is not of dynamic type CubedSphereGeomSpec.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
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
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
  end function make_file_metadata

   function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(CubedSphereGeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: im, im_world
      type (Variable) :: v
      integer, parameter :: MAXLEN=80
      character(len=MAXLEN) :: gridspec_file_name
      !!! character(len=5), allocatable :: cvar(:,:)
      integer, allocatable :: ivar(:,:)
      integer, allocatable :: ivar2(:,:,:)

      real(ESMF_KIND_R8), allocatable :: temp_coords(:)

      integer :: status, i
      integer, parameter :: ncontact = 4
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      integer, parameter :: nf = 6
      logical :: is_stretched

      im_world = geom_spec%get_im_world()
      schmidt_parameters = geom_spec%get_schmidt_parameters()
      is_stretched = is_stretched_cube(schmidt_parameters)
      ! Grid dimensions
      call file_metadata%add_dimension('Xdim', im_world, _RC)
      call file_metadata%add_dimension('Ydim', im_world, _RC)
      call file_metadata%add_dimension('XCdim', im_world+1, _RC)
      call file_metadata%add_dimension('YCdim', im_world+1, _RC)
      call file_metadata%add_dimension('nf', nf, _RC)
      call file_metadata%add_dimension('ncontact', ncontact, _RC)
      call file_metadata%add_dimension('orientationStrLen', 5, _RC)

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='Xdim')
      call v%add_attribute('long_name', 'Fake Longitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_east')
      temp_coords = [(i,i=1,im_world)]
      call file_metadata%add_variable('Xdim', CoordinateVariable(v, temp_coords))
      deallocate(temp_coords)

      v = Variable(type=PFIO_REAL64, dimensions='Ydim')
      call v%add_attribute('long_name', 'Fake Latitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_north')
      temp_coords = [(i,i=1,im_world)] 
      call file_metadata%add_variable('Ydim', CoordinateVariable(v, temp_coords))
      deallocate(temp_coords)

      v = Variable(type=PFIO_INT32, dimensions='nf')
      call v%add_attribute('long_name','cubed-sphere face')
      call v%add_attribute('axis','e')
      call v%add_attribute('grads_dim','e')
      call v%add_const_value(UnlimitedEntity([1,2,3,4,5,6]))
      call file_metadata%add_variable('nf',v)

      v = Variable(type=PFIO_INT32, dimensions='ncontact')
      call v%add_attribute('long_name','number of contact points')
      call v%add_const_value(UnlimitedEntity([1,2,3,4]))
      call file_metadata%add_variable('ncontact',v)
      ! Other variables
      allocate(ivar(4,6))
      ivar = reshape( [5, 3, 2, 6, &
                       1, 3, 4, 6, &
                       1, 5, 4, 2, &
                       3, 5, 6, 2, &
                       3, 1, 6, 4, &
                       5, 1, 2, 4 ], [ncontact,nf])
      v = Variable(type=PFIO_INT32, dimensions='ncontact,nf')
      call v%add_attribute('long_name', 'adjacent face starting from left side going clockwise')
      call v%add_const_value(UnlimitedEntity(ivar))
      call file_metadata%add_variable('contacts', v)      !!! At present pfio does not seem to work with string variables
      !!! allocate(cvar(4,6))
      !!! cvar =reshape([" Y:-X", " X:-Y", " Y:Y ", " X:X ", &
      !!!                " Y:Y ", " X:X ", " Y:-X", " X:-Y", &
      !!!                " Y:-X", " X:-Y", " Y:Y ", " X:X ", &
      !!!                " Y:Y ", " X:X ", " Y:-X", " X:-Y", &
      !!!                " Y:-X", " X:-Y", " Y:Y ", " X:X ", &
      !!!                " Y:Y ", " X:X ", " Y:-X", " X:-Y" ], [ncontact,nf])
      !!! v = Variable(type=PFIO_STRING, dimensions='orientationStrLen,ncontact,nf')
      !!! call v%add_attribute('long_name', 'orientation of boundary')
      !!! call v%add_const_value(UnlimitedEntity(cvar))
      !!! call file_metadata%add_variable('orientation', v)

      im = im_world
      allocate(ivar2(4,4,6))
      ivar2 = reshape(            &
               [[im, im,  1, im,  &
                  1, im,  1,  1,  &
                  1, im,  1,  1,  &
                 im, im,  1, im], &
                [im,  1, im, im,  &
                  1,  1, im,  1,  &
                  1,  1, im,  1,  &
                 im,  1, im, im], &
                [im, im,  1, im,  &
                  1, im,  1,  1,  &
                  1, im,  1,  1,  &
                 im, im,  1, im], &
                [im,  1, im, im,  &
                  1,  1, im,  1,  &
                  1,  1, im,  1,  &
                 im,  1, im, im], &
                [im, im,  1, im,  &
                  1, im,  1,  1,  &
                  1, im,  1,  1,  &
                 im, im,  1, im], &
                [im,  1, im, im,  &
                  1,  1, im,  1,  &
                  1,  1, im,  1,  &
                 im,  1, im, im] ], [ncontact,ncontact,nf])
      v = Variable(type=PFIO_INT32, dimensions='ncontact,ncontact,nf')
      call v%add_attribute('long_name', 'anchor point')
      call v%add_const_value(UnlimitedEntity(ivar2))
      call file_metadata%add_variable('anchor', v)

      call file_metadata%add_attribute('grid_mapping_name', 'gnomonic cubed-sphere')
      call file_metadata%add_attribute('file_format_version', '2.92')
      call file_metadata%add_attribute('additional_vars', 'contacts,orientation,anchor')
      write(gridspec_file_name,'("C",i0,"_gridspec.nc4")') im_world
      call file_metadata%add_attribute('gridspec_file', trim(gridspec_file_name))

      v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim,nf')
      call v%add_attribute('long_name','longitude')
      call v%add_attribute('units','degrees_east')
      call file_metadata%add_variable('lons',v)

      v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim,nf')
      call v%add_attribute('long_name','latitude')
      call v%add_attribute('units','degrees_north')
      call file_metadata%add_variable('lats',v)

      v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim,nf')
      call v%add_attribute('long_name','longitude')
      call v%add_attribute('units','degrees_east')
      call file_metadata%add_variable('corner_lons',v)

      v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim,nf')
      call v%add_attribute('long_name','latitude')
      call v%add_attribute('units','degrees_north')
      call file_metadata%add_variable('corner_lats',v)

      if (is_stretched) then
         call file_metadata%add_attribute('STRETCH_FACTOR',schmidt_parameters%stretch_factor)
         call file_metadata%add_attribute('TARGET_LON',schmidt_parameters%target_lon)
         call file_metadata%add_attribute('TARGET_LAT',schmidt_parameters%target_lat)
      end if


      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)
  end function typesafe_make_file_metadata

   function is_stretched_cube(schmidt_parameters) result(is_stretched)
      logical :: is_stretched
      type(ESMF_CubedSphereTransform_Args), intent(in) :: schmidt_parameters

      is_stretched = (schmidt_parameters%target_lat /= UNDEF_SCHMIDT) .and. &
                          (schmidt_parameters%target_lon /= UNDEF_SCHMIDT) .and. &
                          (schmidt_parameters%stretch_factor /= UNDEF_SCHMIDT) 
   end function is_stretched_cube

end submodule CubedSphereGeomFactory_smod
