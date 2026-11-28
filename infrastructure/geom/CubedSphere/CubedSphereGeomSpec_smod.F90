#include "MAPL.h"

submodule (mapl_CubedSphereGeomSpec_mod) CubedSphereGeomSpec_smod

   use mapl_CoordinateAxis_mod
   use mapl_GeomSpec_mod
   use pfio
   use mapl_Range_mod
   use mapl_ErrorHandling_mod
   use MAPL_Constants
   use gFTL2_StringVector
   use mapl_StringDictionary_mod
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none
   real(ESMF_Kind_R8) :: undef_schmidt = 1d15

contains

   ! Basic constructor for CubedSphereGeomSpec
   module function new_CubedSphereGeomSpec(im_world, schmidt_parameters, decomposition) result(spec)
      type(CubedSphereGeomSpec) :: spec
      integer, intent(in) :: im_world
      type(ESMF_CubedSphereTransform_Args), intent(in) :: schmidt_parameters
      type(CubedSphereDecomposition), intent(in) :: decomposition

      spec%im_world = im_world
      spec%schmidt_parameters = schmidt_parameters
      spec%decomposition = decomposition
   end function new_CubedSphereGeomSpec

   pure logical module function equal_to(a, b)

      class(CubedSphereGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      select type (b)
      type is (CubedSphereGeomSpec)
         equal_to = (a%im_world== b%im_world)
         if (.not. equal_to) return
         equal_to = (a%decomposition == b%decomposition)
         if (.not. equal_to) return
         equal_to = equal_schmidt(a%schmidt_parameters,b%schmidt_parameters)
      class default
         equal_to = .false.
      end select

   contains

      pure logical function equal_schmidt(a,b)
         type(ESMF_CubedSphereTransform_Args), intent(in) :: a
         type(ESMF_CubedSphereTransform_Args), intent(in) :: b

         equal_schmidt = &
              (a%target_lat /= b%target_lat) .and. &
              (a%target_lon /= b%target_lon) .and. &
              (a%stretch_factor /= b%stretch_factor)
      end function equal_schmidt

   end function equal_to

   ! HConfig section
   module function make_CubedSphereGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(CubedSphereGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      spec%im_world = ESMF_HConfigAsI4(hconfig, keyString='im_world', asOkay=found, _RC)
      _ASSERT(found, '"im_world" not found.')
      spec%decomposition = make_Decomposition(hconfig, cube_size=spec%im_world, _RC)
      spec%schmidt_parameters = make_SchmidtParameters_from_hconfig(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_CubedSphereGeomSpec_from_hconfig

   function make_SchmidtParameters_from_hconfig(hconfig, rc) result(schmidt_parameters)
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out), optional :: rc

      integer :: status, ifound
      logical :: has_tlon, has_tlat, has_sfac, consistent

      schmidt_parameters%stretch_factor = undef_schmidt
      schmidt_parameters%target_lon= undef_schmidt
      schmidt_parameters%target_lat= undef_schmidt
      ifound = 0
      has_sfac = ESMF_HConfigIsDefined(hconfig, keystring='stretch_factor', _RC)
      if (has_sfac) then
         schmidt_parameters%stretch_factor = ESMF_HConfigAsR8(hconfig, keystring='stretch_factor' ,_RC)
         ifound = ifound + 1
      end if
      has_tlon = ESMF_HConfigIsDefined(hconfig, keystring='target_lon', _RC)
      if (has_tlon) then
         schmidt_parameters%target_lon = ESMF_HConfigAsR8(hconfig, keystring='target_lon' ,_RC)
         schmidt_parameters%target_lon = schmidt_parameters%target_lon * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if
      has_tlat = ESMF_HConfigIsDefined(hconfig, keystring='target_lat', _RC)
      if (has_tlat) then
         schmidt_parameters%target_lat = ESMF_HConfigAsR8(hconfig, keystring='target_lat' ,_RC)
         schmidt_parameters%target_lat = schmidt_parameters%target_lat * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if
      consistent = (ifound .eq. 3) .or. (ifound .eq. 0)
      _ASSERT(consistent, "specfied partial stretch parameters")

      _RETURN(_SUCCESS)
   end function make_SchmidtParameters_from_hconfig

   function make_decomposition(hconfig, cube_size, rc) result(decomp)
      type(CubedSphereDecomposition) :: decomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: cube_size
      integer, optional, intent(out) :: rc

      integer, allocatable :: ims(:), jms(:)
      integer :: nx_face, ny_face
      integer :: status
      logical :: has_ims, has_jms, has_nx_face, has_ny_face

      has_ims = ESMF_HConfigIsDefined(hconfig, keystring='ims', _RC)
      has_jms = ESMF_HConfigIsDefined(hconfig, keystring='jms', _RC)
      _ASSERT(has_ims .eqv. has_jms, 'ims and jms must be both defined or both undefined')

      if (has_ims) then
         ims = ESMF_HConfigAsI4Seq(hconfig, keyString='ims', _RC)
         jms = ESMF_HConfigAsI4Seq(hconfig, keyString='jms', _RC)
         decomp = CubedSphereDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      has_nx_face = ESMF_HConfigIsDefined(hconfig, keystring='nx_face', _RC)
      has_ny_face = ESMF_HConfigIsDefined(hconfig, keystring='ny_face', _RC)
      _ASSERT(has_nx_face .eqv. has_ny_face, 'nx_face and ny_face must be both defined or both undefined')

      if (has_nx_face) then
         nx_face = ESMF_HConfigAsI4(hconfig, keyString='nx_face', _RC)
         ny_face = ESMF_HConfigAsI4(hconfig, keyString='ny_face', _RC)
         decomp = CubedSphereDecomposition([cube_size,cube_size], topology=[nx_face, ny_face])
         _RETURN(_SUCCESS)
      end if

      ! Invent a decomposition
      decomp = make_CubedSphereDecomposition([cube_size,cube_size], _RC)

      _RETURN(_SUCCESS)
   end function make_decomposition

   module function make_CubedSphereGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(CubedSphereGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status, im_world
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(CubedSphereDecomposition) :: decomposition

      im_world = file_metadata%get_dimension("Xdim", _RC)
      decomposition = make_CubedSphereDecomposition([im_world,im_world], _RC)
      schmidt_parameters = make_SchmidtParameters_from_metadata(file_metadata, _RC)
      spec = CubedSphereGeomSpec(im_world, schmidt_parameters, decomposition)

      _RETURN(_SUCCESS)
   end function make_CubedSphereGeomSpec_from_metadata

   function make_SchmidtParameters_from_metadata(file_metadata, rc) result(schmidt_parameters)
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(FileMetadata), intent(in) :: file_metadata
      integer, intent(out), optional :: rc

      integer :: status, ifound
      logical :: has_tlon, has_tlat, has_sfac, consistent

      schmidt_parameters%stretch_factor = undef_schmidt
      schmidt_parameters%target_lon= undef_schmidt
      schmidt_parameters%target_lat= undef_schmidt
      ifound = 0
      has_sfac = file_metadata%has_attribute('stretch_factor')
      if (has_sfac) then
         schmidt_parameters%stretch_factor = return_r8(file_metadata, 'stretch_factor', _RC)
         ifound = ifound + 1
      end if
      has_tlon = file_metadata%has_attribute('target_lon')
      if (has_tlon) then
         schmidt_parameters%target_lon = return_r8(file_metadata, 'target_lon', _RC)
         schmidt_parameters%target_lon = schmidt_parameters%target_lon * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if
      has_tlat = file_metadata%has_attribute('target_lat')
      if (has_tlat) then
         schmidt_parameters%target_lat = return_r8(file_metadata, 'target_lat', _RC)
         schmidt_parameters%target_lat = schmidt_parameters%target_lat * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if

      consistent = (ifound .eq. 3) .or. (ifound .eq. 0)
      _ASSERT(consistent, "specfied partial stretch parameters")

      _RETURN(_SUCCESS)
   end function make_SchmidtParameters_from_metadata

   function return_r8(file_metadata, attr_name, rc) result(param)
      real(kind=ESMF_KIND_R8) :: param
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      class(*), pointer :: attr_val(:)
      type(Attribute), pointer :: attr

      attr => file_metadata%get_attribute(attr_name)
      attr_val => attr%get_values()
      select type(q=>attr_val)
      type is (real(kind=REAL32))
         param = q(1)
      type is (real(kind=REAL64))
         param = q(1)
      class default
         _FAIL('unsupported subclass for stretch parameters')
      end select

      _RETURN(_SUCCESS)
   end function return_r8

   ! Accessors
   pure module function get_decomposition(spec) result(decomposition)
      type(CubedSphereDecomposition) :: decomposition
      class(CubedSphereGeomSpec), intent(in) :: spec

      decomposition = spec%decomposition
   end function get_decomposition

   module function get_topology(spec) result(topology)
      class(CubedSphereGeomSpec), intent(in) :: spec
      integer, allocatable :: topology(:)

      ! type(CubedSphereDecomposition) :: decomposition
      integer :: nx_face, ny_face

      ! decomposition = spec%decomposition
      nx_face = size(spec%decomposition%get_x_distribution())
      ny_face = size(spec%decomposition%get_y_distribution())
      topology = [nx_face, ny_face]
   end function get_topology

   pure module function get_im_world(spec) result(im_world)
      integer :: im_world
      class(CubedSphereGeomSpec), intent(in) :: spec

      im_world = spec%im_world
   end function get_im_world

   pure module function get_schmidt_parameters(spec) result(schmidt_parameters)
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      class(CubedSphereGeomSpec), intent(in) :: spec

      schmidt_parameters = spec%schmidt_parameters
   end function get_schmidt_parameters

   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      class(CubedSphereGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: geom_class

      ! Mandatory entry: "class: CubedSphere"
      supports = ESMF_HConfigIsDefined(hconfig, keystring='class', _RC)
      _RETURN_UNLESS(supports)

      geom_class = ESMF_HConfigAsString(hconfig, keyString='class', _RC)
      supports = (geom_class == 'CubedSphere')
      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_hconfig_

   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      class(CubedSphereGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      supports = file_metadata%has_dimension("nf", _RC)

      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_metadata_

   module function make_geom(this, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(CubedSphereGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      character(:), allocatable :: name

      if (this%has_name()) name = this%get_name()
      grid = create_basic_grid(this, name=name, _RC)
      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function make_geom

   module function create_basic_grid(spec, unusable, name, rc) result(grid)
      use mapl_KeywordEnforcer_mod
      type(ESMF_Grid) :: grid
      type(CubedSphereGeomSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: name
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
            staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, &
            name=name, _RC)
      else
         grid = ESMF_GridCreateCubedSPhere(im_world,countsPerDEDim1PTile=ims, &
            countsPerDEDim2PTile=jms,  &
            staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, &
            transformArgs=schmidt_parameters, &
            name=name, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_basic_grid

   module function make_gridded_dims(this, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(CubedSphereGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      call gridded_dims%push_back('Xdim')
      call gridded_dims%push_back('Ydim')
      call gridded_dims%push_back('nf')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_gridded_dims

   module function make_variable_attributes(this, rc) &
        result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(CubedSphereGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      variable_attributes = StringDictionary()
      call variable_attributes%put('coordinates','lons lats')
      call variable_attributes%put('grid_mapping','cubed_sphere')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_variable_attributes

   module function make_file_metadata(this, unusable, chunksizes, rc) &
        result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(CubedSphereGeomSpec), intent(in) :: this
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

      im_world = this%get_im_world()
      schmidt_parameters = this%get_schmidt_parameters()
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
      !call v%add_attribute('units', 'degrees_east')
      call v%add_attribute('units', 'index')
      temp_coords = [(i,i=1,im_world)]
      call file_metadata%add_variable('Xdim', CoordinateVariable(v, temp_coords))
      deallocate(temp_coords)

      v = Variable(type=PFIO_REAL64, dimensions='Ydim')
      call v%add_attribute('long_name', 'Fake Latitude for GrADS Compatibility')
      !call v%add_attribute('units', 'degrees_north')
      call v%add_attribute('units', 'index')
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
   end function make_file_metadata

   logical function is_stretched_cube(schmidt_parameters) result(is_stretched)
      type(ESMF_CubedSphereTransform_Args), intent(in) :: schmidt_parameters

      is_stretched = (schmidt_parameters%target_lat /= undef_schmidt) .and. &
                          (schmidt_parameters%target_lon /= undef_schmidt) .and. &
                          (schmidt_parameters%stretch_factor /= undef_schmidt)
   end function is_stretched_cube

end submodule CubedSphereGeomSpec_smod
