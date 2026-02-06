#include "MAPL_ErrLog.h"

module mapl3g_LocStreamGeomFactory
   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_LocStreamGeomSpec
   use mapl3g_LocStreamDecomposition
   use mapl3g_CoordinateAxis, only: get_dim_name, get_coordinates
   use mapl_ErrorHandlingMod
   use mapl_StringUtilities, only: to_lower
   use mapl3g_get_hconfig, only: get_hconfig
   use mapl3g_hconfig_params, only: HConfigParams
   use pfio_FileMetadataMod,   only: FileMetadata
   use pFIO_VariableMod,       only: Variable
   use pFIO_AttributeMod,      only: Attribute
   use pFIO_StringVariableMapMod
   use pFIO_NetCDF4_FileFormatterMod, only: NetCDF4_FileFormatter
   use pFIO_ConstantsMod,      only: pFIO_READ
   use gftl2_StringVector, only: StringVector
   use mapl3g_StringDictionary, only: StringDictionary
   use mapl_KeywordEnforcerMod, only: KeywordEnforcer
   use MAPL_Constants, only: MAPL_PI_R8
   use esmf
   implicit none
   private

   public :: LocStreamGeomFactory

   type, extends(GeomFactory) :: LocStreamGeomFactory
      private
   contains
      procedure :: make_geom_spec_from_hconfig
      procedure :: make_geom_spec_from_metadata
      procedure :: supports_spec
      procedure :: supports_hconfig
      procedure :: supports_metadata
      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims
      procedure :: make_variable_attributes
   end type LocStreamGeomFactory

contains

   function find_coord_var_name(file_metadata, dim_name, units, rc) result(var_name)
      character(:), allocatable :: var_name
      type(FileMetadata), target, intent(in) :: file_metadata
      character(*), intent(in) :: dim_name
      character(*), intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVariableMap), pointer :: vars
      type(StringVariableMapIterator) :: iter
      type(Variable), pointer :: var
      type(StringVector), pointer :: dims
      type(Attribute), pointer :: attr
      character(:), allocatable :: units_lower, units_found
      logical :: has_units

      var_name = ''
      units_lower = ESMF_UtilStringLowerCase(units, _RC)

      vars => file_metadata%get_variables(_RC)
      associate (e => vars%ftn_end())
         iter = vars%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var => iter%second()

            has_units = var%is_attribute_present('units', _RC)
            if (.not. has_units) cycle

            attr => var%get_attribute('units', _RC)
            units_found = attr%get_string(_RC)
            units_found = ESMF_UtilStringLowerCase(units_found, _RC)
            if (units_found /= units_lower) cycle

            dims => var%get_dimensions()
            if (dims%size() /= 1) cycle
            if (trim(dims%of(1)) /= trim(dim_name)) cycle

            _ASSERT(var_name == '', 'Multiple coordinate variables with units '//units//' for dimension '//dim_name)
            var_name = iter%first()
         end do
      end associate

      _RETURN(_SUCCESS)
   end function find_coord_var_name

   function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LocStreamGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), allocatable :: lon(:), lat(:)
      integer :: npoints
      type(HConfigParams) :: params
      type(LocStreamDecomposition) :: decomp

      logical :: has_lon, has_lat, has_file
      character(len=:), allocatable :: filename
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetadata) :: metadata

      _UNUSED_DUMMY(this)

      ! Two mutually exclusive configuration paths are supported:
      ! 1) Explicit lon/lat arrays in the hconfig (test-oriented path).
      ! 2) A "file" entry pointing to a NetCDF file from which
      !    locstream coordinates are read via FileMetadata.

      has_lon  = ESMF_HConfigIsDefined(hconfig, keyString='lon',  _RC)
      has_lat  = ESMF_HConfigIsDefined(hconfig, keyString='lat',  _RC)
      has_file = ESMF_HConfigIsDefined(hconfig, keyString='file', _RC)

      if (has_file) then
         ! When a file is specified, explicit lon/lat arrays must
         ! not also be present in the same hconfig.
         _ASSERT(.not.(has_lon .or. has_lat), 'LocStream hconfig may specify either lon/lat or file, but not both')

         filename = ESMF_HConfigAsString(hconfig, keyString='file', _RC)

         call file_formatter%open(filename, pFIO_READ, _RC)
         metadata = file_formatter%read(_RC)
         call file_formatter%close(_RC)

         ! Reuse the existing metadata-based path to build the
         ! LocStreamGeomSpec from the file's coordinates.
         geom_spec = this%make_geom_spec_from_metadata(metadata, _RC)

         _RETURN(_SUCCESS)
      end if

      ! No file: fall back to explicit coordinate arrays, which
      ! must provide both lon and lat.
      _ASSERT(has_lon .and. has_lat, 'LocStream hconfig must provide lon/lat arrays or a file')

      params = HConfigParams(hconfig, "lon")
      call get_hconfig(lon, params, _RC)
      params = HConfigParams(hconfig, "lat")
      call get_hconfig(lat, params, _RC)

      _ASSERT(size(lon) == size(lat), "LocStream lon/lat arrays must have same length")
      npoints = size(lon)

      ! Create decomposition from current VM
      decomp = make_LocStreamDecomposition(npoints, _RC)

      allocate(LocStreamGeomSpec :: geom_spec)
      geom_spec = LocStreamGeomSpec(npoints, decomp)

      ! Store coordinate values (in degrees) so that the
      ! LocStream can be created with the correct content
      ! later in make_geom.
      select type (ls_spec => geom_spec)
      type is (LocStreamGeomSpec)
         call ls_spec%set_coordinates(lon, lat)
      end select

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig

   function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LocStreamGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dim_name, lat_dim
      character(:), allocatable :: lon_var_name, lat_var_name
      integer :: npoints
      real(kind=ESMF_KIND_R8), allocatable :: lon(:), lat(:)
      type(LocStreamDecomposition) :: decomp

      _UNUSED_DUMMY(this)

      ! For LocStream metadata we expect latitude and longitude
      ! coordinate variables with units of degrees_north and
      ! degrees_east that share a single dimension, e.g. "loc".

      dim_name = get_dim_name(file_metadata, units='degrees_east', _RC)
      _ASSERT(dim_name /= '', 'LocStream metadata missing longitude coordinates')

      lat_dim = get_dim_name(file_metadata, units='degrees_north', _RC)
      _ASSERT(lat_dim /= '', 'LocStream metadata missing latitude coordinates')
      _ASSERT(lat_dim == dim_name, 'Lat/Lon coordinates must share a single dimension for LocStream')

      ! Identify the specific longitude and latitude coordinate
      ! variable names associated with this shared dimension.
      lon_var_name = find_coord_var_name(file_metadata, dim_name, 'degrees_east', _RC)
      _ASSERT(lon_var_name /= '', 'LocStream metadata missing longitude coordinate variable for dimension '//dim_name)

      lat_var_name = find_coord_var_name(file_metadata, dim_name, 'degrees_north', _RC)
      _ASSERT(lat_var_name /= '', 'LocStream metadata missing latitude coordinate variable for dimension '//dim_name)

      npoints = file_metadata%get_dimension(dim_name, _RC)

      lon = get_coordinates(file_metadata, lon_var_name, _RC)
      lat = get_coordinates(file_metadata, lat_var_name, _RC)

      _ASSERT(size(lon) == npoints, 'LocStream metadata longitude size mismatch with dimension')
      _ASSERT(size(lat) == npoints, 'LocStream metadata latitude size mismatch with dimension')

      ! Create decomposition from current VM
      decomp = make_LocStreamDecomposition(npoints, _RC)

      allocate(LocStreamGeomSpec :: geom_spec)
      geom_spec = LocStreamGeomSpec(npoints, decomp)

      ! Persist coordinate values (degrees) in the spec so
      ! they can be converted to radians when constructing
      ! the ESMF_LocStream.
      select type (ls_spec => geom_spec)
      type is (LocStreamGeomSpec)
         call ls_spec%set_coordinates(lon, lat)
      end select

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata

   logical function supports_spec(this, geom_spec) result(supports)
      class(LocStreamGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(LocStreamGeomSpec) :: reference

      _UNUSED_DUMMY(this)

      supports = same_type_as(geom_spec, reference)

   end function supports_spec

   logical function supports_hconfig(this, hconfig, rc) result(supports)
      class(LocStreamGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(HConfigParams) :: params

      ! Minimal implementation for now: honor class: locstream (case-insensitive)
      character(len=:), allocatable :: class_name

      logical :: has_lon, has_lat, has_file

      _UNUSED_DUMMY(this)

      ! Mandatory entry: "class: locstream" (case-insensitive)
      params = HConfigParams(hconfig, "class")
      call get_hconfig(class_name, params, _RC)
      if (.not. allocated(class_name)) then
         supports = .false.
         _RETURN(_SUCCESS)
      end if

      class_name = to_lower(class_name)
      supports = trim(class_name) == "locstream"
      _RETURN_UNLESS(supports)

      ! Schema: exactly one of the following must be chosen:
      ! 1) Explicit lon/lat arrays
      ! 2) A "file" entry
      has_lon  = ESMF_HConfigIsDefined(hconfig, keyString='lon',  _RC)
      has_lat  = ESMF_HConfigIsDefined(hconfig, keyString='lat',  _RC)
      has_file = ESMF_HConfigIsDefined(hconfig, keyString='file', _RC)

      if (has_file) then
         ! File-based LocStream: explicit lon/lat arrays in the
         ! same configuration are considered an error and must
         ! trigger an exception rather than being silently
         ! rejected.
         if (has_lon .or. has_lat) then
            supports = .false.
            _FAIL("LocStream hconfig may specify either lon/lat or file, but not both")
         else
            supports = .true.
         end if
      else
         ! Explicit-coordinate LocStream: both lon and lat must
         ! be present.
         supports = has_lon .and. has_lat
      end if

      _RETURN(_SUCCESS)
   end function supports_hconfig

   logical function supports_metadata(this, file_metadata, rc) result(supports)
      class(LocStreamGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      character(:), allocatable :: lon_dim, lat_dim

      ! Identify LocStream-style metadata: both latitude and
      ! longitude coordinates exist and share a single dimension
      ! (typically something like "loc"). This pattern is
      ! distinct from regular LatLon grids which use separate
      ! latitude and longitude dimensions.

      _UNUSED_DUMMY(this)

      lon_dim = get_dim_name(file_metadata, units='degrees_east', _RC)
      lat_dim = get_dim_name(file_metadata, units='degrees_north', _RC)

      supports = (lon_dim /= '' .and. lat_dim /= '' .and. lon_dim == lat_dim)

      _RETURN(_SUCCESS)
   end function supports_metadata

   function make_geom(this, geom_spec, rc) result(geom)
      class(LocStreamGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      type(ESMF_Geom) :: geom
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: local_count, i_0, i_1
      type(ESMF_LocStream) :: locstream
      type(ESMF_VM) :: vm
      integer :: local_pet
      real(kind=ESMF_KIND_R8), allocatable :: tlons(:), tlats(:)
      real(kind=ESMF_KIND_R8), pointer :: lons_deg(:) => null(), lats_deg(:) => null()
      type(LocStreamDecomposition) :: decomp

      _UNUSED_DUMMY(this)

      select type (geom_spec)
      type is (LocStreamGeomSpec)
         ! Get the decomposition and current PE's rank
         decomp = geom_spec%get_decomposition()
         call ESMF_VMGetCurrent(vm, _RC)
         call ESMF_VMGet(vm, localPet=local_pet, _RC)

         ! Determine local indices for this PE
         call decomp%get_local_indices(local_pet, i_0, i_1)
         
         local_count = i_1 - i_0 + 1

         allocate(tlons(local_count))
         allocate(tlats(local_count))

         call geom_spec%get_coordinates(lons_deg, lats_deg)
         _ASSERT(associated(lons_deg) .and. associated(lats_deg), 'LocStreamGeomSpec missing coordinates')
         _ASSERT(i_1 <= size(lons_deg), 'LocStreamGeomSpec coordinate index out of bounds')
         _ASSERT(i_1 <= size(lats_deg), 'LocStreamGeomSpec coordinate index out of bounds')

         ! Extract local portion of coordinates and convert from degrees to radians
         tlons = lons_deg(i_0:i_1) * MAPL_PI_R8 / 180.0_ESMF_KIND_R8
         tlats = lats_deg(i_0:i_1) * MAPL_PI_R8 / 180.0_ESMF_KIND_R8

         locstream = ESMF_LocStreamCreate(localCount=local_count, coordSys=ESMF_COORDSYS_SPH_RAD, _RC)
         call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lat", farray=tlats, datacopyflag=ESMF_DATACOPY_VALUE, &
              keyUnits="Radians", keyLongName="Latitude", _RC)
         call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lon", farray=tlons, datacopyflag=ESMF_DATACOPY_VALUE, &
              keyUnits="Radians", keyLongName="Longitude", _RC)

         geom = ESMF_GeomCreate(locstream, _RC)
      class default
         _FAIL("geom_spec type not supported")
      end select

      _RETURN(_SUCCESS)
   end function make_geom

   function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      class(LocStreamGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      type(FileMetadata) :: file_metadata
      integer, optional, intent(out) :: rc

      ! LocStream-specific file metadata generation can be added later.
      file_metadata = FileMetadata()
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom_spec)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)

      _RETURN(_SUCCESS)
   end function make_file_metadata

   function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      class(LocStreamGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      type(StringVector) :: gridded_dims
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom_spec)

      call gridded_dims%push_back("loc")

      _RETURN(_SUCCESS)
   end function make_gridded_dims

   function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      class(LocStreamGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      type(StringDictionary) :: variable_attributes
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom_spec)

      variable_attributes = StringDictionary()

      _RETURN(_SUCCESS)
   end function make_variable_attributes

end module mapl3g_LocStreamGeomFactory
