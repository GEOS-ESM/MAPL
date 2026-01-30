#include "MAPL_ErrLog.h"

module mapl3g_LocStreamGeomFactory
   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_LocStreamGeomSpec
   use mapl3g_CoordinateAxis, only: get_dim_name, get_coordinates
   use mapl_ErrorHandlingMod
   use mapl_StringUtilities, only: to_lower
   use mapl3g_get_hconfig, only: get_hconfig
   use mapl3g_hconfig_params, only: HConfigParams
   use pfio_FileMetadataMod, only: FileMetadata
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

   function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LocStreamGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), allocatable :: lon(:), lat(:)
      integer :: npoints
      type(HConfigParams) :: params

      ! Test-oriented path: accept explicit lon/lat coordinate
      ! arrays from hconfig. These are expected to be in degrees.
      params = HConfigParams(hconfig, "lon")
      call get_hconfig(lon, params, _RC)
      params = HConfigParams(hconfig, "lat")
      call get_hconfig(lat, params, _RC)

      _ASSERT(size(lon) == size(lat), "LocStream lon/lat arrays must have same length")
      npoints = size(lon)

      allocate(LocStreamGeomSpec :: geom_spec)
      geom_spec = LocStreamGeomSpec(npoints)

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

      character(:), allocatable :: dim_name
      integer :: npoints
      real(kind=ESMF_KIND_R8), allocatable :: lon(:), lat(:)

      ! For LocStream metadata we expect latitude and longitude
      ! coordinate variables with units of degrees_north and
      ! degrees_east that share a single dimension, e.g. "loc".

      dim_name = get_dim_name(file_metadata, units='degrees_east', _RC)
      _ASSERT(dim_name /= '', 'LocStream metadata missing longitude coordinates')

      block
         character(:), allocatable :: lat_dim
         lat_dim = get_dim_name(file_metadata, units='degrees_north', _RC)
         _ASSERT(lat_dim /= '', 'LocStream metadata missing latitude coordinates')
         _ASSERT(lat_dim == dim_name, 'Lat/Lon coordinates must share a single dimension for LocStream')
      end block

      npoints = file_metadata%get_dimension(dim_name, _RC)

      lon = get_coordinates(file_metadata, dim_name, _RC)
      lat = get_coordinates(file_metadata, dim_name, _RC)
      _ASSERT(size(lon) == npoints, 'LocStream metadata longitude size mismatch with dimension')
      _ASSERT(size(lat) == npoints, 'LocStream metadata latitude size mismatch with dimension')

      allocate(LocStreamGeomSpec :: geom_spec)
      geom_spec = LocStreamGeomSpec(npoints)

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

      params = HConfigParams(hconfig, "class")
      call get_hconfig(class_name, params, _RC)
      if (allocated(class_name)) then
         class_name = to_lower(class_name)
         supports = trim(class_name) == "locstream"
      else
         supports = .false.
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
      integer :: local_count
      type(ESMF_LocStream) :: locstream
      real(kind=ESMF_KIND_R8), allocatable :: tlons(:), tlats(:)
      real(kind=ESMF_KIND_R8), pointer :: lons_deg(:) => null(), lats_deg(:) => null()

      select type (geom_spec)
      type is (LocStreamGeomSpec)
         local_count = geom_spec%get_npoints()

         allocate(tlons(local_count), stat=status)
         _VERIFY(status)
         allocate(tlats(local_count), stat=status)
         _VERIFY(status)

             call geom_spec%get_coordinates(lons_deg, lats_deg)
             _ASSERT(associated(lons_deg) .and. associated(lats_deg), 'LocStreamGeomSpec missing coordinates')
             _ASSERT(size(lons_deg) == local_count, 'LocStreamGeomSpec coordinate size mismatch')
             _ASSERT(size(lats_deg) == local_count, 'LocStreamGeomSpec coordinate size mismatch')

         ! Convert from degrees to radians for the LocStream
         tlons = lons_deg * MAPL_PI_R8 / 180.0_ESMF_KIND_R8
         tlats = lats_deg * MAPL_PI_R8 / 180.0_ESMF_KIND_R8

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

      integer :: status

      ! LocStream-specific file metadata generation can be added later.
      file_metadata = FileMetadata()
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

      integer :: status

      call gridded_dims%push_back("loc")

      _RETURN(_SUCCESS)
   end function make_gridded_dims

   function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      class(LocStreamGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      type(StringDictionary) :: variable_attributes
      integer, optional, intent(out) :: rc

      integer :: status

      variable_attributes = StringDictionary()

      _RETURN(_SUCCESS)
   end function make_variable_attributes

end module mapl3g_LocStreamGeomFactory
