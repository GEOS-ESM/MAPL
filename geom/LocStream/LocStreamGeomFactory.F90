#include "MAPL_ErrLog.h"

module mapl3g_LocStreamGeomFactory
   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_LocStreamGeomSpec
   use mapl_ErrorHandlingMod
   use mapl_StringUtilities, only: to_lower
   use mapl3g_get_hconfig, only: get_hconfig
   use mapl3g_hconfig_params, only: HConfigParams
   use pfio_FileMetadataMod, only: FileMetadata
   use gftl2_StringVector, only: StringVector
   use mapl3g_StringDictionary, only: StringDictionary
   use mapl_KeywordEnforcerMod, only: KeywordEnforcer
   use esmf
   use iso_fortran_env, only: REAL64
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

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig

   function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LocStreamGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      ! Metadata-driven LocStream specs are not yet supported.
      ! Return a trivial spec for now and rely on supports_metadata
      ! being false so this factory is not selected.
      allocate(LocStreamGeomSpec :: geom_spec)
      geom_spec = LocStreamGeomSpec(0)

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

      ! Metadata-based LocStream detection is not implemented yet.
      _UNUSED_DUMMY(file_metadata)
      supports = .false.

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
      real(kind=REAL64), allocatable :: tlons(:), tlats(:)

      _HERE
      select type (geom_spec)
      type is (LocStreamGeomSpec)
         _HERE
         local_count = geom_spec%get_npoints()

         allocate(tlons(local_count), stat=status)
         _VERIFY(status)
         allocate(tlats(local_count), stat=status)
         _VERIFY(status)

         tlons = 0.0_REAL64
         tlats = 0.0_REAL64

         _HERE
         locstream = ESMF_LocStreamCreate(localCount=local_count, coordSys=ESMF_COORDSYS_SPH_RAD, _RC)
         _HERE
         call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lat", farray=tlats, datacopyflag=ESMF_DATACOPY_VALUE, &
              keyUnits="Radians", keyLongName="Latitude", _RC)
         _HERE
         call ESMF_LocStreamAddKey(locstream, keyName="ESMF:Lon", farray=tlons, datacopyflag=ESMF_DATACOPY_VALUE, &
              keyUnits="Radians", keyLongName="Longitude", _RC)

         _HERE
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
