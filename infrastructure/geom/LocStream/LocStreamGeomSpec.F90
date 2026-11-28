#include "MAPL.h"

module mapl_LocStreamGeomSpec_mod

   use mapl_GeomSpec_mod
   use mapl_LocStreamDecomposition_mod
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_Geom
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: LocStreamGeomSpec

   integer, parameter :: R8 = ESMF_KIND_R8

   type, extends(GeomSpec) :: LocStreamGeomSpec
      private
      integer :: npoints = 0
      real(kind=R8), pointer :: lons(:) => null()
      real(kind=R8), pointer :: lats(:) => null()
      type(LocStreamDecomposition) :: decomposition
   contains
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8
      procedure, public :: get_npoints
      procedure, public :: set_coordinates
      procedure, public :: get_coordinates
      procedure, public :: get_decomposition
      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims
      procedure :: make_variable_attributes
   end type LocStreamGeomSpec

   interface LocStreamGeomSpec
      module procedure new_LocStreamGeomSpec
      module procedure new_LocStreamGeomSpec_with_decomp
   end interface LocStreamGeomSpec

contains

   function new_LocStreamGeomSpec(npoints) result(spec)
      type(LocStreamGeomSpec) :: spec
      integer, intent(in) :: npoints
      spec%npoints = npoints
      ! Create default decomposition (will be set properly later)
      spec%decomposition = LocStreamDecomposition([npoints])
   end function new_LocStreamGeomSpec

   function new_LocStreamGeomSpec_with_decomp(npoints, decomposition) result(spec)
      type(LocStreamGeomSpec) :: spec
      integer, intent(in) :: npoints
      type(LocStreamDecomposition), intent(in) :: decomposition
      spec%npoints = npoints
      spec%decomposition = decomposition
   end function new_LocStreamGeomSpec_with_decomp

   integer function get_npoints(this) result(n)
      class(LocStreamGeomSpec), intent(in) :: this
      n = this%npoints
   end function get_npoints

   subroutine set_coordinates(this, lons_in, lats_in)
      class(LocStreamGeomSpec), intent(inout) :: this
      real(kind=R8), intent(in) :: lons_in(:)
      real(kind=R8), intent(in) :: lats_in(:)

      integer :: status

      if (associated(this%lons)) deallocate(this%lons)
      if (associated(this%lats)) deallocate(this%lats)

      if (size(lons_in) /= size(lats_in)) then
         stop "LocStreamGeomSpec::set_coordinates - lon/lat size mismatch"
      end if

      allocate(this%lons(size(lons_in)), stat=status)
      if (status /= 0) stop "LocStreamGeomSpec::set_coordinates - alloc lons failed"
      allocate(this%lats(size(lats_in)), stat=status)
      if (status /= 0) stop "LocStreamGeomSpec::set_coordinates - alloc lats failed"

      this%lons = lons_in
      this%lats = lats_in
   end subroutine set_coordinates

   subroutine get_coordinates(this, lons_out, lats_out)
      class(LocStreamGeomSpec), intent(in) :: this
      real(kind=R8), pointer, intent(out) :: lons_out(:)
      real(kind=R8), pointer, intent(out) :: lats_out(:)

      lons_out => this%lons
      lats_out => this%lats
   end subroutine get_coordinates

   pure function get_decomposition(this) result(decomposition)
      type(LocStreamDecomposition) :: decomposition
      class(LocStreamGeomSpec), intent(in) :: this

      decomposition = this%decomposition
   end function get_decomposition

   logical function equal_to(a, b)
      class(LocStreamGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      equal_to = .false.
      select type (b)
      type is (LocStreamGeomSpec)
         equal_to = (a%npoints == b%npoints)
      class default
         equal_to = .false.
      end select

   end function equal_to

   subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, geom, rc)
      class(LocStreamGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)
      _FAIL('get_horz_ij_index is not supported for LocStreamGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, geom, rc)
      class(LocStreamGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)
      _FAIL('get_horz_ij_index is not supported for LocStreamGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r8

   function make_geom(this, rc) result(geom)
      use esmf
      use MAPL_Constants, only: MAPL_PI_R8
      class(LocStreamGeomSpec), intent(in) :: this
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

      ! Get the decomposition and current PE's rank
      decomp = this%get_decomposition()
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=local_pet, _RC)

      ! Determine local indices for this PE
      call decomp%get_local_indices(local_pet, i_0, i_1)

      local_count = i_1 - i_0 + 1

      allocate(tlons(local_count))
      allocate(tlats(local_count))

      call this%get_coordinates(lons_deg, lats_deg)
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

      _RETURN(_SUCCESS)
   end function make_geom

   function make_file_metadata(this, unusable, chunksizes, rc) &
        result(file_metadata)
      use pfio, only: FileMetadata
      use mapl_KeywordEnforcer_mod
      class(LocStreamGeomSpec), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      type(FileMetadata) :: file_metadata
      integer, optional, intent(out) :: rc

      ! LocStream-specific file metadata generation can be added later.
      file_metadata = FileMetadata()
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)

      _RETURN(_SUCCESS)
   end function make_file_metadata

   function make_gridded_dims(this, rc) result(gridded_dims)
      use gftl2_StringVector, only: StringVector
      class(LocStreamGeomSpec), intent(in) :: this
      type(StringVector) :: gridded_dims
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)

      call gridded_dims%push_back("loc")

      _RETURN(_SUCCESS)
   end function make_gridded_dims

   function make_variable_attributes(this, rc) result(variable_attributes)
      use mapl_StringDictionary_mod, only: StringDictionary
      class(LocStreamGeomSpec), intent(in) :: this
      type(StringDictionary) :: variable_attributes
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)

      variable_attributes = StringDictionary()

      _RETURN(_SUCCESS)
   end function make_variable_attributes

end module mapl_LocStreamGeomSpec_mod
