#include "MAPL.h"

module mapl3g_LocStreamGeomSpec

   use mapl3g_GeomSpec
   use mapl3g_LocStreamDecomposition
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use mapl_ErrorHandling

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

   subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(LocStreamGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)
      _FAIL('get_horz_ij_index is not supported for LocStreamGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(LocStreamGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)
      _FAIL('get_horz_ij_index is not supported for LocStreamGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r8

end module mapl3g_LocStreamGeomSpec
