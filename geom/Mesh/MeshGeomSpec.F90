#include "MAPL_ErrLog.h"

module mapl3g_MeshGeomSpec
   use mapl3g_GeomSpec
   use mapl3g_MeshDecomposition
   use esmf, only: ESMF_KIND_R8
   implicit none(type,external)
   private

   public :: MeshGeomSpec

   integer, parameter :: R8 = ESMF_KIND_R8

   type, extends(GeomSpec) :: MeshGeomSpec
      private
      integer :: npoints = 0
      real(kind=R8), pointer :: lons(:) => null()
      real(kind=R8), pointer :: lats(:) => null()
      type(MeshDecomposition) :: decomposition
   contains
      procedure :: equal_to
      procedure, public :: get_npoints
      procedure, public :: set_coordinates
      procedure, public :: get_coordinates
      procedure, public :: get_decomposition
   end type MeshGeomSpec

   interface MeshGeomSpec
      module procedure new_MeshGeomSpec
      module procedure new_MeshGeomSpec_with_decomp
   end interface MeshGeomSpec

contains

   function new_MeshGeomSpec(npoints) result(spec)
      type(MeshGeomSpec) :: spec
      integer, intent(in) :: npoints
      spec%npoints = npoints
      ! Create default decomposition (will be set properly later)
      spec%decomposition = MeshDecomposition([npoints])
   end function new_MeshGeomSpec

   function new_MeshGeomSpec_with_decomp(npoints, decomposition) result(spec)
      type(MeshGeomSpec) :: spec
      integer, intent(in) :: npoints
      type(MeshDecomposition), intent(in) :: decomposition
      spec%npoints = npoints
      spec%decomposition = decomposition
   end function new_MeshGeomSpec_with_decomp

   integer function get_npoints(this) result(n)
      class(MeshGeomSpec), intent(in) :: this
      n = this%npoints
   end function get_npoints

   subroutine set_coordinates(this, lons_in, lats_in)
      class(MeshGeomSpec), intent(inout) :: this
      real(kind=R8), intent(in) :: lons_in(:)
      real(kind=R8), intent(in) :: lats_in(:)

      integer :: status

      if (associated(this%lons)) deallocate(this%lons)
      if (associated(this%lats)) deallocate(this%lats)

      if (size(lons_in) /= size(lats_in)) then
         stop "MeshGeomSpec::set_coordinates - lon/lat size mismatch"
      end if

      allocate(this%lons(size(lons_in)), stat=status)
      if (status /= 0) stop "MeshGeomSpec::set_coordinates - alloc lons failed"
      allocate(this%lats(size(lats_in)), stat=status)
      if (status /= 0) stop "MeshGeomSpec::set_coordinates - alloc lats failed"

      this%lons = lons_in
      this%lats = lats_in
   end subroutine set_coordinates

   subroutine get_coordinates(this, lons_out, lats_out)
      class(MeshGeomSpec), intent(in) :: this
      real(kind=R8), pointer, intent(out) :: lons_out(:)
      real(kind=R8), pointer, intent(out) :: lats_out(:)

      lons_out => this%lons
      lats_out => this%lats
   end subroutine get_coordinates

   pure function get_decomposition(this) result(decomposition)
      type(MeshDecomposition) :: decomposition
      class(MeshGeomSpec), intent(in) :: this

      decomposition = this%decomposition
   end function get_decomposition

   logical function equal_to(a, b)
      class(MeshGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      equal_to = .false.
      select type (b)
      type is (MeshGeomSpec)
         equal_to = (a%npoints == b%npoints)
      class default
         equal_to = .false.
      end select

   end function equal_to

end module mapl3g_MeshGeomSpec
