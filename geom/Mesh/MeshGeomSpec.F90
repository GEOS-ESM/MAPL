#include "MAPL_ErrLog.h"

module mapl3g_MeshGeomSpec
   use mapl3g_GeomSpec
   use esmf, only: ESMF_KIND_R8
   implicit none(type,external)
   private

   public :: MeshGeomSpec

   integer, parameter :: R8 = ESMF_KIND_R8

   type, extends(GeomSpec) :: MeshGeomSpec
      private
      integer :: numElements = 0
      integer :: numConns    = 0
      integer :: numNodes    = 0
      real(kind=R8), pointer :: lons(:) => null()
      real(kind=R8), pointer :: lats(:) => null()
   contains
      procedure :: equal_to
      procedure, public :: get_numElements
      procedure, public :: get_numConns
      procedure, public :: get_numNodes
      procedure, public :: set_coordinates
      procedure, public :: get_coordinates
   end type MeshGeomSpec

   interface MeshGeomSpec
      module procedure new_MeshGeomSpec
   end interface MeshGeomSpec

contains

   function new_MeshGeomSpec(numElements, numConns, numNodes) result(spec)
      type(MeshGeomSpec) :: spec
      integer, intent(in) :: numElements
      integer, intent(in) :: numConns
      integer, intent(in) :: numNodes
      spec%numElements = numElements
      spec%numConns    = numConns
      spec%numNodes    = numNodes
   end function new_MeshGeomSpec

   integer function get_numElements(this) result(n)
      class(MeshGeomSpec), intent(in) :: this
      n = this%numElements
   end function get_numElements

   integer function get_numConns(this) result(n)
      class(MeshGeomSpec), intent(in) :: this
      n = this%numConns
   end function get_numConns

   integer function get_numNodes(this) result(n)
      class(MeshGeomSpec), intent(in) :: this
      n = this%numNodes
   end function get_numNodes

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

   logical function equal_to(a, b)
      class(MeshGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      equal_to = .false.
      select type (b)
      type is (MeshGeomSpec)
         equal_to = (a%numElements == b%numElements .and. a%numConns == b%numConns .and. a%numNodes == b%numNodes )
      class default
         equal_to = .false.
      end select

   end function equal_to

end module mapl3g_MeshGeomSpec
