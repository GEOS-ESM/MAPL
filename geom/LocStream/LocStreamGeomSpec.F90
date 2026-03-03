#include "MAPL_ErrLog.h"

module mapl3g_LocStreamGeomSpec
   use mapl3g_GeomSpec
   use esmf, only: ESMF_KIND_R8
   implicit none(type,external)
   private

   public :: LocStreamGeomSpec

   integer, parameter :: R8 = ESMF_KIND_R8

   type, extends(GeomSpec) :: LocStreamGeomSpec
      private
      integer :: npoints = 0
      real(kind=R8), pointer :: lons(:) => null()
      real(kind=R8), pointer :: lats(:) => null()
   contains
      procedure :: equal_to
      procedure, public :: get_npoints
      procedure, public :: set_coordinates
      procedure, public :: get_coordinates
   end type LocStreamGeomSpec

   interface LocStreamGeomSpec
      module procedure new_LocStreamGeomSpec
   end interface LocStreamGeomSpec

contains

   function new_LocStreamGeomSpec(npoints) result(spec)
      type(LocStreamGeomSpec) :: spec
      integer, intent(in) :: npoints
      spec%npoints = npoints
   end function new_LocStreamGeomSpec

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

end module mapl3g_LocStreamGeomSpec
