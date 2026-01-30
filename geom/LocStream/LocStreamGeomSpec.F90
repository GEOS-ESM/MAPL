#include "MAPL_ErrLog.h"

module mapl3g_LocStreamGeomSpec
   use mapl3g_GeomSpec
   implicit none(type,external)
   private

   public :: LocStreamGeomSpec

   type, extends(GeomSpec) :: LocStreamGeomSpec
      private
      ! For now, just store the number of points; coordinates
      ! may be obtained from PFIO metadata or hconfig as needed.
      integer :: npoints = 0
   contains
      procedure :: equal_to
      procedure, public :: get_npoints
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
