#include "MAPL_ErrLog.h"

! NullGeomSpec is used to return a concrete object fore failing
! factory methods that return GeomSpec objects.
module mapl3g_NullGeomSpec
   use mapl3g_GeomSpec
   implicit none(type,external)
   private

   public :: NULL_GEOM_SPEC

   type, extends(GeomSpec) :: NullGeomSpec
   contains
      procedure :: equal_to
   end type NullGeomSpec

   type(NullGeomSpec), protected :: NULL_GEOM_SPEC

contains

   logical function equal_to(a, b)
      class(NullGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b
      equal_to = .false.
      _UNUSED_DUMMY(a)
      _UNUSED_DUMMY(b)
   end function equal_to

end module mapl3g_NullGeomSpec
