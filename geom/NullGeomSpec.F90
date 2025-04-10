! NullGeomSpec is used to return a concrete object fore failing
! factory methods that return GeomSpec objects.
module mapl3g_NullGeomSpec
   use mapl3g_GeomSpec
   implicit none

   type, extends(GeomSpec) :: NullGeomSpec
   contains
      procedure :: equal_to
   end type NullGeomSpec

contains

   logical function equal_to(a, b)
      class(NullGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b
      equal_to = .false.
   end function equal_to

end module mapl3g_NullGeomSpec
