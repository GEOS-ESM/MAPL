#include "MAPL_Generic.h"

module mapl3g_GeomSpec
   use esmf
   implicit none
   private

   public :: GeomSpec
   public :: NULL_GEOM_SPEC

   type, abstract :: GeomSpec
      private
   contains
      procedure(I_equal_to), deferred :: equal_to
      generic :: operator(==) => equal_to
   end type GeomSpec


   abstract interface
      logical function I_equal_to(a, b)
         import GeomSpec
         class(GeomSpec), intent(in) :: a
         class(GeomSpec), intent(in) :: b
      end function I_equal_to
   end interface


   type, extends(GeomSpec) :: NullGeomSpec
   contains
      procedure :: equal_to => false
   end type NullGeomSpec

   type(NullGeomSpec) :: NULL_GEOM_SPEC

contains

   logical function false(a,b)
      class(NullGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b
      false = .false.
   end function false
   
end module mapl3g_GeomSpec
