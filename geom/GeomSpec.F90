#include "MAPL.h"

module mapl3g_GeomSpec
   use esmf
   implicit none(type,external)
   private

   public :: GeomSpec

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

end module mapl3g_GeomSpec
