#include "MAPL_Generic.h"

module mapl_RegridderFactory
   implicit none

   private

   public :: RegridderFactory

   type, abstract :: RegridderFactory
   contains
      procedure(I_supports), deferred :: supports
      procedure(I_make_regridder), deferred :: make_regridder
   end type RegridderFactory

   abstract interface

      logical function I_supports(this, spec)
         use mapl_RegridderSpec
         import :: RegridderFactory
         class(RegridderFactory), intent(in) :: this
         class(RegridderSpec), intent(in) :: spec
      end function I_supports

      function I_make_regridder(this, spec, rc) result(regriddr)
         use mapl_RegridderSpec
         use mapl_Regridder
         import :: RegridderFactory
         class(Regridder), allocatable :: regriddr
         class(RegridderFactory), intent(in) :: this
         class(RegridderSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function I_make_regridder

   end interface

end module mapl_RegridderFactory
   
