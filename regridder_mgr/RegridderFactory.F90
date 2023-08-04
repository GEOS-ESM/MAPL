#include "MAPL_Generic.h"

module mapl_RegridderFactory
   implicit none
   private

   public :: RegridderFactory

   type, abstract :: RegridderFactory
   contains
      procedure(I_supports), deferred :: supports
      procedure(I_make_regridder_typesafe), deferred :: make_regridder_typesafe
      generic :: make_regridder => make_regridder_typesafe
   end type RegridderFactory

   abstract interface

      logical function I_supports(this, param)
         use mapl_RegridderParam
         import :: RegridderFactory
         class(RegridderFactory), intent(in) :: this
         class(RegridderParam), intent(in) :: param
      end function I_supports

      function I_make_regridder_typesafe(this, spec, rc) result(regriddr)
         use mapl_RegridderSpec
         use mapl_Regridder
         import :: RegridderFactory
         class(Regridder), allocatable :: regriddr
         class(RegridderFactory), intent(in) :: this
         type(RegridderSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function I_make_regridder_typesafe

   end interface

end module mapl_RegridderFactory
   
