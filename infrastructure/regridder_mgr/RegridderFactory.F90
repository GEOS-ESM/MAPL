#include "MAPL.h"

module mapl_RegridderFactory_mod
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
         use mapl_RegridderParam_mod
         import :: RegridderFactory
         class(RegridderFactory), intent(in) :: this
         class(RegridderParam), intent(in) :: param
      end function I_supports

      function I_make_regridder_typesafe(this, spec, rc) result(regriddr)
         use mapl_RegridderSpec_mod
         use mapl_Regridder_mod
         import :: RegridderFactory
         class(Regridder), allocatable :: regriddr
         class(RegridderFactory), intent(inout) :: this
         type(RegridderSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function I_make_regridder_typesafe

   end interface

end module mapl_RegridderFactory_mod
   
