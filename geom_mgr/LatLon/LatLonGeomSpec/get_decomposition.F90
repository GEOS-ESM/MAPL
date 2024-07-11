#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) get_decomposition_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none
   
contains

   pure module function get_decomposition(spec) result(decomposition)
      type(LatLonDecomposition) :: decomposition
      class(LatLonGeomSpec), intent(in) :: spec

      decomposition = spec%decomposition
   end function get_decomposition

end submodule get_decomposition_smod
