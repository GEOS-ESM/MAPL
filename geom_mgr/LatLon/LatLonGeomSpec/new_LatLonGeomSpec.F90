#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) new_LatLonGeomSpec_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none
   
contains

   ! Basic constructor for LatLonGeomSpec
   module function new_LatLonGeomSpec(lon_axis, lat_axis, decomposition) result(spec)
      type(LatLonGeomSpec) :: spec
      type(LonAxis), intent(in) :: lon_axis
      type(LatAxis), intent(in) :: lat_axis
      type(LatLonDecomposition), intent(in) :: decomposition
      
      spec%lon_axis = lon_axis
      spec%lat_axis = lat_axis
      spec%decomposition = decomposition
      
   end function new_LatLonGeomSpec

end submodule new_LatLonGeomSpec_smod
