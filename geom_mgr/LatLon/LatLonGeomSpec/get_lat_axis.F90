#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) get_lat_axis_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none
   
contains

   pure module function get_lat_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LatAxis) :: axis
      axis = spec%lat_axis
   end function get_lat_axis

end submodule get_lat_axis_smod
