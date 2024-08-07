#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) get_lon_axis_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none
   
contains

   ! Accessors
   pure module function get_lon_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LonAxis) :: axis
      axis = spec%lon_axis
   end function get_lon_axis

end submodule get_lon_axis_smod
