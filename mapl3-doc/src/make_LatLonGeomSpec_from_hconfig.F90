#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) make_LatLonGeomSpec_from_hconfig_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   ! HConfig section
   module function make_LatLonGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      logical :: is_regional
      integer :: status

      spec%lon_axis = make_LonAxis(hconfig, _RC)
      spec%lat_axis = make_LatAxis(hconfig, _RC)
      associate (im => spec%lon_axis%get_extent(), jm => spec%lat_axis%get_extent())
        spec%decomposition = make_Decomposition(hconfig, dims=[im,jm], _RC)
      end associate

      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_hconfig

end submodule make_LatLonGeomSpec_from_hconfig_smod
