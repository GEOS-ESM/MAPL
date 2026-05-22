#include "MAPL.h"

submodule (mapl_LatLonGeomSpec) make_LatLonGeomSpec_from_hconfig_smod
   use mapl_CoordinateAxis_mod
   use mapl_GeomSpec_mod
   use mapl_ErrorHandling_mod
   use esmf
   implicit none (type, external)

contains

   ! HConfig section
   module function make_LatLonGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      spec%lon_axis = make_LonAxis(hconfig, _RC)
      spec%lat_axis = make_LatAxis(hconfig, _RC)
      associate (im => spec%lon_axis%get_extent(), jm => spec%lat_axis%get_extent())
        spec%decomposition = make_Decomposition(hconfig, dims=[im,jm], _RC)
      end associate

      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_hconfig

end submodule make_LatLonGeomSpec_from_hconfig_smod
