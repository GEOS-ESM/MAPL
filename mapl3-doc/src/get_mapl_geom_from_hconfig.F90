#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) get_mapl_geom_from_hconfig_smod

   implicit none

contains
   
   module function get_mapl_geom_from_hconfig(this, hconfig, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      class(GeomSpec), allocatable :: geom_spec
      integer :: status

      geom_spec = this%make_geom_spec(hconfig, _RC)
      mapl_geom => this%get_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_hconfig

end submodule get_mapl_geom_from_hconfig_smod
