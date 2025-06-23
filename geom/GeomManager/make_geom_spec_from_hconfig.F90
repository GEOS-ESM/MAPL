#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) make_geom_spec_from_hconfig_smod
   use mapl3g_NullGeomSpec, only: NULL_GEOM_SPEC
   implicit none(type,external)

contains
   
   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: status
      
      geom_spec = NULL_GEOM_SPEC
      factory => find_factory(this%factories, supports_hconfig, _RC)
      deallocate(geom_spec)
      geom_spec = factory%make_spec(hconfig, _RC)
      
      _RETURN(_SUCCESS)
   contains
      logical function supports_hconfig(factory)
         class(GeomFactory), intent(in) :: factory
         supports_hconfig = factory%supports(hconfig)
      end function supports_hconfig
   end function make_geom_spec_from_hconfig

end submodule make_geom_spec_from_hconfig_smod
