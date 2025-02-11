#include "MAPL_ErrLog.h"


submodule (mapl3g_ComponentSpecParser) parse_component_spec_smod
   implicit none(type,external)

contains

   module function parse_component_spec(hconfig, registry, timeStep, refTime, rc) result(spec)
      type(ComponentSpec) :: spec
      type(ESMF_HConfig), target, intent(inout) :: hconfig
      type(StateRegistry), target, intent(in) :: registry
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_Time), optional, intent(in) :: refTime
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_mapl_section
      type(ESMF_HConfig) :: mapl_cfg

      has_mapl_section = ESMF_HConfigIsDefined(hconfig, keyString=MAPL_SECTION, _RC)
      _RETURN_UNLESS(has_mapl_section)
      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)

      spec%geometry_spec = parse_geometry_spec(mapl_cfg, registry, _RC)
      spec%var_specs = parse_var_specs(mapl_cfg, timeStep, refTime, _RC)
      spec%connections = parse_connections(mapl_cfg, _RC)
      spec%children = parse_children(mapl_cfg, _RC)

      call parse_misc(spec, mapl_cfg, _RC)

      call ESMF_HConfigDestroy(mapl_cfg, _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec

   ! TODO - we may want a `misc` section in the mapl section, but
   ! should wait to see what else goes there.  Or maybe a `test`
   ! section?
   
   subroutine parse_misc(spec, hconfig, rc)
      type(ComponentSpec), intent(inout) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_activate_all_exports

      has_activate_all_exports = ESMF_HConfigIsDefined(hconfig,keyString=COMPONENT_ACTIVATE_ALL_EXPORTS, _RC)
      if (has_activate_all_exports) then
         spec%activate_all_exports = ESMF_HConfigASLogical(hconfig, keyString=COMPONENT_ACTIVATE_ALL_EXPORTS, _RC)
      end if


      _RETURN(_SUCCESS)
   end subroutine parse_misc


end submodule parse_component_spec_smod

