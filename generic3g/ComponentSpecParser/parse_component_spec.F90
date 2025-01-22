#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_component_spec_smod
   
contains

   module function parse_component_spec(hconfig, registry, refTime, timestep, rc) result(spec)
      type(ComponentSpec) :: spec
      type(ESMF_HConfig), target, intent(inout) :: hconfig
      type(StateRegistry), target, intent(in) :: registry
      type(ESMF_Time), intent(in) :: refTime ! default
      type(ESMF_TimeInterval), intent(in) :: timestep ! default
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_mapl_section
      type(ESMF_HConfig) :: mapl_cfg

      spec%timestep = timestep

      has_mapl_section = ESMF_HConfigIsDefined(hconfig, keyString=MAPL_SECTION, _RC)
      _RETURN_UNLESS(has_mapl_section)
      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)

      call parse_timestep(mapl_cfg, spec%timestep, _RC)
      spec%geometry_spec = parse_geometry_spec(mapl_cfg, registry, _RC)
      spec%var_specs = parse_var_specs(mapl_cfg, spec%timestep, _RC)
      spec%connections = parse_connections(mapl_cfg, _RC)
      spec%children = parse_children(mapl_cfg, _RC)

      call ESMF_HConfigDestroy(mapl_cfg, _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec

end submodule parse_component_spec_smod

