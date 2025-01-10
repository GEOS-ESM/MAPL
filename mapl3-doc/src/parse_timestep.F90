#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timestep_smod
   use MAPL_TimeStringConversion, only: parse_isostring => string_to_esmf_timeinterval
contains

      module function parse_timestep(hconfig, rc) result(timestep)
         type(ESMF_TimeInterval) :: timestep
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
         
         integer :: status
         logical :: has_timestep
         character(len=:), allocatable :: iso_duration

         has_timestep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         _RETURN_UNLESS(has_timestep)
         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP, _RC)
         timestep = parse_isostring(iso_duration, _RC)
         _RETURN(_SUCCESS)

      end function parse_timestep

end submodule parse_timestep_smod
