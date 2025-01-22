#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timestep_smod
   use MAPL_TimeStringConversion, only: parse_isostring => string_to_esmf_timeinterval
contains

      module subroutine parse_timestep(hconfig, timestep, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), intent(inout) :: timestep
         integer, optional, intent(out) :: rc
         
         integer :: status
         logical :: has_timestep
         character(len=:), allocatable :: iso_duration

         has_timestep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         _RETURN_UNLESS(has_timestep)

         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP, _RC)
         timestep = parse_isostring(iso_duration, _RC)

         _RETURN(_SUCCESS)

      end subroutine parse_timestep

end submodule parse_timestep_smod
