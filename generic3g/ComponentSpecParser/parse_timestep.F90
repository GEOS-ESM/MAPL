#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timestep_smod
contains

      module subroutine parse_timestep(hconfig, timestep, reference_time, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), intent(inout) :: timestep
         type(ESMF_Time), intent(inout) :: reference_time
         integer, optional, intent(out) :: rc
         
         integer :: status
         logical :: has_timestep, has_reference_time
         character(len=32) :: iso_datetime
         character(len=128) :: iso_duration
         type(ESMF_Time) :: time
         type(ESMF_TimeInterval) :: interval

         has_timestep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         has_reference_time = ESMF_HConfigIsDefined(hconfig, keyString=KEY_REFERENCE_TIME, _RC)
         if(has_timestep) then
            iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP, _RC)
            call ESMF_TimeIntervalSet(interval, timeIntervalString=iso_duration, _RC)
            timestep = interval
         end if

         _RETURN_UNLESS(has_reference_time)
         iso_datetime = ESMF_HConfigAsString(hconfig, keyString=KEY_REFERENCE_TIME, _RC)
         call ESMF_TimeSet(time, timeString=iso_datetime, _RC)
         reference_time = time

         _RETURN(_SUCCESS)

      end subroutine parse_timestep

end submodule parse_timestep_smod
