#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timestep_smod
contains

      module subroutine parse_timestep(hconfig, timestep, start, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), allocatable, intent(inout) :: timestep
         type(ESMF_Time), allocatable, intent(inout) :: start
         integer, optional, intent(out) :: rc
         
         integer :: status
         logical :: has_timestep, has_start
         character(len=32) :: iso_datetime
         character(len=128) :: iso_duration
         type(ESMF_Time) :: time
         type(ESMF_TimeInterval) :: interval

         has_timestep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         _RETURN_UNLESS(has_timestep)
         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP, _RC)
!         timestep = string_to_esmf_timeinterval(trim(iso_duration), _RC)
         call ESMF_TimeIntervalSet(interval, timeIntervalString=iso_duration, _RC)
         allocate(timestep, source=interval)

         has_start = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP_START, _RC)
         _RETURN_UNLESS(has_start)
         iso_datetime = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP_START, _RC)
!         start = string_to_esmf_time(iso_datetime, _RC)
         call ESMF_TimeSet(time, timeString=iso_datetime, _RC)
         allocate(start, source=time)

         _RETURN(_SUCCESS)

      end subroutine parse_timestep

end submodule parse_timestep_smod
