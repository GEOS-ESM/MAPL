#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timestep_smod
   implicit none(type,external)
   
contains

      module subroutine parse_timespec(hconfig, timestep, refTime, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), allocatable, intent(out) :: timeStep
         type(ESMF_Time), allocatable, intent(out) :: refTime
         integer, optional, intent(out) :: rc
         
         integer :: status

         call parse_timeStep(hconfig, timeStep, _RC)
         call parse_refTime(hconfig, refTime, _RC)

         _RETURN(_SUCCESS)

      end subroutine parse_timespec

      subroutine parse_timestep(hconfig, timeStep, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), allocatable, intent(out) :: timeStep
         integer, optional, intent(out) :: rc
         
         integer :: status
         logical :: has_timestep
         character(len=128) :: iso_duration
         type(ESMF_TimeInterval) :: interval

         has_timeStep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         _RETURN_UNLESS(has_timeStep)

         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP, _RC)
         call ESMF_TimeIntervalSet(interval, timeIntervalString=iso_duration, _RC)
         timestep = interval

         _RETURN(_SUCCESS)
      end subroutine parse_timestep

      subroutine parse_refTime(hconfig, refTime, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_Time), allocatable, intent(out) :: refTime
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_refTime
         character(len=32) :: iso_datetime
         type(ESMF_Time) :: time

         has_refTime = ESMF_HConfigIsDefined(hconfig, keyString=KEY_REFERENCE_TIME, _RC)
         _RETURN_UNLESS(has_refTime)

         iso_datetime = ESMF_HConfigAsString(hconfig, keyString=KEY_REFERENCE_TIME, _RC)
         call ESMF_TimeSet(time, timeString=iso_datetime, _RC)
         refTime = time

         _RETURN(_SUCCESS)

      end subroutine parse_refTime
         

end submodule parse_timestep_smod
