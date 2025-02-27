#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timespec_smod
   implicit none(type,external)
   
contains

      module subroutine parse_timespec(hconfig, timeStep, offset, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), allocatable, intent(out) :: timeStep
         type(ESMF_TimeInterval), allocatable, intent(out) :: offset
         integer, optional, intent(out) :: rc
         
         integer :: status

         call parse_timestep(hconfig, timeStep, _RC)
         call parse_offset(hconfig, offset, _RC)

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

         has_timestep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         _RETURN_UNLESS(has_timestep)

         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_TIMESTEP, _RC)
         call ESMF_TimeIntervalSet(interval, timeIntervalString=iso_duration, _RC)
         timeStep = interval

         _RETURN(_SUCCESS)
      end subroutine parse_timestep

      subroutine parse_offset(hconfig, offset, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), allocatable, intent(out) :: offset
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_offset
         character(len=32) :: iso_duration
         type(ESMF_TimeInterval) :: duration

         has_offset = ESMF_HConfigIsDefined(hconfig, keyString=KEY_RUN_TIME_OFFSET, _RC)
         _RETURN_UNLESS(has_offset)

         iso_duration = ESMF_HConfigAsString(hconfig, keyString=KEY_RUN_TIME_OFFSET, _RC)
         call ESMF_TimeIntervalSet(duration, timeIntervalString=iso_duration, _RC)
         offset = duration

         _RETURN(_SUCCESS)

      end subroutine parse_offset

   end submodule parse_timespec_smod
