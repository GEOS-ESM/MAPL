#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_timespec_smod
   use mapl3g_HConfig_API
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

         has_timestep = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIMESTEP, _RC)
         _RETURN_UNLESS(has_timestep)

         timestep = mapl_HConfigAsTimeInterval(hconfig, keystring=KEY_TIMESTEP, _RC)

         _RETURN(_SUCCESS)
      end subroutine parse_timestep

      subroutine parse_offset(hconfig, offset, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), allocatable, intent(out) :: offset
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_offset

         has_offset = ESMF_HConfigIsDefined(hconfig, keyString=KEY_RUN_TIME_OFFSET, _RC)
         _RETURN_UNLESS(has_offset)

         offset = mapl_HConfigAsTimeInterval(hconfig, keystring=KEY_TIMESTEP, _RC)

         _RETURN(_SUCCESS)

      end subroutine parse_offset

   end submodule parse_timespec_smod
