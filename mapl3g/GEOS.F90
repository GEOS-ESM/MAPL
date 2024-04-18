#define I_AM_MAIN
#include "MAPL_Generic.h"

program geos
   use mapl3
   use esmf
   implicit none

   integer :: status
   type(ESMF_Config) :: config

   call initialize(config=config, _RC)
   call run_geos(config, _RC)
   call finalize(config=config, _RC)

contains

#undef I_AM_MAIN
#include "MAPL_Generic.h"

   subroutine initialize(config, rc)
      type(ESMF_Config), intent(out) :: config
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_HConfig) :: hconfig, mapl_hconfig
      logical :: has_mapl_section

      call ESMF_Initialize(configFilenameFromArgNum=1, configKey=['esmf'], config=config, _RC)
      call ESMF_ConfigGet(config, hconfig=hconfig, _RC)
      has_mapl_section = ESMF_HConfigIsDefined(hconfig, keystring='mapl', _RC)
      if (has_mapl_section) then
         mapl_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='mapl', _RC)
      end if
      call MAPL_Initialize(mapl_hconfig=mapl_hconfig, _RC)
      call ESMF_HConfigDestroy(mapl_hconfig, _RC)
 
   end subroutine initialize

   subroutine run_geos(config, rc)
      type(ESMF_Config), intent(inout) :: config
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: cap_hconfig
      integer :: status

      call ESMF_ConfigGet(config, hconfig=cap_hconfig, _RC)
      call MAPL_run_driver(cap_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine run_geos

   subroutine finalize(config, rc)
      type(ESMF_Config), intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      
      call MAPL_Finalize(_RC)
      call ESMF_ConfigDestroy(config, _RC)
      call ESMF_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine finalize

end program geos
