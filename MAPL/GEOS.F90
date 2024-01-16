#define I_AM_MAIN
#include "MAPL_Generic.h"

program geos
   use mapl3g
   use mapl_ErrorHandling
   use esmf
   implicit none

   integer :: status
   type(ESMF_Config) :: config
   type(ESMF_HConfig) :: hconfig

   call ESMF_Initialize(configFileNameFromArgNum=1, configKey=['esmf'], config=config, _RC)
   call ESMF_ConfigGet(config, hconfig=hconfig, _RC)
   call run_geos(hconfig, _RC)
   call ESMF_Finalize(_RC)

contains

#undef I_AM_MAIN
#include "MAPL_Generic.h"

   subroutine run_geos(hconfig, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc
      integer :: status

!#      call MAPL_initialize(hconfig, _RC)
!#      call MAPL_run_driver(hconfig, _RC)
!#      call MAPL_finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine run_geos

end program geos
