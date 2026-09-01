#define I_AM_MAIN
#include "MAPL.h"

program geos
   use MAPL
   use mapl_CapDriver_mod, only: MAPL_CapCreate, MAPL_CapRun
   use esmf
   implicit none

   integer :: status
   type(MAPL_GriddedComponentDriver) :: driver
   type(ESMF_GridComp), allocatable :: servers(:)

   call MAPL_Initialize(configFileNameFromArgNum=1, _RC)
   call MAPL_CreateServers(servers, _RC)
   call MAPL_CapCreate(driver, _RC)
   call MAPL_RunServers(servers, _RC)
   call MAPL_CapRun(driver, _RC)
   call MAPL_Finalize(_RC)

end program geos
