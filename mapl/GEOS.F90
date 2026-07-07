#define I_AM_MAIN
#include "MAPL.h"

program geos
   use MAPL
   implicit none

   integer :: status
   type(MAPL_GriddedComponentDriver) :: driver
   type(ESMF_GridComp), allocatable :: servers(:)

   call MAPL_Initialize(configFileNameFromArgNum=1, _RC)
   call MAPL_CreateServers(servers, _RC)
   call MAPL_CreateCap(driver, _RC)
   call MAPL_RunServers(servers, _RC)
   call MAPL_RunCap(driver, _RC)
   call MAPL_Finalize(_RC)

end program geos
