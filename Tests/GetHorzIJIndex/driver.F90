#define I_AM_MAIN

#include "MAPL.h"

program driver_GetHorzIJIndex
   use MAPL
   use GridComp, only: SetServices
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cap_options = FargparseCLI()
   cap = MAPL_Cap('GetHorzIJIndex', SetServices, cap_options = cap_options, _RC)
   call cap%run(_RC)

end program driver_GetHorzIJIndex
