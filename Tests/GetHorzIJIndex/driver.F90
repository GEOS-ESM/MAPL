#define I_AM_MAIN

#include "MAPL_Generic.h"

program driver_GetHorzIJIndex
   use MAPL
   use GridComp, only: SetServices
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_FargparseCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FargparseCLI()
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('GetHorzIJIndex', SetServices, cap_options = cap_options, __RC)
   call cap%run(__RC)

end program driver_GetHorzIJIndex
