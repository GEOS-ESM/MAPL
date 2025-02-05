#define I_AM_MAIN

#include "MAPL_Generic.h"

program driver_MAPL_GetHorzIJIndex
   use MAPL
   use MAPL_GetHorzIJIndex_mod, only: SetServices
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_FargparseCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FargparseCLI()
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('GetHorzIJ', SetServices, cap_options = cap_options)
   call cap%run(_RC)

end program driver_MAPL_GetHorzIJIndex
