#define I_AM_MAIN

#include "MAPL_Generic.h"

program Example_Driver
   use MPI
   use MAPL
   use mapl_CapOptionsMod, only: MAPL_CapOptions
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_FargparseCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FargparseCLI()
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('example', cap_options = cap_options)
   call cap%run(_RC)

end program Example_Driver

