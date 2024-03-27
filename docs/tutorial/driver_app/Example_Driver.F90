#define I_AM_MAIN

#include "MAPL_Generic.h"

program Example_Driver
   use MPI
   use MAPL
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cap_options = FargparseCLI()
   cap = MAPL_Cap('example', cap_options = cap_options)
   call cap%run(_RC)

end program Example_Driver

