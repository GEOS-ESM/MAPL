#define I_AM_MAIN

#include "MAPL_Generic.h"

program Example_Driver
   use MPI
   use MAPL
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_FlapCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FlapCLI(description = 'GEOS AGCM', &
                                     authors     = 'GMAO')
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('example', cap_options = cap_options)
   call cap%run(_RC)

end program Example_Driver

