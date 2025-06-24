#define I_AM_MAIN

#include "MAPL_Generic.h"

program Example_Driver
   use MPI
   use MAPL
#ifdef __NVCOMPILER
   ! Needed by NVIDIA but breaks Intel (see https://github.com/GEOS-ESM/MAPL/pull/2664)
   use mapl_CapOptionsMod, only: MAPL_CapOptions
#endif
   implicit none

   type (MAPL_Cap) :: cap
   type (MAPL_FargparseCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FargparseCLI()
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('example', cap_options = cap_options)
   call cap%run(__RC)

end program Example_Driver

