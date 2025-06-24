#define I_AM_MAIN

#include "MAPL_Generic.h"

program CapDriver_Main
   use MPI
   use MAPL
   use ExtDataUtRoot_GridCompMod, only:  ROOT_SetServices => SetServices
   implicit none

   character(len=*), parameter :: Iam="CapDriver_Main"
   type (MAPL_Cap) :: cap
   type (MAPL_FargparseCLI) :: cli
   type (MAPL_CapOptions) :: cap_options
   integer :: status

   cli = MAPL_FargparseCLI()
   cap_options = MAPL_CapOptions(cli)
   cap = MAPL_Cap('Root', ROOT_SetServices, cap_options = cap_options)

   call cap%run(__RC)

end program CapDriver_Main

