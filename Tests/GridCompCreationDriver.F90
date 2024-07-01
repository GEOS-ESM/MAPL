#define I_AM_MAIN
#include "MAPL_Generic.h"
program grid_comp_creation_driver

!_   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   use MPI
   use ESMF
   use gridcomp_creation_cap
   use MAPL

   implicit none
!_ INTERFACES
!_ TYPES
!_ VARIABLES
   integer :: status
   character(len=:), parameter :: Iam="GridCompCreationDriver"
   character(len=:), parameter :: CAPNAME = 'GridCompCreationCap'
   type(MAPL_CapOptions) :: cap_options
   type(MAPL_FargparseCLI) :: cli

   cli = MAPL_FargparseCLI()
   cap_options = MAPL_CapOptions(CLI)

   call initialize_cap(CAPNAME, Root_SetServices, options=cap_options, _RC) 
   call run_cap(_RC)
   stop

!_contains
end program grid_comp_creation_driver
