#define I_AM_MAIN
#include "MAPL_Generic.h"
program grid_comp_creation_tester

   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   use MPI
   use ESMF
   use gridcomp_creator
   use MAPL
   implicit none

   integer, parameter :: EXPECTED_NARGS = 1
   character(len=:), parameter :: Iam="GridCompCreationDriver"
   integer :: status
   character(len=:), allocatable :: parameter_file
   character(len=MAXSTR) :: raw
   
   _ASSERT(command_argument_count() == EXPECTED_NARGS, 'Unexpected number of command line arguments')
   call get_command_argument(1, value=raw, _RC_(status, status))
   parameter_file = trim(adjustl(raw))
   call initialize_cap(Root_SetServices, parameter_file, _RC) 
   call run_cap(_RC)
   stop

!_contains
end program grid_comp_creation_tester
