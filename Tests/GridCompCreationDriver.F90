#define I_AM_MAIN
#include "MAPL_Generic.h"
program grid_comp_creation_driver

   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   use MPI
   use ESMF
   use gridcomp_creation_cap
   use MAPL
   implicit none

   integer :: status
   character(len=:), parameter :: Iam="GridCompCreationDriver"
   character(len=:), parameter :: CAPNAME = 'GridCompCreationCap'
   character(len=:), parameter :: TESTNAME = 'GridCompCreationTest'
   integer :: npes_model = 20
   type(TestParameters) :: parameters

   parameters = TestParameters(TESTNAME, npes_model)
   call initialize_cap(Root_SetServices, parameters, _RC) 
   call run_cap(_RC)
   stop

!_contains
end program grid_comp_creation_driver
