#define I_AM_MAIN
#include "MAPL_Generic.h"
program grid_comp_creation_tester

   use shared_constants
   use strings
   use key_value_pairs
   use gridcomp_creator, initialize_creator => initialize, run_creator => run
   use MAPL

   implicit none

   character(len=:), parameter :: Iam="GridCompCreationTester"
   integer, parameter :: EXPECTED_NARGS = 1
   integer, parameter :: ARG_FILENAME = 1
   integer :: status
   character(len=:), allocatable :: parameter_file
   character(len=MAXSTR) :: raw
   type(String) :: arguments(EXPECTED_NARGS)
   type(KeyValuePair), allocatable :: parameters(:)
   
   call get_arguments(arguments, _RC)
   parameter_filename = arguments(ARG_FILENAME)
   call initialize_creator(parameter_filename, parameters, _RC) 
   call run_creator(_RC)
   stop

contains

   subroutine get_arguments(arguments, rc)
      type(String) :: arguments(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      character(len=MAXSTR) :: raw

      _ASSERT(command_argument_count() == size(arguments), 'Unexpected number of command line arguments')
      do i=1, size(arguments)
         call get_command_argument(i, value=raw, status=status)
         arguments(i) = trim(adjustl(raw))
      end do
      
   end subroutine get_arguments

end program grid_comp_creation_tester
