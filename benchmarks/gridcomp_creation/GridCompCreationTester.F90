#define _SET_IF_PRESENT(A, B) if(present(A)) A = B
#define _RC _SET_IF_PRESENT(rc, status)
program grid_comp_creation_tester

   use shared_constants
   use gridcomp_creator, initialize_creator => initialize, run_creator => run

   implicit none

   integer :: status
   integer, parameter :: ARG_NPES = 1
   integer, parameter :: ARG_FILENAME = 2
   integer, parameter :: MAX_NARGS = 2
   integer :: npes, nargs
   character(len=:), allocatable :: parameter_filename
   character(len=MAXSTR) :: raw
   
   parameter_filename = ''
   nargs = command_argument_count()
   if(nargs > 0) then
      call get_command_argument(ARG_NPES, value=raw, status=status)
      write(npes, fmt='(I0)', iostat=status) trim(adjustl(raw))
   end if
   if(nargs > 1) then
      call get_command_argument(ARG_FILENAME, value=raw, status=status)
      parameter_filename = trim(adjustl(raw))
   end if
   
   call initialize_creator(parameter_filename, npes, rc = status) 
   call run_creator(rc=status)
   _RC

   stop

end program grid_comp_creation_tester
