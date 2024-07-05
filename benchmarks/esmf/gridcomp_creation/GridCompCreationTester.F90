#define I_AM_MAIN
#define RC__(A, B) A=B
#define RC(A) RC__(A, A)
#define RC_ RC__(rc, status)
#define STATUS_ RC(status)
program grid_comp_creation_tester

   use, intrinsic :: iso_fortran_env, only: I64 => int64, R64 => real64
   use shared_constants
   use grid_comp_creator, initialize_creator => initialize, run_creator => run

   implicit none

   integer :: status
   integer, parameter :: ARG_NPES = 1
   integer, parameter :: ARG_NGC = ARG_NPES+1
   integer, parameter :: ARG_FILENAME = ARG_NPES+2
   integer, parameter :: MAX_NARGS = 3
   integer :: npes, nargs, ngc
   character(len=:), allocatable :: parameter_filename
   character(len=MAXSTR) :: raw
   real(kind=R64) :: time
   integer(kind=I64) :: memory
   character(len=*), parameter :: FMT_ = '(G0, ", ", G0, ", ", G0)'

   parameter_filename = ''
   npes = 1
   ngc = 10
   nargs = command_argument_count()
   if(nargs > 0) then
      call get_command_argument(ARG_NPES, value=raw, STATUS_)
      raw = adjustl(raw)
      read(raw, fmt='(I)', iostat=status) npes
   end if
   if(nargs > 1) then
      call get_command_argument(ARG_NGC, value=raw, STATUS_)
      raw = adjustl(raw)
      read(raw, fmt='(I)', iostat=status) ngc
   end if
   if(nargs > 2) then
      call get_command_argument(ARG_FILENAME, value=raw, STATUS_)
      parameter_filename = trim(adjustl(raw))
   end if
   
   call initialize_creator(npes, ngc, RC_) 
   call run_creator(time, memory, RC_)
   write(*, fmt=FMT_) ngc, time, memory

   stop

end program grid_comp_creation_tester
