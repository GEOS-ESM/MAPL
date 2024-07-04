#define I_AM_MAIN

#if defined(I_AM_MAIN)
#  define _RETURN_(A) if(A == SUCCESS) then; stop A; else; error stop A; end if
#else
#  define _RETURN_(A) if(present(rc)) rc=A; return
#endif

#define _VERIFY(A) if(A /= SUCCESS)
#define _RC_(A, B) A=B
#define RC(A) _RC_(A, A)
#define _RC _RC_(rc, status)
#define _STATUS_ RC(status)
#define _IOSTAT_(A, S) __RC__(iostat, A)) S; if(A /= SUCCESS) then; _RETURN_(A)
#define _HERE print*,__FILE__,__LINE__
program grid_comp_creation_tester

   use, intrinsic :: iso_fortran_env, only: I64 => int64
   use shared_constants
   use grid_comp_creator, initialize_creator => initialize, run_creator => run

   implicit none

   integer :: status
   integer, parameter :: ARG_NPES = 1
   integer, parameter :: ARG_FILENAME = 2
   integer, parameter :: MAX_NARGS = 2
   integer :: npes, nargs, num_gc
   character(len=:), allocatable :: parameter_filename
   character(len=MAXSTR) :: raw
   integer(kind=I64) :: time
   integer(kind=I64) :: memory
   character(len=*), parameter :: FMT_ = '(I0, 1X, "ms, ", I0, 1X, "B")'

   parameter_filename = ''
   npes = 1
   num_gc = 10
   nargs = command_argument_count()
   if(nargs > 0) then
      call get_command_argument(ARG_NPES, value=raw, _STATUS_)
      write(npes, fmt='(I0)', _IOSTAT_(status)) trim(adjustl(raw))
   end if
   if(nargs > 1) then
      call get_command_argument(ARG_FILENAME, value=raw, _STATUS_)
      parameter_filename = trim(adjustl(raw))
   end if
   
   call initialize_creator(npes, num_gc, rc = status) 
   call run_creator(time, memory, rc=status)
   write(*, fmt=FMT_) time, memory

   stop

end program grid_comp_creation_tester
