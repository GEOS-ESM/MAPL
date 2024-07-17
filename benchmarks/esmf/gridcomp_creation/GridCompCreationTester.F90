#define I_AM_MAIN
#include "MAPL_Generic.h"
program grid_comp_creation_tester

   use, intrinsic :: iso_fortran_env, only: R64 => real64
   use grid_comp_creation_shared
   use grid_comp_creator
   use grid_comp_creator_memory_profiler
   use mapl_ErrorHandlingMod

   implicit none

   integer, parameter :: SUCCESS = 0
   integer, parameter :: NGC_NOT_SET = 1
   integer, parameter :: RUN_FAILED  = 2
   integer, parameter :: GENERAL_ERROR = -1

   integer :: status, rc
   integer :: ngc
   character(len=MAXSTR) :: raw
   type(GridCompCreator) :: creator
   integer :: i, n
   logical :: is_silent = .TRUE.

   rc = SUCCESS
   ngc = -1
   do i = 1, command_argument_count()
      call get_command_argument(i, value=raw, status=status)
      if(status /= 0) cycle
      raw = adjustl(raw)
      read(raw, fmt='(I32)', iostat=status) n
      if(status == 0) ngc = n
   end do   

   if(ngc < 0) rc = rc + NGC_NOT_SET
   if(rc /= SUCCESS) error stop rc, QUIET=is_silent

   creator = GridCompCreator(ngc)
   call run(creator, status)
   if(status /= 0) rc = rc + RUN_FAILED
   if(rc /= SUCCESS) error stop rc, QUIET=is_silent
   call write_results(creator)
   call finalize(rc=status)
   stop rc, QUIET=is_silent

end program grid_comp_creation_tester
