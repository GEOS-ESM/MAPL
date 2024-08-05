#define I_AM_MAIN
!#include "MAPL_Generic.h"
program grid_comp_creation_tester

   use grid_comp_creation_shared
   use grid_comp_creator
   use grid_comp_creator_memory_profiler
!   use mapl_ErrorHandlingMod

   implicit none

   integer, parameter :: SUCCESS = 0
   integer, parameter :: GENERAL_ERROR = 1
   integer, parameter :: NGC_NOT_SET = 2*GENERAL_ERROR
   integer, parameter :: RUN_FAILED  = 2*NGC_NOT_SET

   integer :: rc, status
   integer :: ngc
   character(len=MAXSTR) :: raw
   type(GridCompCreator) :: creator
   integer :: i, n
   logical :: is_silent = .TRUE.

   rc = SUCCESS
   ngc = -1
   do i = 1, command_argument_count()
      call get_command_argument(i, value=raw, status=status)
      if(status /= SUCCESS) cycle
      raw = adjustl(raw)
      read(raw, fmt='(I32)', iostat=status) n
      if(status == SUCCESS) ngc = n
   end do

   if(ngc < 0) then
      rc = rc + NGC_NOT_SET
      error stop rc, QUIET=is_silent
   end if
   
   call creation_driver(ngc, status)
   if(status == SUCCESS) then
      call write_results()
   else
      rc = rc + RUN_FAILED
   end if
   call finalize(rc=rc)
   if(rc == SUCCESS) stop rc, QUIET=is_silent
   error stop rc, QUIET=is_silent
!   creator = GridCompCreator(ngc)
!   call run(creator, rc)

end program grid_comp_creation_tester
