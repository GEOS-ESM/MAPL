#define I_AM_MAIN
#include "MAPL_Generic.h"
program grid_comp_creation_tester

   use, intrinsic :: iso_fortran_env, only: R64 => real64
   use shared_constants
   use grid_comp_creator
   use grid_comp_creator_memory_profiler
   use mapl_ErrorHandlingMod

   implicit none

   integer :: status
   integer, parameter :: ARG_NGC = 1
   integer, parameter :: ARG_FILENAME = ARG_NGC+1
   integer, parameter :: MAX_NARGS = 2
   integer :: nargs, ngc
   character(len=MAXSTR) :: parameter_filename
   character(len=MAXSTR) :: raw
   type(GridCompCreator) :: creator
   real(kind=R64) :: time
   type(MemoryProfile) :: mem_used
   type(MemoryProfile) :: mem_commit

   parameter_filename = ''
   ngc = 10
   nargs = command_argument_count()
   if(nargs > 0) then
      call get_command_argument(ARG_NGC, value=raw, _RC_(status, status))
      raw = adjustl(raw)
      read(raw, fmt='(I32)', iostat=status) ngc
   end if
   if(nargs > 1) then
      call get_command_argument(ARG_FILENAME, value=raw, _RC_(status, status))
      parameter_filename = adjustl(raw)
   end if
   
   creator = GridCompCreator(ngc)
   call creator%run(time, mem_used, mem_commit, _RC)
   if(creator%rank == 0) then
      call write_results(ngc, time, mem_used, mem_commit, rc=status)
      if(status /= 0) write(*, '(A)') 'Failed to write results.'
   end if
   call creator%finalize(rc=status)
   stop

contains

   subroutine write_results(ngc, time, mem_used, mem_commit, rc)
      integer, intent(in) :: ngc
      real(R64), intent(in) :: time
      class(MemoryProfile), intent(in) :: mem_used
      class(MemoryProfile), intent(in) :: mem_commit
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: performance
      character(len=MAXSTR) :: columns
!      real(R64) :: real_columns(6) 
!      character(len=*), parameter :: FMT_ = "(I0, 6(',', 1X, ES22.16))"
      real(R64) :: real_columns(3)
      character(len=*), parameter :: FMT_ = "(I0, 3(',', 1X, ES22.16))"
      
!      real_columns(1) = time
!      real_columns(2:4) = [mem_used%total, mem_used%memory, mem_used%percent]
!      real_columns(5:6) = [mem_commit%memory, mem_commit%percent]
      real_columns = [time, mem_used%memory, mem_used%percent]
!      columns = '# num_components, time(s), ' // &
!         'total_mem (MB), mem_used(MB), percent_used, ' // &
!         'mem_commit (MB), percent_commit'
      columns = '# num_components, time(s), mem_used(MB), percent_used(MB)'
      write(performance, fmt=FMT_, iostat=status) ngc, real_columns
      if(present(rc)) rc = status
      if(status /= 0) return

      write(*, fmt='(A)') trim(columns)
      write(*, fmt='(A)') trim(performance)

   end subroutine write_results

end program grid_comp_creation_tester
