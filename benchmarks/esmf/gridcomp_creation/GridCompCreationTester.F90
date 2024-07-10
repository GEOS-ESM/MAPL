#define I_AM_MAIN
#include "MAPL_Generic.h"
#define SET_RC_(S) if(present(rc)) rc=S
#define _SET_RCZ SET_RC_(0)
#define _SET_RCS SET_RC_(status)
#define _SET_RCS_ _SET_RCS; _CHECK_STATUS
#define _CHECK_RC rc=status); CHECK_STATUS(status
#define CHECK_STATUS(S) if(S /= 0) return
#define _CHECK_STATUS CHECK_STATUS(status)

program grid_comp_creation_tester

   use, intrinsic :: iso_fortran_env, only: I64 => int64, R64 => real64
   use shared_constants
   use grid_comp_creator
   use grid_comp_creator_memory_profiler
   use mapl_ErrorHandlingMod

   implicit none

   interface add_column
      procedure :: add_column_real
      procedure :: add_column_integer
      procedure :: add_column_character
   end interface add_column

   integer :: status
   integer, parameter :: ARG_NPES = 1
   integer, parameter :: ARG_NGC = ARG_NPES+1
   integer, parameter :: ARG_FILENAME = ARG_NPES+2
   integer, parameter :: MAX_NARGS = 3
   integer :: npes, nargs, ngc
   character(len=2), parameter :: JOIN = ', '
   character(len=MAXSTR) :: parameter_filename
   character(len=MAXSTR) :: raw
   real(kind=R64) :: time
   type(GridCompCreator) :: creator
   type(MemoryProfile) :: mem_used
   type(MemoryProfile) :: mem_commit
   integer :: rank

   parameter_filename = ''
   npes = 1
   ngc = 10
   nargs = command_argument_count()
   if(nargs > 0) then
      call get_command_argument(ARG_NPES, value=raw, _RC_(status, status))
      raw = adjustl(raw)
      read(raw, fmt='(I32)', iostat=status) npes
   end if
   if(nargs > 1) then
      call get_command_argument(ARG_NGC, value=raw, _RC_(status, status))
      raw = adjustl(raw)
      read(raw, fmt='(I32)', iostat=status) ngc
   end if
   if(nargs > 2) then
      call get_command_argument(ARG_FILENAME, value=raw, _RC_(status, status))
      parameter_filename = adjustl(raw)
   end if
   
   creator = GridCompCreator(npes, ngc)
   call run_creator(creator, time, mem_used, mem_commit, _RC)
   call MPI_Comm_Rank(creator%comm, rank, status)
   if(rank == 0) then
      call write_results(ngc, time, mem_used, mem_commit, rc=status)
      if(status /= 0) write(*, '(A)') 'Failed to write results.'
   end if
   call finalize_all(creator)
   stop

contains

   subroutine write_results(ngc, time, mem_used, mem_commit, rc)
      integer, intent(in) :: ngc
      real(R64), intent(in) :: time
      class(MemoryProfile), intent(in) :: mem_used
      class(MemoryProfile), intent(in) :: mem_commit
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: performance
      character(len=:), allocatable :: columns
      
      columns = '# num_components'
      performance = as_character_integer(ngc, _CHECK_RC)
      call add_column(columns, 'time (s)')
      call add_column(performance, time, _CHECK_RC)
      call add_column(columns, 'total_mem (MB)')
      call add_column(performance, mem_used%total, _CHECK_RC)
      call add_column(columns, 'mem_used (MB)')
      call add_column(performance, mem_used%memory, _CHECK_RC)
      call add_column(columns, 'mem_commit (MB)')
      call add_column(performance, mem_commit%memory, _CHECK_RC)
      call add_column(columns, 'percent_used')
      call add_column(performance, mem_used%percent, _CHECK_RC)
      call add_column(columns, 'percent_commit')
      call add_column(performance, mem_commit%percent, _CHECK_RC)

      write(*, fmt='(A)') columns
      write(*, fmt='(A)') performance

      _SET_RCZ

   end subroutine write_results

   subroutine add_column_real(row, t, rc)
      character(len=:), allocatable, intent(inout) :: row
      real(R64), intent(in) :: t
      integer, optional, intent(out) :: rc
      integer :: status

      row = row // JOIN // as_character_real(t, rc=status)
      _SET_RCS

   end subroutine add_column_real

   subroutine add_column_integer(row, n, rc)
      character(len=:), allocatable, intent(inout) :: row
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status

      row = row // JOIN // as_character_integer(n, rc=status)
      _SET_RCS

   end subroutine add_column_integer

   subroutine add_column_character(row, ch, rc)
      character(len=:), allocatable, intent(inout) :: row
      character(len=*), intent(in) :: ch
      integer, optional, intent(out) :: rc
      
      row = row // JOIN // ch
      _SET_RCZ

   end subroutine add_column_character

   function as_character_real(t, rc) result(ch)
      character(len=:), allocatable :: ch
      real(R64), intent(in) :: t
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: raw
      character(len=*), parameter :: FMT_ = '(G0)'

      write(raw, fmt=FMT_, iostat=status) t
      _SET_RCS_
      ch = trim(adjustl(raw))

   end function as_character_real

   function as_character_integer(n, rc) result(ch)
      character(len=:), allocatable :: ch
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=MAXSTR) :: raw
      character(len=*), parameter :: FMT_ = '(I0)'

      write(raw, fmt=FMT_, iostat=status) n
      _SET_RCS_
      ch = trim(adjustl(raw))

   end function as_character_integer

end program grid_comp_creation_tester
