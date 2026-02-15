#include "MAPL.h"

! From MAPL_MemUtils.F90

module mapl3g_MemInfo

   use mpi
   use MAPL_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use pFlogger, only: logging, logger_t => logger
   use, intrinsic :: iso_fortran_env, only: int64

   implicit none
   private

   public :: MemInfo
   public :: MemInfoWrite

   type ProcessMem
      real :: hwm ! high water mark
      real :: rss ! resident set size
   contains
      procedure :: get_process_mem
   end type ProcessMem

   type SystemMem
      real :: mem_used
      real :: swap_used
      real :: commit_limit
      real :: committed_as
   contains
      procedure :: get_system_mem
   end type SystemMem

   type MemInfo
      type(ProcessMem) :: process_mem
      type(SystemMem) :: system_mem
      class(logger_t), pointer :: logger => null()
   contains
      procedure :: get
      procedure :: write
   end type MemInfo

contains

   subroutine MemInfoWrite(comm, logger, text, rc)
      integer, intent(in) :: comm
      class(logger_t), pointer, optional, intent(in) :: logger
      character(len=*), optional, intent(in) :: text
      integer, optional, intent(out) :: rc

      type(MemInfo) :: mem_info
      integer :: status

      mem_info%logger => logging%get_logger('mapl.meminfo')
      if (present(logger)) then
         mem_info%logger => logger
      end if
      call mem_info%get(comm, _RC)
      call mem_info%write(mem_info%logger, text)

      _RETURN(_SUCCESS)
   end subroutine MemInfoWrite

   subroutine get(this, comm, rc)
      class(MemInfo), intent(inout) :: this
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status

      call this%process_mem%get_process_mem(comm, _RC)
      call this%system_mem%get_system_mem(comm, _RC)

      _RETURN(_SUCCESS)
   end subroutine get

   ! This routine returns the memory usage of calling process
   subroutine get_process_mem(this, comm, rc)
      class(ProcessMem), intent(inout) :: this
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc


      character(len=*), parameter :: process_mem_file = '/proc/self/status'
      character(len=32) :: line
      real :: hwm, rss
      integer :: unit, status

      open(newunit=unit, file=process_mem_file, form='formatted', iostat=status)
      _VERIFY(status)
      do; read (unit, '(a)', end=10) line
         if (index(line, 'VmHWM:') == 1) then  ! High Water Mark
            hwm = get_value(line, "VmHWM:")
         endif
         if (index(line, 'VmRSS:') == 1) then  ! Resident Memory
            rss = get_value(line, "VmRSS:")
         endif
      enddo
10    close(unit)

      ! Reduce
      call MPI_AllReduce(hwm, this%hwm, 1, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)

      call MPI_AllReduce(rss, this%rss, 1, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine get_process_mem

   ! This routine returns the memory usage on Linux system
   subroutine get_system_mem(this, comm, rc)
      class(SystemMem), intent(inout) :: this
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      ! This routine returns the memory usage on Linux systems.
      ! It does this by querying a system file (file_name below).

      character(len=*), parameter :: system_mem_file   = '/proc/meminfo'
      character(len=32) :: line
      integer(kind=int64) :: memtot, memfree, swaptot, swapfree, commit_limit, committed_as
      real :: local
      integer :: unit, status

      ! Read local memory information
      open(newunit=unit, file=system_mem_file, form='formatted', iostat=status)
      _VERIFY(STATUS)
      do; read (unit, '(a)', end=20) line
         if (index(line, 'MemTotal:') == 1) then  ! High Water Mark
            memtot = get_value(line, "MemTotal:")
         endif
         if (index(line, 'MemFree:') == 1) then  ! High Water Mark
            memfree = get_value(line, "MemFree:")
         endif
         if (index(line, 'SwapTotal:') == 1) then  ! Resident Memory
            swaptot = get_value(line, "SwapTotal:")
         endif
         if (index(line, 'SwapFree:') == 1) then  ! Resident Memory
            swapfree = get_value(line, "SwapFree:")
         endif
         if (index(line, 'CommitLimit:') == 1) then  ! Resident Memory
            commit_limit = get_value(line, "CommitLimit:")
         endif
         if (index(line, 'Committed_AS:') == 1) then  ! Resident Memory
            committed_as = get_value(line, "Committed_AS:")
         endif
      enddo
20    close(unit)

      ! Reduce
      local = memtot - memfree
      call MPI_AllReduce(local, this%mem_used, 1, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)

      local = swaptot - swapfree
      call MPI_AllReduce(local, this%swap_used, 1, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)

      call MPI_AllReduce(commit_limit, this%commit_limit, 1, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)

      call MPI_AllReduce(committed_as, this%committed_as, 1, MPI_REAL, MPI_MAX, comm, status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine get_system_mem

   subroutine write(this, logger, text)
      class(MemInfo), target, intent(in) :: this
      class(logger_t), pointer, intent(in) :: logger
      character(len=*), optional, intent(in) :: text

      character(len=:), allocatable :: text_
      type(ProcessMem), pointer :: process_mem => null()
      type(SystemMem), pointer :: system_mem => null()

      text_ = ":"
      if (present(text)) text_ = " at <" // text // ">:"

      process_mem => this%process_mem
      system_mem => this%system_mem

      call logger%info("Process HWM/RSS (MB)%a: %es11.3 %es11.3", text_, process_mem%hwm, process_mem%rss)
      call logger%info("Mem/Swap used (MB)%a: %es11.3 %es11.3", text_, system_mem%mem_used, system_mem%swap_used)
      call logger%info( &
           "CommitLimit/Committed_AS (MB)%a %es11.3 %es11.3", &
           text_, system_mem%commit_limit, system_mem%committed_as)
   end subroutine write

   function get_value(string, key, rc) result(value)
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: key
      integer, intent(out), optional :: rc
      real :: value ! result

      real :: multiplier
      integer :: key_len, string_len
      character(len=:), allocatable :: msg

      msg = "input string <"//trim(string)//"> does not contain key <"//trim(key)//">"
      _ASSERT(index(string, key) == 1, msg)
      key_len = len_trim(key)
      string_len = len_trim(string)
      read(string(key_len+1:string_len-2),*) value
      ! Convert kB -> MB
      multiplier = 1.0
      if (trim(string(string_len-1:)) == "kB") multiplier = 1.0/1024.
      value = value * multiplier

      _RETURN(_SUCCESS)
   end function get_value

end module mapl3g_MemInfo
