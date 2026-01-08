#include "MAPL.h"

! From MAPL_MemUtils.F90

module mapl3g_MemInfo

   use mpi
   use MAPL_ErrorHandling, only: MAPL_Verify, MAPL_Return, MAPL_Assert
   use pFlogger, only: logging, logger_t => logger
   use, intrinsic :: iso_fortran_env, only: int64

   implicit none
   private

   public :: MemInfoWrite

   type ProcessMem
      real :: hwm ! high water mark
      real :: rss ! resident set size
   contains
      procedure :: get_process_mem
      procedure :: write_process_mem
   end type ProcessMem

   type SystemMem
      real :: mem_used
      real :: swap_used
      real :: commit_limit
      real :: committed_as
   contains
      procedure :: get_system_mem
      procedure :: write_system_mem
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

   subroutine MemInfoWrite(comm, logger, rc)
      integer, intent(in) :: comm
      class(logger_t), pointer, optional, intent(in) :: logger
      integer, optional, intent(out) :: rc

      type(MemInfo) :: mem_info
      integer :: status

      mem_info%logger => logging%get_logger('mapl.meminfo')
      if (present(logger)) then
         mem_info%logger => logger
      end if
      call mem_info%get(comm, _RC)
      call mem_info%write(mem_info%logger)

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

   subroutine write(this, logger)
      class(MemInfo), intent(in) :: this
      class(logger_t), pointer, intent(in) :: logger

      call this%process_mem%write_process_mem(logger)
      call this%system_mem%write_system_mem(logger)
   end subroutine write

   subroutine write_process_mem(this, logger)
      class(ProcessMem), intent(in) :: this
      class(logger_t), pointer, intent(in) :: logger

      call logger%info("HWM/RSS (MB): %es11.3 %es11.3", this%hwm, this%rss)
   end subroutine write_process_mem

   subroutine write_system_mem(this, logger)
      class(SystemMem), intent(in) :: this
      class(logger_t), pointer, intent(in) :: logger

      call logger%info("Mem/Swap used (MB): %es11.3 %es11.3", this%mem_used, this%swap_used)
      call logger%info("CommitLimit/Committed_AS (MB): %es11.3 %es11.3", this%commit_limit, this%committed_as)
   end subroutine write_system_mem

   function get_value(string, key, rc) result(value)
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: key
      integer, intent(out), optional :: rc
      real :: value ! result

      real :: multiplier
      integer :: key_len, string_len

      _ASSERT(index(string, key) == 1, &
           "input string <"//trim(string)//"> does not contain key <"//trim(key)//">")
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
