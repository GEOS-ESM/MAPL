#include "MAPL_Generic.h"
module grid_comp_creator_memory_profiler
   
   use grid_comp_creation_shared
   use mapl_ErrorHandlingMod
   use MAPL_MemUtilsMod
   use, intrinsic :: iso_fortran_env, only: R64 => real64
   implicit none
   private

   public :: MemoryProfile
   public :: profile_memory
   public :: print_memory_profile
   public :: MEMORY_PROFILE_HEADER
   public :: operator(-)
   public :: operator(==)

   type :: MemoryProfile
      real(R64) :: total = -1.0
      real(R64) :: used = -1.0
      real(R64) :: committed = -1.0
      real(R64) :: percent_used = -1.0
      real(R64) :: percent_committed = -1.0
   end type MemoryProfile

   interface operator(-)
      module procedure :: subtract_memory_profile
   end interface operator(-)

   interface operator(==)
      module procedure :: equals_memory_profile
   end interface operator(==)

   enum, bind(c)
      enumerator :: INDEX_ = 0
      enumerator :: INDEX_TOTAL
      enumerator :: INDEX_USED
      enumerator :: INDEX_COMMITTED
      enumerator :: INDEX_PERCENT_USED
      enumerator :: INDEX_PERCENT_COMMITTED
   end enum

   integer(kind=kind(INDEX_)), parameter :: INDEX_SIZE = INDEX_PERCENT_COMMITTED
   character(len=*), parameter :: MEMORY_PROFILE_HEADER = "total (MB), used (MB), committed (MB), percent_used, percent_committed"

contains
   
   function as_array(mem) result(arr)
      real(R64) :: arr(INDEX_SIZE)
      class(MemoryProfile), intent(in) :: mem

      arr(INDEX_TOTAL) = mem%total
      arr(INDEX_USED) = mem%used
      arr(INDEX_COMMITTED) = mem%committed
      arr(INDEX_PERCENT_USED) = mem%percent_used
      arr(INDEX_PERCENT_COMMITTED) = mem%percent_committed

   end function as_array

   function from_array(arr) result(mem)
      type(MemoryProfile) :: mem
      real(R64), intent(in) :: arr(INDEX_SIZE)
      
      mem%total = arr(INDEX_TOTAL)
      mem%used = arr(INDEX_USED)
      mem%committed = arr(INDEX_COMMITTED)
      mem%percent_used = arr(INDEX_PERCENT_USED)
      mem%percent_committed = arr(INDEX_PERCENT_COMMITTED)

   end function from_array

   function subtract_memory_profile(p1, p2) result(diff)
      type(MemoryProfile) :: diff
      class(MemoryProfile), intent(in) :: p1, p2
      real(R64) :: total = -1.0

      if(p2%total == p1%total) total = p2%total
      diff = from_array(as_array(p1) - as_array(p2))
      diff%total = total

   end function subtract_memory_profile

   logical function equals_memory_profile(p1, p2) result(eq)
      class(MemoryProfile), intent(in) :: p1, p2

      eq = all(as_array(p1)==as_array(p2))

   end function equals_memory_profile

   subroutine profile_memory(mem, rc)
      type(MemoryProfile), allocatable, intent(out) :: mem
      integer, optional, intent(out) :: rc
      integer :: status
      real :: total, used, committed, percent_used, percent_committed

      ! Get used memory
      call MAPL_MemUsed(memtotal=total, used=used, percent_used=percent_used, _RC)
      ! Get committed memory
      call MAPL_MemCommited(memtotal=total, committed_as=committed, percent_committed=percent_committed, _RC)
      mem = MemoryProfile()
      mem%total = total
      mem%used = used
      mem%committed = committed
      mem%percent_used = percent_used
      mem%percent_committed = percent_committed

      _RETURN(_SUCCESS)

   end subroutine profile_memory

   function print_memory_profile(mem) result(values)
      character(len=:), allocatable :: values
      class(MemoryProfile), intent(in) :: mem
      integer :: status

      values = to_characters(mem%total, rc=status)
      values = values // JOIN // to_characters(mem%used, rc=status)
      values = values // JOIN // to_characters(mem%committed, rc=status)
      values = values // JOIN // to_characters(mem%percent_used, rc=status)
      values = values // JOIN // to_characters(mem%percent_committed, rc=status)

   end function print_memory_profile

end module grid_comp_creator_memory_profiler
