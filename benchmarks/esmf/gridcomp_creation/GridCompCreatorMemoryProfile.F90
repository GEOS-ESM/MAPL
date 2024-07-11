#include "MAPL_Generic.h"
module grid_comp_creator_memory_profiler
   
   use shared_constants
   use mapl_ErrorHandlingMod
   use MAPL_MemUtilsMod
   use, intrinsic :: iso_fortran_env, only: R64 => real64
   implicit none
   private

   public :: MemoryProfile
   public :: profile_memory
   public :: print_memory_profile
   public :: operator(-)
   public :: operator(==)

   type :: MemoryProfile
      real(R64) :: total = -1.0
      real(R64) :: memory = -1.0
      real(R64) :: percent = -1.0
   end type MemoryProfile

   interface MemoryProfile
      module procedure :: construct_memory_profile
   end interface MemoryProfile

   interface operator(-)
      module procedure :: subtract_memory_profile
   end interface operator(-)

   interface operator(==)
      module procedure :: equals_memory_profile
   end interface operator(==)

contains
   
   function construct_memory_profile(total, memory, percent) result(mem)
      type(MemoryProfile) :: mem
      real(R64), intent(in) :: total, memory, percent

      mem%total = total
      mem%memory = memory
      mem%percent = percent

   end function construct_memory_profile

   function subtract_memory_profile(minuend, subtrahend) result(difference)
      type(MemoryProfile) :: difference
      class(MemoryProfile), intent(in) :: minuend, subtrahend

      difference = MemoryProfile(minuend%total - subtrahend%total, &
         minuend%memory - subtrahend%memory, &
         minuend%percent - subtrahend%percent)

   end function subtract_memory_profile

   logical function equals_memory_profile(p1, p2) result(are_equal)
      class(MemoryProfile), intent(in) :: p1, p2

      are_equal = .TRUE.
      are_equal = are_equal .and. p1%total == p2%total
      are_equal = are_equal .and. p1%memory == p2%memory
      are_equal = are_equal .and. p1%percent == p2%percent

   end function equals_memory_profile

   subroutine profile_memory(memu, memc, rc)
      type(MemoryProfile), intent(out) :: memu
      type(MemoryProfile), intent(out) :: memc
      integer, optional, intent(out) :: rc
      integer :: status
      real :: total, memory, percent
      character(len=*), parameter :: FMT_ = '(A,3(1X, ES23.16))'

      ! Get used memory
      call MAPL_MemUsed(memtotal=total, used=memory, percent_used=percent, _RC)
      write(*, fmt=FMT_) 'used: ',total, memory, percent
      memu = MemoryProfile(total=total, memory=memory, percent=percent)
      ! Get committed memory
      call MAPL_MemCommited(memtotal=total, committed_as=memory, percent_committed=percent, _RC)
      write(*, fmt=FMT_) 'committed: ',total, memory, percent
      memc = MemoryProfile(total=total, memory=memory, percent=percent)

      _RETURN(_SUCCESS)

   end subroutine profile_memory

   subroutine print_memory_profile(mem)
      class(MemoryProfile), intent(in) :: mem
      character(len=*), parameter :: FMT_ = '(3(A,ES23.7))'

      write(*, fmt=FMT_) 'Total: ', mem%total, ', Memory: ', mem%memory, ', Percent: ', mem%percent

   end subroutine print_memory_profile

end module grid_comp_creator_memory_profiler
