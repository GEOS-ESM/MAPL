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
   public :: operator(-)

   type :: MemoryProfile
      real(R64) :: total = -1.0
      real(R64) :: memory = -1.0
      real(R64) :: percent = -1.0
   end type MemoryProfile

   interface operator(-)
      module procedure :: subtract_memory_profile
   end interface operator(-)

contains
   
   function subtract_memory_profile(minuend, subtrahend) result(difference)
      type(MemoryProfile) :: difference
      class(MemoryProfile), intent(in) :: minuend, subtrahend

      difference = MemoryProfile(minuend%total - subtrahend%total, &
         minuend%memory - subtrahend%memory, &
         minuend%percent - subtrahend%percent)

   end function subtract_memory_profile

   subroutine profile_memory(memu, memc, rc)
      type(MemoryProfile), intent(out) :: memu
      type(MemoryProfile), intent(out) :: memc
      integer, optional, intent(out) :: rc
      integer :: status
      real :: total, memory, percent

      ! Get percent of used memory
      call MAPL_MemUsed(total, memory, percent)
      memu = MemoryProfile(total, memory, percent)
      ! Get percent of committed memory
      call MAPL_MemCommited(total, memory, percent)
      memu = MemoryProfile(total, memory, percent)

      _RETURN(_SUCCESS)

   end subroutine profile_memory

end module grid_comp_creator_memory_profiler
