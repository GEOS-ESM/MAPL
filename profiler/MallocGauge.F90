#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module mapl_GlobalMallocTable
   use gFTL2_Integer64Integer64Map
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none

   type(Integer64Integer64Map), save :: active_allocations
   integer(kind=INT64), save :: active_allocations_total = 0

end module mapl_GlobalMallocTable

module MAPL_MallocGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use, intrinsic :: iso_c_binding, only : C_INT
   use MAPL_AbstractGauge
   use mapl_GlobalMallocTable
   implicit none
   private

   public :: MallocGauge

   type, extends(AbstractGauge) :: MallocGauge
      private
      integer(kind=INT64) :: baseline = 0
   contains
      procedure :: get_measurement
   end type MallocGauge

   interface MallocGauge
      module procedure :: new_MallocGauge
   end interface MallocGauge

contains


   function new_MallocGauge() result(gauge)
      type (MallocGauge) :: gauge

      gauge%baseline = 0

   end function new_MallocGauge


   function get_measurement(this) result(mem_use)
      class (MallocGauge), intent(inout) :: this
      real(kind=REAL64) :: mem_use

      type(Mallinfo_t) :: info

      mem_use = active_allocations_total

   end function get_measurement

end module MAPL_MallocGauge


subroutine fortran_malloc(addr, num_bytes)
   use, intrinsic :: iso_fortran_env, only: INT64
   use mapl_GlobalMallocTable
   integer(kind=INT64), intent(in) :: addr
   integer(kind=INT64), intent(in) :: num_bytes

   call active_allocations%insert(addr, num_bytes)
   active_allocations_total = active_allocations_total + num_bytes

   print*, active_allocations%size(), 'fortran malloc: ', num_bytes, active_allocations_total, addr
end subroutine fortran_malloc

! Find the corresponding allocation in the table to find the number of bytes
! being deallocated. Free the table entry and decrement the total.
subroutine fortran_free(addr)
   use, intrinsic :: iso_fortran_env, only: INT64
   use mapl_GlobalMallocTable
   integer(kind=INT64), intent(in) :: addr

   integer(kind=INT64) :: num_bytes
   type(IntegerInteger64MapIterator) :: iter
 
   iter = active_allocations%find(addr)
   num_bytes = active_allocations%of(addr)
   iter = active_allocations%erase(iter)

   active_allocations_total = active_allocations_total - num_bytes
   print*,active_allocations%size(), 'fortran dealloc: ', num_bytes, active_allocations_total, addr

end subroutine fortran_free

