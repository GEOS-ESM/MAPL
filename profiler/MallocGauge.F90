#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_MallocGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use, intrinsic :: iso_c_binding, only : C_INT, C_LONG_LONG
   use MAPL_AbstractGauge
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

   interface
      subroutine getMallocStat(tm, cnt) bind(C, name="getMallocStat")
        use iso_c_binding, only: C_LONG_LONG
        import C_LONG_LONG
        implicit none
        integer(kind=C_LONG_LONG) :: tm
        integer(kind=C_LONG_LONG) :: cnt
      end subroutine getMallocStat
   end interface
contains


   function new_MallocGauge() result(gauge)
      type (MallocGauge) :: gauge

      gauge%baseline = 0

   end function new_MallocGauge


   function get_measurement(this) result(mem_use)
      class (MallocGauge), intent(inout) :: this
      real(kind=REAL64) :: mem_use

       integer(kind=C_LONG_LONG) :: tm, cnt

      call getMallocStat(tm, cnt)
      mem_use = tm

   end function get_measurement

end module MAPL_MallocGauge


subroutine fortran_malloc(addr, num_bytes)
   use, intrinsic :: iso_fortran_env, only: INT64
   use mapl_GlobalMallocTable
   integer(kind=INT64), intent(in) :: addr
   integer(kind=INT64), intent(in) :: num_bytes
   integer, pointer :: a(:)
   

   allocate(a(10))
   return
   call active_allocations%insert(addr, num_bytes)
   active_allocations_total = active_allocations_total + num_bytes

!   write (*,'(i5,a,2i10, z16)') active_allocations%size(), 'fortran malloc: ', num_bytes, active_allocations_total, addr
   
end subroutine fortran_malloc

! Find the corresponding allocation in the table to find the number of bytes
! being deallocated. Free the table entry and decrement the total.
subroutine fortran_free(addr)
   use, intrinsic :: iso_fortran_env, only: INT64
   use mapl_GlobalMallocTable
   integer(kind=INT64), intent(in) :: addr

   integer(kind=INT64) :: num_bytes
   type(Integer64Integer64MapIterator) :: iter

   return
!   write (*,'(a,z16)')'in fortran free: ', addr

   iter = active_allocations%find(addr)
   num_bytes = iter%second()
   iter = active_allocations%erase(iter)

   active_allocations_total = active_allocations_total - num_bytes
!   print*,active_allocations%size(), 'fortran free: ', num_bytes, active_allocations_total, addr

end subroutine fortran_free

