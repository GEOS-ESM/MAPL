#include "unused_dummy.H"

module MAPL_MallocGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use, intrinsic :: iso_c_binding, only : C_INT
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

   type, bind(C) :: mallinfo_t
      integer(C_INT) :: arena     ! Non-mmapped space allocated (bytes)
      integer(C_INT) :: ordblks   ! Number of free chunks
      integer(C_INT) :: smblks    ! Number of free fastbin blocks
      integer(C_INT) :: hblks     ! Number of mmapped regions
      integer(C_INT) :: hblkhd    ! Space allocated in mmapped regions (bytes)
      integer(C_INT) :: usmblks   ! See below
      integer(C_INT) :: fsmblks   ! Space in freed fastbin blocks (bytes)
      integer(C_INT) :: uordblks  ! Total allocated space (bytes)
      integer(C_INT) :: fordblks  ! Total free space (bytes)
      integer(C_INT) :: keepcost  ! Top-most, releasable space (bytes)
   end type mallinfo_t

#if  (!defined(sysDarwin) && (defined(__INTEL_COMPILER) || defined(__GFORTRAN)))
   interface
      function mallinfo() result(info) bind(C,name="mallinfo")
         import mallinfo_t
         type(mallinfo_t) :: info
      end function mallinfo
   end interface
#endif

contains


   function new_MallocGauge() result(gauge)
      type (MallocGauge) :: gauge

      gauge%baseline = 0

   end function new_MallocGauge


   function get_measurement(this) result(mem_use)
      class (MallocGauge), intent(inout) :: this
      real(kind=REAL64) :: mem_use

      type(Mallinfo_t) :: info

      info = mallinfo()
      mem_use = info%uordblks

   end function get_measurement

#if !(!defined(sysDarwin) && (defined(__INTEL_COMPILER) || defined(__GFORTRAN)))
   function mallinfo() result(info)
      type(mallinfo_t) :: info
      info %uordblks = 0
   end function mallinfo
#endif
end module MAPL_MallocGauge

