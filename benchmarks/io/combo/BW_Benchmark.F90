#include "MAPL_ErrLog.h"

module mapl_BW_Benchmark
   use mapl_ErrorHandlingMod
   use Kernel_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

   public :: BW_Benchmark

   type, extends(Kernel_T) :: BW_Benchmark
      real, allocatable :: buffer(:)
      character(:), allocatable :: filename
   contains
      procedure :: run
   end type BW_Benchmark

contains

   !-----------
   ! Note: This benchmark assumes that the time to open the file is
   ! _relevant_ and that the time to _delete_ the file is
   ! inconsequential.
   !-----------
   subroutine run(this, rc)
      class(BW_Benchmark), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: unit

      unit = open_file(this%filename, __RC)
      call write_file(this%buffer, unit, __RC)
      call delete_file(this%filename, __RC)

      __RETURN(__SUCCESS)
   end subroutine run

   function open_file(filename, rc) result(unit)
      integer :: unit
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      unit = -1 ! unless
      open(file=filename, newunit=unit, &
           status='new', form='unformatted', access='sequential', __IOSTAT)

      __RETURN(__SUCCESS)
   end function open_file

   subroutine write_file(buffer, unit, rc)
      real, intent(in) :: buffer(:)
      integer :: unit
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      write(unit, iostat=status) buffer
      __VERIFY(status)

      ! Without the close, maybe the writing is not done?
      close(unit, __IOSTAT)

      __RETURN(__SUCCESS)
   end subroutine write_file
      

   subroutine delete_file(filename, rc)
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: unit

      open(file=filename, newunit=unit, &
           status='old', form='unformatted', access='sequential', __IOSTAT)
      close(unit, status='delete', __IOSTAT)

      __RETURN(__SUCCESS)
   end subroutine delete_file

end module mapl_BW_Benchmark

