#include "MAPL_ErrLog.h"

module mapl_BW_Benchmark
   use mapl_ErrorHandlingMod
   use netcdf
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

   public :: BW_Benchmark
   public :: NETCDF_FILE
   public :: BINARY_FILE

   enum, bind(c)
      enumerator :: NETCDF_FILE
      enumerator :: BINARY_FILE
   end enum

   type :: BW_Benchmark
      real, allocatable :: buffer(:)
      character(:), allocatable :: filename
      integer :: file_type
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
      integer :: var_id

      if (this%file_type == BINARY_FILE) then
         unit = open_bin_file(this%filename, _RC)
         call write_bin_file(this%buffer, unit, _RC)
         call delete_bin_file(this%filename, _RC)
      else if (this%file_type == NETCDF_FILE) then
         unit = open_netcdf_file(this%filename, this%buffer, var_id, _RC)
         call write_netcdf_file(this%buffer, unit, var_id, _RC)
         call delete_netcdf_file(this%filename, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine run

   function open_bin_file(filename, rc) result(unit)
      integer :: unit
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      unit = -1 ! unless
      open(file=filename, newunit=unit, &
           status='new', form='unformatted', access='sequential', _IOSTAT)

      _RETURN(_SUCCESS)
   end function open_bin_file

   subroutine write_bin_file(buffer, unit, rc)
      real, intent(in) :: buffer(:)
      integer :: unit
      integer, optional, intent(out) :: rc

      integer :: status

      write(unit, iostat=status) buffer(:)
      _VERIFY(status)

      ! Without the close, maybe the writing is not done?
      close(unit, _IOSTAT)

      _RETURN(_SUCCESS)
   end subroutine write_bin_file


   subroutine delete_bin_file(filename, rc)
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: unit

      open(file=filename, newunit=unit, &
           status='old', form='unformatted', access='sequential', _IOSTAT)
      close(unit, status='delete', _IOSTAT)

      _RETURN(_SUCCESS)
   end subroutine delete_bin_file

   function open_netcdf_file(filename, buffer, varid, rc) result(unit)
      integer :: unit
      character(*), intent(in) :: filename
      real, intent(in) :: buffer(:)
      integer, intent(inout) :: varid
      integer, optional, intent(out) :: rc

      integer :: status, dimid

      status = NF90_Create(filename, IOR(NF90_CLOBBER, NF90_NETCDF4), unit)
      _VERIFY(status)
      status = NF90_def_dim(unit, "dim1", size(buffer), dimid)
      _VERIFY(status)
      status = NF90_def_var(unit, 'var1', NF90_FLOAT, [dimid], varid)
      _VERIFY(status)
      status = NF90_EndDef(unit)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function open_netcdf_file

   subroutine write_netcdf_file(buffer, unit, varid, rc)
      real, intent(in) :: buffer(:)
      integer :: unit
      integer :: varid
      integer, optional, intent(out) :: rc

      integer :: status

      status = nf90_put_var(unit, varid, buffer)
      _VERIFY(status)

      ! Without the close, maybe the writing is not done?
      status = nf90_close(unit)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine write_netcdf_file


   subroutine delete_netcdf_file(filename, rc)
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc
      integer :: status

      call execute_command_line('rm '//filename, exitstat=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine delete_netcdf_file

end module mapl_BW_Benchmark

