#define I_AM_MAIN
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

program main
  use MAPL_ExceptionHandling
  use pFIO
  implicit none

  type (FileMetadata) :: file_metadata
  type (NetCDF4_FileFormatter) :: formatter
  type (FileMetadata) :: test_metadata
  integer :: status

  call formatter%open('test_in.nc4', pFIO_READ, rc=status)
  _ASSERT(status==0, "Failed to open 'test_in.nc4'")
  file_metadata = formatter%read(rc=status)
  _ASSERT(status==0, "Failed while reading 'test_in.nc4'")
  call formatter%close(rc=status)
  _ASSERT(status==0, "Wrong in closing test_in.nc4")
  print*, "Successfully opened, read and closed 'test_in.nc4'."
end program
