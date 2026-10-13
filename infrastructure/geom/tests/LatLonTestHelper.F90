#include "MAPL_ErrLog.h"

module mapl_LatLonTestHelper_mod
   use pfio
   use mapl_ErrorHandling_mod
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: create_latlon_file
   public :: read_latlon_metadata

contains

   ! Create a simple regular lat-lon grid file with 1-D 'lon'/'lat'
   ! coordinate variables (degrees_east / degrees_north), suitable for
   ! exercising make_LatLonGeomSpec_from_metadata(). An optional
   ! perturbation is added to every coordinate value, so that two files
   ! built with different (small) perturbations describe near-identical
   ! -but-not-bitwise-equal grids, for coordinate-tolerance testing.
   subroutine create_latlon_file(filename, im, jm, rc, perturbation)
      character(len=*), intent(in)  :: filename
      integer,          intent(in)  :: im, jm
      integer, optional, intent(out) :: rc
      real(kind=REAL64), optional, intent(in) :: perturbation

      integer :: status, i, j
      type(FileMetadata)          :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable)               :: var
      real(kind=REAL64), allocatable :: lons(:), lats(:)
      real(kind=REAL64) :: dlon, dlat, delta

      delta = 0.0_REAL64
      if (present(perturbation)) delta = perturbation

      dlon = 360.0_REAL64 / im
      dlat = 180.0_REAL64 / jm

      allocate(lons(im), lats(jm))
      do i = 1, im
         lons(i) = -180.0_REAL64 + (i - 0.5_REAL64) * dlon + delta
      end do
      do j = 1, jm
         lats(j) = -90.0_REAL64 + (j - 0.5_REAL64) * dlat + delta
      end do

      file_metadata = FileMetadata()
      call file_metadata%add_dimension('lon', im)
      call file_metadata%add_dimension('lat', jm)

      var = Variable(type=pFIO_REAL64, dimensions='lon')
      call var%add_attribute('units', Attribute('degrees_east'))
      call file_metadata%add_variable('lon', var)

      var = Variable(type=pFIO_REAL64, dimensions='lat')
      call var%add_attribute('units', Attribute('degrees_north'))
      call file_metadata%add_variable('lat', var)

      call formatter%create(file=filename, mode=pFIO_CLOBBER, rc=status)
      _VERIFY(status)
      call formatter%write(file_metadata, rc=status)
      _VERIFY(status)
      call formatter%put_var('lon', lons, rc=status)
      _VERIFY(status)
      call formatter%put_var('lat', lats, rc=status)
      _VERIFY(status)
      call formatter%close(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine create_latlon_file

   ! Re-opens a file written by create_latlon_file() and reads back its
   ! FileMetadata (with real coordinate data attached as CoordinateVariables,
   ! as would happen for any file ExtData/GeomManager reads from disk).
   function read_latlon_metadata(filename, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(NetCDF4_FileFormatter) :: formatter

      call formatter%open(filename, pFIO_READ, rc=status)
      _VERIFY(status)
      file_metadata = formatter%read(rc=status)
      _VERIFY(status)
      call formatter%close(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function read_latlon_metadata

end module mapl_LatLonTestHelper_mod
