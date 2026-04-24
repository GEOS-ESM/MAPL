#include "MAPL_ErrLog.h"

module mapl3g_XYTestHelper
   use pfio
   use mapl_ErrorHandlingMod
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: create_xy_file
   public :: create_xy_file_with_corners
   public :: create_xy_file_with_undef

contains

   ! Create a simple regular XY grid file with 'lons' and 'lats' variables.
   ! Coordinates are in degrees; the factory converts to radians on read.
   ! Grid spans -180..180 in lon, -90..90 in lat.
   subroutine create_xy_file(filename, im, jm, rc)
      character(len=*), intent(in)  :: filename
      integer,          intent(in)  :: im, jm
      integer, optional, intent(out) :: rc

      integer :: status, i, j
      type(FileMetadata)       :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable)           :: var
      real(REAL64), allocatable :: lons(:,:), lats(:,:)
      real(REAL64) :: dlon, dlat

      dlon = 360.0_REAL64 / im
      dlat = 180.0_REAL64 / jm

      allocate(lons(im,jm), lats(im,jm))
      do j = 1, jm
         do i = 1, im
            lons(i,j) = -180.0_REAL64 + (i - 0.5_REAL64) * dlon
            lats(i,j) = -90.0_REAL64  + (j - 0.5_REAL64) * dlat
         end do
      end do

      file_metadata = FileMetadata()
      call file_metadata%add_dimension('Xdim', im)
      call file_metadata%add_dimension('Ydim', jm)
      call file_metadata%add_attribute('grid_type', Attribute('XY'))

      var = Variable(type=pFIO_REAL64, dimensions='Xdim,Ydim')
      call var%add_attribute('units', Attribute('degrees_east'))
      call file_metadata%add_variable('lons', var)

      var = Variable(type=pFIO_REAL64, dimensions='Xdim,Ydim')
      call var%add_attribute('units', Attribute('degrees_north'))
      call file_metadata%add_variable('lats', var)

      call formatter%create(file=filename, mode=pFIO_CLOBBER, rc=status)
      _VERIFY(status)
      call formatter%write(file_metadata, rc=status)
      _VERIFY(status)
      call formatter%put_var('lons', lons, rc=status)
      _VERIFY(status)
      call formatter%put_var('lats', lats, rc=status)
      _VERIFY(status)
      call formatter%close(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine create_xy_file

   ! As above, but also writes corner_lons and corner_lats (im+1 x jm+1).
   subroutine create_xy_file_with_corners(filename, im, jm, rc)
      character(len=*), intent(in)  :: filename
      integer,          intent(in)  :: im, jm
      integer, optional, intent(out) :: rc

      integer :: status, i, j
      type(FileMetadata)        :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable)            :: var
      real(REAL64), allocatable :: lons(:,:), lats(:,:)
      real(REAL64), allocatable :: corner_lons(:,:), corner_lats(:,:)
      real(REAL64) :: dlon, dlat

      dlon = 360.0_REAL64 / im
      dlat = 180.0_REAL64 / jm

      allocate(lons(im,jm), lats(im,jm))
      allocate(corner_lons(im+1,jm+1), corner_lats(im+1,jm+1))

      do j = 1, jm
         do i = 1, im
            lons(i,j) = -180.0_REAL64 + (i - 0.5_REAL64) * dlon
            lats(i,j) = -90.0_REAL64  + (j - 0.5_REAL64) * dlat
         end do
      end do
      do j = 1, jm+1
         do i = 1, im+1
            corner_lons(i,j) = -180.0_REAL64 + (i - 1) * dlon
            corner_lats(i,j) = -90.0_REAL64  + (j - 1) * dlat
         end do
      end do

      file_metadata = FileMetadata()
      call file_metadata%add_dimension('Xdim',  im)
      call file_metadata%add_dimension('Ydim',  jm)
      call file_metadata%add_dimension('XCdim', im+1)
      call file_metadata%add_dimension('YCdim', jm+1)
      call file_metadata%add_attribute('grid_type', Attribute('XY'))

      var = Variable(type=pFIO_REAL64, dimensions='Xdim,Ydim')
      call var%add_attribute('units', Attribute('degrees_east'))
      call file_metadata%add_variable('lons', var)

      var = Variable(type=pFIO_REAL64, dimensions='Xdim,Ydim')
      call var%add_attribute('units', Attribute('degrees_north'))
      call file_metadata%add_variable('lats', var)

      var = Variable(type=pFIO_REAL64, dimensions='XCdim,YCdim')
      call var%add_attribute('units', Attribute('degrees_east'))
      call file_metadata%add_variable('corner_lons', var)

      var = Variable(type=pFIO_REAL64, dimensions='XCdim,YCdim')
      call var%add_attribute('units', Attribute('degrees_north'))
      call file_metadata%add_variable('corner_lats', var)

      call formatter%create(file=filename, mode=pFIO_CLOBBER, rc=status)
      _VERIFY(status)
      call formatter%write(file_metadata, rc=status)
      _VERIFY(status)
      call formatter%put_var('lons',        lons,        rc=status) ; _VERIFY(status)
      call formatter%put_var('lats',        lats,        rc=status) ; _VERIFY(status)
      call formatter%put_var('corner_lons', corner_lons, rc=status) ; _VERIFY(status)
      call formatter%put_var('corner_lats', corner_lats, rc=status) ; _VERIFY(status)
      call formatter%close(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine create_xy_file_with_corners

   ! As create_xy_file but sets some lons/lats to MAPL_UNDEFINED_REAL64
   ! so that add_mask is exercised.
   subroutine create_xy_file_with_undef(filename, im, jm, rc)
      use mapl_InternalConstants, only: MAPL_UNDEFINED_REAL64
      character(len=*), intent(in)  :: filename
      integer,          intent(in)  :: im, jm
      integer, optional, intent(out) :: rc

      integer :: status, i, j
      type(FileMetadata)        :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable)            :: var
      real(REAL64), allocatable :: lons(:,:), lats(:,:)
      real(REAL64) :: dlon, dlat

      dlon = 360.0_REAL64 / im
      dlat = 180.0_REAL64 / jm

      allocate(lons(im,jm), lats(im,jm))
      do j = 1, jm
         do i = 1, im
            lons(i,j) = -180.0_REAL64 + (i - 0.5_REAL64) * dlon
            lats(i,j) = -90.0_REAL64  + (j - 0.5_REAL64) * dlat
         end do
      end do
      ! Mark the first column as undefined (e.g. off-Earth-disk as in ABI)
      lons(1,:) = MAPL_UNDEFINED_REAL64
      lats(1,:) = MAPL_UNDEFINED_REAL64

      file_metadata = FileMetadata()
      call file_metadata%add_dimension('Xdim', im)
      call file_metadata%add_dimension('Ydim', jm)
      call file_metadata%add_attribute('grid_type', Attribute('XY'))

      var = Variable(type=pFIO_REAL64, dimensions='Xdim,Ydim')
      call var%add_attribute('units', Attribute('degrees_east'))
      call file_metadata%add_variable('lons', var)

      var = Variable(type=pFIO_REAL64, dimensions='Xdim,Ydim')
      call var%add_attribute('units', Attribute('degrees_north'))
      call file_metadata%add_variable('lats', var)

      call formatter%create(file=filename, mode=pFIO_CLOBBER, rc=status)
      _VERIFY(status)
      call formatter%write(file_metadata, rc=status)
      _VERIFY(status)
      call formatter%put_var('lons', lons, rc=status) ; _VERIFY(status)
      call formatter%put_var('lats', lats, rc=status) ; _VERIFY(status)
      call formatter%close(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine create_xy_file_with_undef

end module mapl3g_XYTestHelper
