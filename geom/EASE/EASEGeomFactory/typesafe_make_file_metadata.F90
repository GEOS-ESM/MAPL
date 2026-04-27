#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) typesafe_make_file_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl3g_EASEConversion
   use mapl_ErrorHandlingMod
   use pfio
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none (type, external)

contains

   module function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(EASEGeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      type(Variable) :: v
      real(kind=REAL64), allocatable :: lon_cen(:), lat_cen(:)

      call compute_lon_centers_(geom_spec, lon_cen)
      call compute_lat_centers_(geom_spec, lat_cen)

      call file_metadata%add_dimension('lon', geom_spec%get_im_world())
      call file_metadata%add_dimension('lat', geom_spec%get_jm_world())

      ! Longitude coordinate variable
      v = Variable(type=PFIO_REAL64, dimensions='lon', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(lon_cen))
      call file_metadata%add_variable('lon', v)

      ! Latitude coordinate variable
      v = Variable(type=PFIO_REAL64, dimensions='lat', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(lat_cen))
      call file_metadata%add_variable('lat', v)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function typesafe_make_file_metadata

   subroutine compute_lon_centers_(spec, centers)
      type(EASEGeomSpec), intent(in) :: spec
      real(kind=REAL64), allocatable, intent(out) :: centers(:)

      integer :: cols, i
      real(kind=REAL64) :: delta

      cols  = spec%get_im_world()
      delta = 360.0_REAL64 / cols
      allocate(centers(cols))
      do i = 1, cols
         centers(i) = -180.0_REAL64 + (i - 0.5_REAL64)*delta
      end do
   end subroutine compute_lon_centers_

   subroutine compute_lat_centers_(spec, centers)
      type(EASEGeomSpec), intent(in) :: spec
      real(kind=REAL64), allocatable, intent(out) :: centers(:)

      integer :: rows, row
      real :: lat, tmplon, s
      character(len=:), allocatable :: grid_name

      grid_name = spec%get_grid_name()
      rows      = spec%get_jm_world()
      allocate(centers(rows))
      do row = 0, rows-1
         s = real(row)
         call ease_inverse(grid_name, 0., s, lat, tmplon)
         centers(rows - row) = real(lat, kind=REAL64)
      end do
   end subroutine compute_lat_centers_

end submodule typesafe_make_file_metadata_smod
