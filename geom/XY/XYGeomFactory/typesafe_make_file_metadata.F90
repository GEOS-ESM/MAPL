#include "MAPL.h"

submodule (mapl3g_XYGeomFactory) typesafe_make_file_metadata_smod
   use mapl_ErrorHandlingMod
   use pfio
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none (type, external)

contains

   module function typesafe_make_file_metadata(spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(XYGeomSpec), intent(in) :: spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: im, jm, i
      type(Variable) :: v
      real(kind=REAL64), allocatable :: fake_coord(:)

      im = spec%get_im_world()
      jm = spec%get_jm_world()

      call file_metadata%add_dimension('Xdim', im)
      call file_metadata%add_dimension('Ydim', jm)

      if (spec%get_has_corners()) then
         call file_metadata%add_dimension('XCdim', im+1)
         call file_metadata%add_dimension('YCdim', jm+1)
      end if

      ! Fake 1-D coordinate variables for GrADS/Panoply compatibility
      allocate(fake_coord(im))
      do i = 1, im
         fake_coord(i) = dble(i)
      end do
      v = Variable(type=PFIO_REAL64, dimensions='Xdim', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'Fake Longitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(fake_coord))
      call file_metadata%add_variable('Xdim', v)
      deallocate(fake_coord)

      allocate(fake_coord(jm))
      do i = 1, jm
         fake_coord(i) = dble(i)
      end do
      v = Variable(type=PFIO_REAL64, dimensions='Ydim', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'Fake Latitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(fake_coord))
      call file_metadata%add_variable('Ydim', v)
      deallocate(fake_coord)

      ! 2-D actual coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call file_metadata%add_variable('lons', v)

      v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call file_metadata%add_variable('lats', v)

      if (spec%get_has_corners()) then
         v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim', chunksizes=chunksizes)
         call v%add_attribute('long_name', 'longitude')
         call v%add_attribute('units', 'degrees_east')
         call file_metadata%add_variable('corner_lons', v)

         v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim', chunksizes=chunksizes)
         call v%add_attribute('long_name', 'latitude')
         call v%add_attribute('units', 'degrees_north')
         call file_metadata%add_variable('corner_lats', v)
      end if

      call file_metadata%add_attribute('grid_type', 'XY')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function typesafe_make_file_metadata

end submodule typesafe_make_file_metadata_smod
