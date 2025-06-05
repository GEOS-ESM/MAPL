#define I_AM_MAIN
#include "MAPL_ErrLog.h"
program main
   use mapl_ErrorHandling
   use sf_Mesh
   use sf_Element
   use sf_Vertex
   use pfio
   use esmf
   use mpi
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   implicit none(type,external)

   character(:), allocatable :: in_filename
   character(:), allocatable :: out_filename

   integer(kind=PIXEL_KIND), target, allocatable :: pixels(:,:)
   integer(kind=PIXEL_KIND), pointer :: pixels2(:,:)
   integer(kind=PIXEL_KIND), pointer :: p

   integer :: refine_lat, refine_lon
   logical :: done
   integer :: level, num_levels
   
   type(Mesh), target :: m
   type(Element), pointer :: e
   integer :: ni, nj

   integer :: rc, status
   integer :: counters(4)
   integer :: catch_index

   integer(kind=INT64) :: c0, c1, crate
   integer :: i, n
   type(ESMF_Mesh) :: msh

   call MPI_Init(status)
   in_filename = 'GEOS5_10arcsec_mask.nc'
   call fill_pixels(pixels, in_filename, _RC)
   _HERE, 'raster shape: ', shape(pixels)

   call m%initialize(pixels, [-180.d0,+180.d0], [-90.d0, +90.d0])
   e => m%get_element(1) ! archetype

   level = 0

   print*,' level: ', level, '(', num_levels, ') # cells: ', m%num_elements()
   levels: do while (.not. done)
      level = level + 1

!#      if (level > 18) exit
      done = .true. ! unless
      call system_clock(c0, crate)

      n = m%num_elements()
      do i = 1, n
         e => m%get_element(i)
         if (e%do_refine()) then
            call m%refine(e, _RC)
            done = .false.
         else
            call e%set_fully_refined()
         end if
      end do

      call system_clock(c1, crate)
      print '(a,i2,2x,a,i10,1x,3x,5x,f8.2)',' level: ', level, '# cells: ', m%num_elements(), &
           real(c1-c0)/crate
   end do levels

   ! Diagnostics
   counters = 0
   do i = 1, m%num_elements()
      e => m%get_element(i)
!#      _ASSERT(.not. e%do_refine(), 'hmm algorithm did not complete')

      p => e%pixels(1,1)
      catch_index = p
      select case (catch_index)
      case (0)
         counters(1) = counters(1) + 1
      case (1:291284)
         counters(2) = counters(2) + 1
      case (190000000)
         counters(3) = counters(3) + 1
      case (200000000)
         counters(4) = counters(4) + 1
      end select
   end do
   _HERE,'counts: ocean: ', counters(1), 'land: ', counters(2), 'lake: ', counters(3), 'landice: ', counters(4)

   call ESMF_Initialize(_RC)
   msh = m%make_esmf_mesh(_RC)
!#   call write_to_file(elements, out_filename)
   call ESMF_Finalize(_RC)

   call MPI_Finalize(status)
contains
#undef I_AM_MAIN
#include "MAPL_ErrLog.h"

   subroutine fill_pixels(pixels, filename, rc) 
      integer(kind=PIXEL_KIND), allocatable, intent(out) :: pixels(:,:)
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      type(NetCDF4_FileFormatter) :: formatter
      type(FileMetadata), target :: filemd

      integer :: n_lat, n_lon
      integer :: status

      real(kind=REAL64), allocatable :: longitudes(:), latitudes(:)
      character(*), parameter :: LANDCOVER = 'CatchIndex'
!!$      character(*), parameter :: LANDCOVER = 'landcover'

      call formatter%open(filename, mode=PFIO_READ, _RC)

      filemd = formatter%read(_RC)
      n_lon = filemd%get_dimension('N_lon')
      n_lat = filemd%get_dimension('N_lat')
!!$      n_lon = filemd%get_dimension('longitude')
!!$      n_lat = filemd%get_dimension('latitude')

      allocate(longitudes(n_lon), latitudes(n_lat))
      allocate(pixels(n_lon,n_lat))

      call formatter%get_var('longitude', longitudes, _RC)
      call formatter%get_var('latitude', latitudes, _RC)

      call formatter%get_var(LANDCOVER, pixels(:,:n_lat/4), &
           count=[n_lon,n_lat/4], start=[1,1], _RC)
      call formatter%get_var(LANDCOVER, pixels(:,n_lat/4+1:n_lat/2), &
           count=[n_lon,n_lat/4], start=[1,1+n_lat/4], _RC)
      call formatter%get_var(LANDCOVER, pixels(:,n_lat/2+1:3*n_lat/4), &
           count=[n_lon,n_lat/4], start=[1,1+n_lat/2], _RC)
      call formatter%get_var(LANDCOVER, pixels(:,3*n_lat/4+1:), &
           count=[n_lon,n_lat/4], start=[1,1+3*n_lat/4], _RC)

      call formatter%close(_RC)

      where (pixels >= 1 .and. pixels < 300000)
         pixels=1
      end where
      _RETURN(_SUCCESS)
   end subroutine fill_pixels


!#   subroutine write_to_file(elements, filename)
!#
!#      type(Variable) :: catch_index
!#
!#      file_md = FileMetadata()
!#      call file_md%add_dimension('elementCount', elements%size())
!#      call file_md%add_dimension('nodeCount', ???)
!#      call file_md%add_dimension('coordDim', 2)
!#
!#      catch_index = Variable(PFIO_INT, dimensions='num_elements')
!#      call catch_index%add_attribute('CatchIndex', "Ocean (0) Land (1-291284) Lakes (190000000) ice (200000000)")
!#      call file_md%add_variable('CatchIndex', catch_index)
!#
!#      element_mask = Variable(PFIO_INT, dimensions='elementCount')
!#      call element_mask%add_attribute('elementMask', "Ocean (0) Land (1) Lakes (190000000) ice (200000000)")
!#      call file_md%add_variable('elementMask', element_mask)
!#
!#      node_coords = Variable(PFIO_REAL64, dimensions='codeCount,coordDim')
!#      call node_coords%add_attribute('units', 'degrees')
!#      call file_md%add_variable('nodeCoords', node_coords)
!#
!#
!#      call formatter%open(file, mode=PFIO_WRITE, _RC)
!#
!#      call formatter%...(file_md)
!#
!#      call formatter%put_var('CatchIndex', elements%catch_index..., _RC)
!#      call formatter%put_var('elementMask', ..., _RC)
!#      call formatter%put_var('nodeCoords', ..., _RC)
!#
!#      call formatter%close(_RC)
!#      
!#   end subroutine write_to_file

end program main
