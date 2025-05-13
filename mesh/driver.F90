#define I_AM_MAIN
#include "MAPL_ErrLog.h"
program main
   use mapl_ErrorHandling
   use sf_Point
   use sf_Pixel
   use sf_PixelVector
   use sf_MeshElement
   use sf_MeshElementVector
   use pfio
   use mpi
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none(type,external)

   type(Pixel), allocatable :: pixels(:,:)
   type(MeshElementVector), target :: elements
   integer :: refine_by
   logical :: done
   integer :: level, num_levels
   integer :: n, i
   character(:), allocatable :: in_filename
   character(:), allocatable :: out_filename
   integer, allocatable :: factors(:)
   type(MeshElement), pointer :: e

   integer :: rc, status
   integer :: counters(4)
   type(Pixel), pointer :: p
   integer :: catch_index

   call MPI_Init(status)
   in_filename = 'GEOS5_10arcsec_mask.nc'
   call fill_pixels(pixels, in_filename, _RC)
   _HERE, 'raster: ', shape(pixels)
   call coarse_refinement(elements, pixels)

   ! Assume n_lon is always 2x n_lat.
   factors = get_factors(size(pixels,2))
   _HERE, 'factors: ', factors
   num_levels = size(factors)

   done = .false.
   level = 0
   print*,' level: ', level, 'cells: ', elements%size()

   levels: do level = 1, num_levels
      done = .true. ! unless

      n = elements%size()
      do i = 1, n
         e => elements%of(i)
         if (do_refine(e)) then
            call refine(elements, i, refine_by=factors(level))
            done = .false.
         end if
      end do

      print*,' level: ', level, 'cells: ', elements%size(), factors(level), ' (done ? ', done, ')'
   end do levels

   ! Diagnostics
   counters = 0
   do i = 1, elements%size()
      e => elements%of(i)
      p => e%pixels%of(1)
      catch_index = p%catch_index
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
   _HERE,'counts: ',counters
   
!#   call write_to_file(elements, out_filename)

   call MPI_Finalize(status)
contains
#undef I_AM_MAIN
#include "MAPL_ErrLog.h"

   subroutine refine(elements, idx, refine_by)
      type(MeshElementVector), target, intent(inout) :: elements
      integer, intent(in) :: idx
      integer, intent(in) :: refine_by

      type(MeshElement) :: child
      integer :: i, j

      do j = refine_by, 1, -1
         do i = refine_by, 1, -1
            if (j==1 .and. i==1) then ! overwrite existing element last
               elements%of(idx) = get_child(elements%of(idx), i, j, refine_by, refine_by)
            else ! new element
               child = get_child(elements%of(idx), i, j, refine_by, refine_by)
               call elements%push_back(child)
            end if
         end do
      end do

   end subroutine refine

   subroutine fill_pixels(pixels, filename, rc) 
      type(Pixel), allocatable, intent(out) :: pixels(:,:)
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      type(NetCDF4_FileFormatter) :: formatter
      type(FileMetadata), target :: filemd

      integer :: n_lat, n_lon
      integer :: i, j
      integer :: status

      real(kind=REAL64), allocatable :: longitudes(:), latitudes(:)

!!$      integer, parameter :: SCALE=16
      integer, parameter :: SCALE=8
      call formatter%open(filename, mode=PFIO_READ, _RC)

      filemd = formatter%read(_RC)
      n_lon = filemd%get_dimension('N_lon')
      n_lat = filemd%get_dimension('N_lat')
      _HERE, n_lon, n_lat

!!$      allocate(pixels(n_lon, n_lat))
      allocate(pixels(n_lon/SCALE, n_lat/SCALE))
      allocate(longitudes(n_lon), latitudes(n_lat))

      call formatter%get_var('longitude', longitudes, _RC)
      call formatter%get_var('latitude', latitudes, _RC)

      do j = 1, n_lat/SCALE
         pixels(:,j)%center%longitude = longitudes(::SCALE)
      end do
      do i = 1, n_lon/SCALE
         pixels(i,:)%center%latitude = latitudes(::SCALE)
      end do

!!$      call formatter%get_var('CatchIndex', pixels(:,:)%catch_index, _RC)
      call formatter%get_var('CatchIndex', pixels(:,:)%catch_index, count=[n_lon/SCALE,n_lat/SCALE], stride=[SCALE,SCALE], _RC)
      _HERE, 'num lake pixels: ', count(pixels(:,:)%catch_index == 190000000)
      _HERE, 'min/max: ', minval(pixels(:,:)%catch_index), maxval(pixels(:,:)%catch_index)
      call formatter%close(_RC)
      _HERE, _SUCCESS

      _RETURN(_SUCCESS)
   end subroutine fill_pixels

   subroutine coarse_refinement(elements, pixels)
      type(MeshElementVector), target, intent(inout) :: elements
      type(Pixel), intent(in) :: pixels(:,:)

      real(kind=REAL64) :: lon_1, lon_2, lat_1, lat_2
      type(MeshElement) :: e
      integer :: i, j
      
      lon_1 = -180._REAL64
      lon_2 = +180._REAL64
      lat_1 = -90._REAL64
      lat_2 = +90._REAL64
      e = MeshElement(&
           lon_1=lon_1, lon_2=lon_2, lat_1=lat_1, lat_2=lat_2, &
           corners = reshape([Point(lon_1,lat_1), Point(lon_2, lat_1), Point(lon_2, lat_2), Point(lon_1, lat_2)], [2,2]), &
           pixels=PixelVector())

      do j = 1, size(pixels,2)
         do i = 1, size(pixels,1)
            if ( &
                 pixels(i,j)%center%longitude >= lon_1 .and. &
                 pixels(i,j)%center%longitude < lon_2 .and. &
                 pixels(i,j)%center%latitude >= lat_1 .and. &
                 pixels(i,j)%center%latitude < lat_2) then
               call e%pixels%push_back(pixels(i,j))
            end if
            end do
      end do
      call elements%push_back(e)
      
   end subroutine coarse_refinement

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

   function get_factors(n) result(factors)
      integer, allocatable :: factors(:)
      integer, intent(in) :: n

      integer :: i, m, p

      ! We want factors of 3 last to keep 3x3 catchments "together".  So "3" is the last prime we check.
      integer, parameter :: PRIMES(*) = [2, 5, 7, 11, 13, 3]

      factors = [integer :: ] ! empty
      m = n

      do while (m /= 1)
         do i = 1, size(PRIMES)
            p = PRIMES(i)
            if (mod(m, p) == 0) then
               factors = [factors, p]
               m = m / p
               exit
            end if
         end do
         if (i > size(PRIMES)) then
            error stop "Exceeded permitted prime factors"
         end if
      end do

   end function get_factors
      
end program main
