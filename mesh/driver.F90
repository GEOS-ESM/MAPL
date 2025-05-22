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
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT64
   implicit none(type,external)

   character(:), allocatable :: in_filename
   character(:), allocatable :: out_filename

   integer :: n_lon, n_lat
   integer :: level_n_lon, level_n_lat
   type(Pixel), target, allocatable :: pixels(:,:)
   type(MeshElementVector), target :: elements

   integer :: refine_lat, refine_lon
   logical :: done
   integer :: level, num_levels
   integer :: level_lon, level_lat
   integer, allocatable :: factors_lat(:)
   integer, allocatable :: factors_lon(:)

   type(MeshElement), pointer :: e

   integer :: rc, status
   integer :: counters(4)
   type(Pixel), pointer :: p
   integer :: catch_index

   integer(kind=INT64) :: c0, c1, crate
   integer(kind=INT64) :: i, n

   call MPI_Init(status)
   in_filename = 'GEOS5_10arcsec_mask.nc'
   call fill_pixels(pixels, in_filename, _RC)
   _HERE, 'raster: ', shape(pixels)

   ! Assume n_lon is always 2x n_lat.
   n_lon = size(pixels, 1)
   n_lat = size(pixels, 2)

   call coarse_refinement(elements, pixels)

   factors_lon = get_factors(n_lon)
   factors_lat = get_factors(n_lat)
   _HERE, 'factors: ', factors_lat

   num_levels = size(factors_lat) + size(factors_lon)
   done = .false.
   level = 0
   level_lon = 0
   level_lat = 0
   level_n_lon = n_lon
   level_n_lat = n_lat

   print*,' level: ', level, '(', num_levels, ') # cells: ', elements%size() 
   levels: do level = 1, num_levels
      call system_clock(c0, crate)
      if (level_n_lon >= level_n_lat) then
         level_lon = level_lon + 1
         refine_lon = factors_lon(level_lon)
         level_n_lon = level_n_lon / factors_lon(level_lon)
         refine_lat = 1
      else
         level_lat = level_lat + 1
         refine_lon = 1
         refine_lat = factors_lat(level_lat)
         level_n_lat = level_n_lat / factors_lat(level_lat)
      end if

      done = .true. ! unless

      n = elements%size()
      do i = 1, n
         e => elements%of(i)
         if (do_refine(e)) then
            call refine(elements, i, refine_lon, refine_lat)
            done = .false.
         end if
      end do

      call system_clock(c1, crate)
      print '(a,i2,2x,a,i10,1x,i2,1x,i2,3x,f5.3,5x,f8.2)',' level: ', level, '# cells: ', elements%size(), &
           refine_lon, refine_lat, real(elements%size())/(real(n_lon/level_n_lon) * real(n_lat/level_n_lat)), &
           real(c1-c0)/crate
   end do levels

   ! Diagnostics
   counters = 0
   do i = 1, elements%size()
      e => elements%of(i)
      p => e%pixels(1,1)
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

   subroutine refine(elements, idx, refine_lon, refine_lat)
      type(MeshElementVector), target, intent(inout) :: elements
      integer(kind=INT64), intent(in) :: idx
      integer, intent(in) :: refine_lon, refine_lat

      type(MeshElement) :: child
      integer :: i, j

      do j = refine_lat, 1, -1
         do i = refine_lon, 1, -1
            if (j==1 .and. i==1) then ! overwrite existing element last
               elements%of(idx) = get_child(elements%of(idx), i, j, refine_lon, refine_lat)
            else ! new element
               child = get_child(elements%of(idx), i, j, refine_lon, refine_lat)
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

      call formatter%open(filename, mode=PFIO_READ, _RC)

      filemd = formatter%read(_RC)
      n_lon = filemd%get_dimension('N_lon')
      n_lat = filemd%get_dimension('N_lat')
      _HERE, n_lon, n_lat

      allocate(longitudes(n_lon), latitudes(n_lat))
      allocate(pixels(n_lon,n_lat))

      call formatter%get_var('longitude', longitudes, _RC)
      call formatter%get_var('latitude', latitudes, _RC)

      do j = 1, n_lat
         pixels(:,j)%center%longitude = longitudes
      end do
      do i = 1, n_lon
         pixels(i,:)%center%latitude = latitudes
      end do

!#      call formatter%get_var('CatchIndex', pixels(:,:)%catch_index, _RC)
      _HERE
      call formatter%get_var('CatchIndex', pixels(:,:n_lat/4)%catch_index, &
           count=[n_lon,n_lat/4], start=[1,1], _RC)
     _HERE
      call formatter%get_var('CatchIndex', pixels(:,n_lat/4+1:n_lat/2)%catch_index, &
           count=[n_lon,n_lat/4], start=[1,1+n_lat/4], _RC)
      _HERE
      call formatter%get_var('CatchIndex', pixels(:,n_lat/2+1:3*n_lat/4)%catch_index, &
           count=[n_lon,n_lat/4], start=[1,1+n_lat/2], _RC)
      _HERE
      call formatter%get_var('CatchIndex', pixels(:,3*n_lat/4+1:)%catch_index, &
           count=[n_lon,n_lat/4], start=[1,1+3*n_lat/4], _RC)
      _HERE

      call formatter%close(_RC)

      _RETURN(_SUCCESS)
   end subroutine fill_pixels

   subroutine coarse_refinement(elements, pixels)
      type(MeshElementVector), target, intent(inout) :: elements
      type(Pixel), target, intent(in) :: pixels(:,:)

      real(kind=REAL64) :: lon_1, lon_2, lat_1, lat_2
      type(MeshElement) :: e
      
      lon_1 = -180.
      lon_2 = +180.
      lat_1 = -90.
      lat_2 = +90.

      e = MeshElement(&
           lon_1=lon_1, lon_2=lon_2, lat_1=lat_1, lat_2=lat_2, &
           pixels=pixels)
      e%pixels => pixels

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
