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

   integer :: n_lon, n_lat
   integer(kind=PIXEL_KIND), target, allocatable :: pixels(:,:)
   integer(kind=PIXEL_KIND), pointer :: pixels2(:,:)
   integer(kind=PIXEL_KIND), pointer :: p

   integer :: refine_lat, refine_lon
   integer :: level, num_levels
   integer :: n_lon_level, n_lat_level
   integer :: lon_level, lat_level
   integer, allocatable :: lat_factors(:)
   integer, allocatable :: lon_factors(:)

   type(Mesh), target :: m
   type(Element), pointer :: e
   integer :: ni, nj

   integer :: rc, status
   integer :: counters(4)
   integer :: catch_index

   integer(kind=INT64) :: c0, c1, crate
   integer :: i, n

   call MPI_Init(status)
   in_filename = 'GEOS5_10arcsec_mask.nc'
   call fill_pixels(pixels, in_filename, _RC)
   _HERE, 'raster shape: ', shape(pixels)

   n_lon = size(pixels, 1)
   n_lat = size(pixels, 2)

!#   call m%initialize(pixels, [-180.d0,+180.d0], [-90.d0, +90.d0])
   call m%initialize(pixels, [-180.d0,+180.d0], [-90.d0, +90.d0])
!#   pixels2 => pixels(2::3,2::3)
!#   call m%initialize(pixels2, [-180.d0,+180.d0], [-90.d0, +90.d0])
   e => m%get_element(1) ! archetype

   ni = size(e%pixels,1)
   nj = size(e%pixels,2)
   _HERE, 'equatorial shape: ', ni, nj

   lon_factors = get_factors(ni)
   lat_factors = get_factors(nj)
   _HERE, 'lon_factors: ', lon_factors
   _HERE, 'lat_factors: ', lat_factors

   num_levels = size(lat_factors) + size(lon_factors)
   level = 0
   lon_level = 0
   lat_level = 0
   n_lon_level = n_lon
   n_lat_level = n_lat

   print*,' level: ', level, '(', num_levels, ') # cells: ', m%num_elements()
   levels: do level = 1, num_levels
      call system_clock(c0, crate)
      if (n_lon_level >= n_lat_level) then ! refine east-west
         lon_level = lon_level + 1
         refine_lon = lon_factors(lon_level)
         n_lon_level = n_lon_level / lon_factors(lon_level)
         refine_lat = 1
      else ! refine north-south
         lat_level = lat_level + 1
         refine_lon = 1
         refine_lat = lat_factors(lat_level)
         n_lat_level = n_lat_level / lat_factors(lat_level)
      end if

      _HERE
      n = m%num_elements()
      do i = 1, n
         e => m%get_element(i)
         if (e%do_refine()) then
            call m%refine(e, refine_lon, refine_lat, _RC)
         else
            call e%set_fully_refined()
         end if
      end do

      call system_clock(c1, crate)
      print '(a,i2,2x,a,i10,1x,i2,1x,i2,3x,f5.3,5x,f8.2)',' level: ', level, '# cells: ', m%num_elements(), &
           refine_lon, refine_lat, real(m%num_elements()-2)/(4*real(n_lon/n_lon_level) * real(n_lat/n_lat_level)), &
           real(c1-c0)/crate
   end do levels

   ! Diagnostics
   counters = 0
   do i = 1, m%num_elements()
      e => m%get_element(i)
      _ASSERT(e%is_fully_refined(), 'hmm algorithm did not complete')
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
!#   call create_mesh(elements, m, _RC)
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
      _HERE, n_lon, n_lat

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

   function get_factors(n) result(factors)
      integer, allocatable :: factors(:)
      integer, intent(in) :: n

      integer :: i, m, p

      ! We want factors of 3 last to keep 3x3 catchments "together".  So "3" is the last prime we check.
      integer, parameter :: PRIMES(*) = [31, 29, 23, 19, 17, 13, 11, 7, 5, 2, 3]

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

!#   subroutine create_mesh(elements, mesh, rc)
!#      type(MeshElementMap), intent(in) :: elements
!#      type(ESMF_Mesh), intent(out) :: mesh
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#
!#      integer, allocatable :: nodeIds(:)
!#      real(kind=ESMF_KIND_R8), allocatable :: nodeCoords(:)
!#      integer :: nodeMask(4)
!#      integer, allocatable :: elementIds(:)
!#      integer, allocatable :: elementTypes(:)
!#      integer, allocatable :: elementConn(:)
!#
!#      integer :: num_elements
!#      integer :: i
!#
!#      num_elements = elements%size()
!#      allocate(elementIds(num_elements)
!#      allocate(elementTypes(num_elements), source=ESMF_MESHELEMTYPE_QUAD)
!#      allocate(elementConn(4*num_elements))
!#      do i = 1, num_elements
!#         e => ...
!#         elementIds(i) = i
!#         elementConn(i+0:i+3) = [e_node(1), e_node(2), e_node(3), e_node(4), MESH_POLYBREAK_IND]
!#      end do
!#      elementMask = [OCEAN_MASK, LAND_MASK, LAKE_MASK, LANDICE_MASK]
!#      
!#      mesh = ESMF_MeshCreate1Part(parametricDim=2, spatialDim=2, &
!#           nodeIds, nodeCoords, &
!#           elementIds, elementTypes, elementConn, elementMask&
!#           _RC)
!#      
!#   end subroutine create_mesh
end program main
