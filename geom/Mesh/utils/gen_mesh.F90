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

   character(:), allocatable:: in_filename

   integer(kind=PIXEL_KIND), target, allocatable :: pixels(:,:)
   integer(kind=PIXEL_KIND), pointer :: pixels2(:,:)
   integer(kind=PIXEL_KIND), pointer :: p

   integer :: refine_lat, refine_lon
   logical :: done
   integer :: level, num_levels
   
   type(Mesh), target :: m
   type(Element), pointer :: e
   integer :: ni, nj

   integer :: rc, status, connCount
   integer :: counters(4)
   integer :: catch_index

   integer(kind=INT64) :: c0, c1, crate
   integer :: i, n, nargs
   type(ESMF_Mesh)   :: msh, msh2
   real(kind=REAL64) :: MIN_RESOLUTION
   character(len=16) :: arg
   character(len=16) :: res
   character(len=256):: outfile

   nargs = command_argument_count()
    
   if (nargs == 0) then
     call print_usage()
     stop
   end if
   i = 1
   do while ( i <= nargs)
      call get_command_argument(i, arg, status=status)
      if (status /=0) then
        print*, "Error, cannot retrieve argument"
         stop 1
      endif
      select case ( trim(arg))
        case('-r', '--resolution')
          i = i + 1
          if (i > nargs) then
             print *, 'Error: -r/--resolution requires CXXX'
             stop 1
          end if
          call get_command_argument(i, res, status=status)
        case('-o', '--output')
          i = i + 1
          if (i > nargs) then
             print *, 'Error: -o/--optput requires filename '
             stop 1
          end if
          call get_command_argument(i, outfile, status=status)
        case ('-h', '--help')
           call print_usage()
           stop
                
        case default
           print *, 'Error: Unknown option: ', trim(arg)
           call print_usage()
           stop 1
        end select
        i = i + 1
   end do 

   select case (trim(res))
     case ('C48')
        MIN_RESOLUTION = 2.0d0
     case ('C90')
        MIN_RESOLUTION = 1.0d0
     case ('C180')
        MIN_RESOLUTION = 1.0d0/2
     case ('C360')
        MIN_RESOLUTION = 1.0d0/4
     case ('C720')
        MIN_RESOLUTION = 1.0d0/8
     case ('C1440')
        MIN_RESOLUTION = 1.0d0/16
     case ('C2880')
        MIN_RESOLUTION = 1.0d0/32
     case ('C5760')
        MIN_RESOLUTION = 1.0d0/64
     case ('C000')
        MIN_RESOLUTION = 0.d0 !fully resolved
     case default
        print*, "not supported"
        stop 1
   end select

   call MPI_Init(status)
   _HERE
   in_filename = 'GEOS5_10arcsec_mask.nc'
   call fill_pixels(pixels, in_filename, _RC)
   _HERE, 'raster shape: ', shape(pixels)

   call m%initialize(pixels, [-180.d0,+180.d0], [-90.d0, +90.d0])
   e => m%get_element(1) ! archetype

   level = 0
   
   levels: do while (.not. done)
      level = level + 1
      done = .true. ! unless
      call system_clock(c0, crate)

      n = m%num_elements()
      do i = 1, n
         e => m%get_element(i)
         if (e%do_refine() .and. m%resolution(e) > MIN_RESOLUTION ) then
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

      catch_index = e%get_type()
      counters(catch_index) = counters(catch_index) + 1
   end do
   _HERE,'counts: ocean: ', counters(1), 'land: ', counters(2), 'lake: ', counters(3), 'landice: ', counters(4)

   call ESMF_Initialize(_RC)

   call m%to_netcdf( trim(outfile)//'.nc4', _RC)

   !call system_clock(c0)
   !msh = m%make_esmf_mesh(_RC)
   !call write_to_file(msh, 'surface_mesh_to_nc4.nc', _RC)
   !call system_clock(c1)

   call m%reorder_elements(_RC)

   call m%to_netcdf(trim(outfile)//'_ordered.nc4', _RC)

  ! call system_clock(c0)
  ! msh = m%make_esmf_mesh_tri(connCount, _RC)
  ! call write_to_file(msh, 'surface_mesh_tri_ordered.nc', connCount, _RC)

   !_HERE, 'time to create ESMF mesh is: ', real(c1-c0)/crate

   call ESMF_Finalize(_RC)

!#   call MPI_Finalize(status)
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

   subroutine print_usage()
        print *, 'Usage: generate the mesh [OPTIONS]'
        print *, 'The file GEOS5_10arcsec_mask.nc should be with driver.x'
        print *, 'Options:'
        print *, '  -r, --resolution  CXX  Input minimum resolution to stop searching smaller area (required)'
        print *, '  -o, --output FILE      Output file, no need suffix nc4 (require)'
        print *, '  -h, --help             Display this help message'
        print *, ''
        print *, 'Example:'
        print *, '  ./gen_mesh.x -r C1440 -o surface_meshC1440'
   end subroutine print_usage

end program main
