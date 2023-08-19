#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) LatLonGeomSpec_smod
   use mapl3g_GeomSpec
   use mapl3g_HConfigUtils
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   
   integer, parameter :: R8 = ESMF_KIND_R8

contains


   ! Basic constructor for LatLonGeomSpec
   module function new_LatLonGeomSpec(lon_axis, lat_axis, decomposition) result(spec)
      type(LatLonGeomSpec) :: spec
      type(LatLonAxis), intent(in) :: lon_axis
      type(LatLonAxis), intent(in) :: lat_axis
      type(LatLonDecomposition), intent(in) :: decomposition
      
      spec%lon_axis = lon_axis
      spec%lat_axis = lat_axis
      spec%decomposition = decomposition
      
   end function new_LatLonGeomSpec


   pure logical module function equal_to(a, b)
      class(LatLonGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      select type (b)
      type is (LatLonGeomSpec)
         equal_to = (a%lon_axis == b%lon_axis) .and. (a%lat_axis == b%lat_axis)
         if (.not. equal_to) return
         equal_to = (a%decomposition == b%decomposition)
      class default
         equal_to = .false.
      end select

   end function equal_to


   ! HConfig section
   module function make_LatLonGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      logical :: is_regional
      integer :: status

      spec%lon_axis = make_LonAxis(hconfig, _RC)
      spec%lat_axis = make_LatAxis(hconfig, _RC)
      associate (im => spec%lon_axis%get_extent(), jm => spec%lat_axis%get_extent())
        spec%decomposition = make_Decomposition(hconfig, dims=[im,jm], _RC)
      end associate

      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_hconfig

   function make_decomposition(hconfig, dims, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc
      integer, allocatable :: ims(:), jms(:)
      integer :: nx, ny

      integer :: status
      logical :: has_ims, has_jms, has_nx, has_ny

      has_ims = ESMF_HConfigIsDefined(hconfig, keystring='ims', _RC)
      has_jms = ESMF_HConfigIsDefined(hconfig, keystring='jms', _RC)
      _ASSERT(has_ims .eqv. has_jms, 'ims and jms must be both defined or both undefined')

      if (has_ims) then
         call MAPL_GetResource(ims, hconfig, 'ims', _RC)
         call MAPL_GetResource(jms, hconfig, 'jms', _RC)
         decomp = LatLonDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      has_nx = ESMF_HConfigIsDefined(hconfig, keystring='nx', _RC)
      has_ny = ESMF_HConfigIsDefined(hconfig, keystring='ny', _RC)
      _ASSERT(has_nx .eqv. has_ny, 'nx and ny must be both defined or both undefined')

      if (has_nx) then
         call MAPL_GetResource(nx, hconfig, 'nx', _RC)
         call MAPL_GetResource(ny, hconfig, 'ny', _RC)
         decomp = LatLonDecomposition(dims, topology=[nx, ny])
         _RETURN(_SUCCESS)
      end if

      ! Invent a decomposition
      decomp = make_LatLonDecomposition(dims, _RC)
      
      _RETURN(_SUCCESS)
   end function make_decomposition

!#   module function get_distribution(hconfig, m_world, key_npes, key_distribution, rc) result(distribution)
!#      integer, allocatable :: distribution(:)
!#      type(ESMF_HConfig), intent(in) :: hconfig
!#      integer, intent(in) :: m_world
!#      character(len=*), intent(in) :: key_npes
!#      character(len=*), intent(in) :: key_distribution
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      integer :: nx
!#      integer, allocatable :: ims(:)
!#      logical :: has_distribution
!#
!#      call MAPL_GetResource(nx, hconfig, key_npes, _RC)
!#      _ASSERT(nx > 0, key_npes // ' must be greater than 0.')
!#
!#      has_distribution = ESMF_HConfigIsDefined(hconfig, keystring=key_distribution, _RC)
!#      if (has_distribution) then
!#         call MAPL_GetResource(ims, hconfig, key_distribution, _RC)
!#         _ASSERT(size(ims) == nx, 'inconsistent processor distribution')
!#         _ASSERT(sum(ims) == m_world, 'Requested pe distribution inconsistent with grid resolution.')
!#      else
!#         allocate(ims(nx))
!#         call MAPL_DecomposeDim(m_world, ims, nx, min_DE_extent=2)
!#      end if
!#
!#      distribution = ims
!#      
!#      _RETURN(_SUCCESS)
!#   end function get_distribution
!#
  
   ! File metadata section

   ! Unfortunately, we cannot quite compute each axis (lat - lon) independently,
   ! as the optimal decomposition depends on the ratio of the extens along each
   ! dimension.
   module function make_LatLonGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=R8), allocatable :: lon_centers(:)
      real(kind=R8), allocatable :: lat_centers(:)
      real(kind=R8), allocatable :: lon_corners(:)
      real(kind=R8), allocatable :: lat_corners(:)
      integer :: im_world, jm_world
      integer :: nx_ny(2)
      integer, allocatable :: lon_distribution(:)
      integer, allocatable :: lat_distribution(:)
      type(LatLonAxis) :: lon_axis, lat_axis
      type(LatLonDecomposition) :: decomposition

      lon_centers = get_coordinates(file_metadata, 'lon', 'longitude', _RC)
      im_world = size(lon_centers)
      ! Enforce convention for longitude range.
      if (any((lon_centers(2:im_world) - lon_centers(1:im_world-1)) < 0)) then
         where(lon_centers > 180) lon_centers = lon_centers - 360
      end if
      lon_corners = get_lon_corners(lon_centers)

      lat_centers = get_coordinates(file_metadata, 'lat', 'latitude', _RC)
      jm_world = size(lat_centers)
      call fix_bad_pole(lat_centers)
      lat_corners = get_lat_corners(lat_centers)
      ! fix corners
      if (lat_corners(1) < -90) lat_corners(1) = -90
      if (lat_corners(jm_world+1) > 90) lat_corners(jm_world+1) = 90

      lon_axis = LatLonAxis(lon_centers, lon_corners)
      lat_axis = LatLonAxis(lat_centers, lat_corners)
      decomposition = make_LatLonDecomposition([im_world, jm_world], _RC)

      spec = LatLonGeomSpec(lon_axis, lat_axis, decomposition)
      
      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_metadata

   module function make_distribution(im, nx) result(distribution)
      integer, allocatable :: distribution(:)
      integer, intent(in) :: im, nx

      allocate(distribution(nx))
      call MAPL_DecomposeDim(im, distribution, nx, min_DE_extent=2)

   end function make_distribution


   module function get_coordinates_try(file_metadata, try1, try2, rc) result(coordinates)
      real(kind=R8), allocatable :: coordinates(:)
      type(FileMetadata), intent(in) :: file_metadata
      character(*), intent(in) :: try1, try2
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dim_name

      dim_name = get_dim_name(file_metadata, try1, try2, _RC)
      coordinates = get_coordinates(file_metadata, dim_name, _RC)

      _RETURN(_SUCCESS)
   end function get_coordinates_try

   module function get_coordinates_dim(file_metadata, dim_name, rc) result(coordinates)
      real(kind=R8), dimension(:), allocatable :: coordinates
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), intent(in) :: dim_name
      integer, optional, intent(out) :: rc

      integer :: status
      class (CoordinateVariable), pointer :: v
      class (*), pointer :: ptr(:)

      v => file_metadata%get_coordinate_variable(dim_name, _RC)
      ptr => v%get_coordinate_data()
      _ASSERT(associated(ptr),'coordinate data not allocated')

      select type (ptr)
      type is (real(kind=REAL64))
         coordinates = ptr
      type is (real(kind=REAL32))
         coordinates = ptr
      class default
         _FAIL('unsuppoted kind for coordinate data -- must be REAL32 or REAL64')
      end select

      _RETURN(_SUCCESS)
   end function get_coordinates_dim


   module function get_lon_corners(centers) result(corners)
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), allocatable :: corners(:)

      associate (im => size(centers))
        allocate(corners(im+1))
        corners(1) = (centers(im) + centers(1))/2 - 180
        corners(2:im) = (centers(1:im-1) + centers(2:im))/2
        corners(im+1) = (centers(im) + centers(1))/2 + 180
      end associate
   end function get_lon_corners


   module function get_lat_corners(centers) result(corners)
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), allocatable :: corners(:)

      associate (jm => size(centers))
        allocate(corners(jm+1))
         corners(1) = centers(1) - (centers(2)-centers(1))/2
         corners(2:jm) = (centers(1:jm-1) + centers(2:jm))/2
         corners(jm+1) = centers(jm) + (centers(jm)-centers(jm-1))/2
      end associate
   end function get_lat_corners


   ! Magic code from ancient times.
   ! Do not touch unless you understand ...
   module subroutine fix_bad_pole(centers)
      real(kind=R8), intent(inout) :: centers(:)

      integer :: n
      real(kind=R8) :: d_lat, d_lat_loc, extrap_lat
      real, parameter :: tol = 1.0e-5
      integer :: i

      if (size(centers) < 4) return ! insufficient data

      ! Check: is this a "mis-specified" pole-centered grid?
      ! Assume lbound=1 and ubound=size for now

      n = size(centers)
      d_lat = (centers(n-1) - centers(2)) / (n - 3)

      ! Check: is this a regular grid (i.e. constant spacing away from the poles)?
      if (any(((centers(2:n-1) - centers(1:n-2)) - d_lat) < tol*d_lat)) return

      ! Should the southernmost point actually be at the pole?
      extrap_lat = centers(2) - d_lat
      if (extrap_lat <= ((d_lat/20.0)-90.0)) then
         centers(1) = -90.0
      end if

      ! Should the northernmost point actually be at the pole?
      extrap_lat = centers(n-1) + d_lat
      if (extrap_lat >= (90.0-(d_lat/20.0))) then
         centers(n) =  90.0
      end if

   end subroutine fix_bad_pole

   module function get_dim_name(file_metadata, try1, try2, rc) result(dim_name)
      character(len=:), allocatable :: dim_name
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), intent(in) :: try1
      character(len=*), intent(in) :: try2
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_try1, has_try2

      dim_name = '' ! unless
      has_try1= file_metadata%has_dimension(try1, _RC)
      has_try2= file_metadata%has_dimension(try2, _RC)
      _ASSERT(has_try1 .neqv. has_try2, 'Exactly one of "//try1//" and "//try2//" should defined in file_metadata')
      if (has_try1) then
         dim_name = try1
         _RETURN(_SUCCESS)
      end if
      
      if (has_try2) then
         dim_name = try2
         _RETURN(_SUCCESS)
      end if

      ! No path to get here
      _RETURN(_FAILURE)
   end function get_dim_name


!#   ! ------------------------------------------------------------------------------------
!#   ! This module function attempts to find a layout with roughly square
!#   ! domains on each process.  Optimal value for
!#   !     nx = (im_world * petcount) / jm_world
!#   ! Except, it needs to be an integer
!#   ! --------------------------------------------------------------------
!#   module function make_de_layout_petcount(aspect_ratio, petCount) result(nx_ny)
!#      integer :: nx_ny(2)
!#      real, intent(in) :: aspect_ratio
!#      integer, intent(in) :: petCount
!#
!#      integer :: nx, ny
!#      integer :: start
!#
!#      ! NOTE: Final iteration (nx=1) is guaranteed to succeed.
!#      start = floor(sqrt(petCount * aspect_ratio))
!#      do nx = start, 1, -1
!#         if (mod(petcount, nx) == 0) then ! found a decomposition
!#            ny = petCount / nx
!#            exit
!#         end if
!#      end do
!#
!#      nx_ny = [nx, ny]
!#
!#   end function make_de_layout_petcount
!#
!#   module function make_de_layout_vm(aspect_ratio, vm, rc) result(nx_ny)
!#      integer :: nx_ny(2)
!#      real, optional, intent(in) :: aspect_ratio
!#      type(ESMF_VM), optional, intent(in) :: vm
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      real :: aspect_ratio_
!#      type(ESMF_VM) :: vm_
!#      integer :: petCount
!#
!#      aspect_ratio_ = 1.0
!#      if (present(aspect_ratio)) aspect_ratio_ = aspect_ratio
!#
!#      if (present(vm)) then
!#         vm_ = vm
!#      else
!#         call ESMF_VMGetCurrent(vm_, _RC)
!#      end if
!#      call ESMF_VMGet(vm_, petCount=petCount, _RC)
!#
!#      nx_ny = make_de_layout(aspect_ratio, petCount)
!#
!#      _RETURN(_SUCCESS)
!#   end function make_de_layout_vm
!#

   ! Accessors
   pure module function get_lon_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LatLonAxis) :: axis
      axis = spec%lon_axis
   end function get_lon_axis

   pure module function get_lat_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LatLonAxis) :: axis
      axis = spec%lat_axis
   end function get_lat_axis


   pure module function get_decomposition(spec) result(decomposition)
      type(LatLonDecomposition) :: decomposition
      class(LatLonGeomSpec), intent(in) :: spec

      decomposition = spec%decomposition
   end function get_decomposition

   logical module function supports_hconfig(this, hconfig, rc) result(supports)
      class(LatLonGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: flag1, flag2
      
      supports = .false.
      
      flag1 = ESMF_HConfigIsDefined(hconfig, keystring='im_world', _RC)
      _RETURN_UNLESS(flag1)
      flag1 = ESMF_HConfigIsDefined(hconfig, keystring='jm_world', _RC)
      _RETURN_UNLESS(flag1)

      flag1 = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
      flag2 = ESMF_HConfigIsDefined(hconfig, keystring='dateline', _RC)
      _RETURN_UNLESS(flag1 .neqv. flag2)

      flag1 = ESMF_HConfigIsDefined(hconfig, keystring='lat_range', _RC)
      flag2 = ESMF_HConfigIsDefined(hconfig, keystring='pole', _RC)
      _RETURN_UNLESS(flag1 .neqv. flag2)

      supports = .true.

      _RETURN(_SUCCESS)
   end function supports_hconfig

   logical module function supports_metadata(this, file_metadata, rc) result(supports)
      class(LatLonGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: flag
      character(:), allocatable :: lon_name, lat_name

      supports = .false.

      lon_name = get_dim_name(file_metadata, 'lon', 'longitude', _RC)
      lat_name = get_dim_name(file_metadata, 'lat', 'latitude', _RC)

      flag = file_metadata%has_variable(lon_name, _RC)
      _RETURN_UNLESS(flag)

      flag = file_metadata%has_variable(lat_name, _RC)
      _RETURN_UNLESS(flag)

      supports = .true.

      _RETURN(_SUCCESS)
   end function supports_metadata

end submodule LatLonGeomSpec_smod
