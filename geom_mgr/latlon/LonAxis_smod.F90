#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) LonAxis_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
!   use hconfig3g
   use esmf
   implicit none
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   ! Constructor
   pure module function new_LonAxis(centers, corners) result(axis)
      type(LonAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LonAxis


   module function make_LonAxis_from_hconfig(hconfig, rc) result(axis)
      type(LonAxis) :: axis
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im_world
      real(kind=R8), allocatable :: centers(:), corners(:)
      type(AxisRanges) :: ranges
      logical :: found

      !call MAPL_HConfigGet(hconfig, 'im_world', im_world, found=found, _RC)
      im_world = ESMF_HConfigAsI4(hconfig, keyString='im_world', asOkay=found, _RC)
      _ASSERT(found, '"im_world" not found.')
      _ASSERT(im_world > 0, "Config parameter 'im_world' must be greater than 0.")

      ranges = get_lon_range(hconfig, im_world, _RC)
      centers = MAPL_Range(ranges%center_min, ranges%center_max, im_world, _RC)
      corners = MAPL_Range(ranges%corner_min, ranges%corner_max, im_world+1, _RC)

      axis%CoordinateAxis = CoordinateAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_LonAxis_from_hconfig

   module function get_lon_range(hconfig, im_world, rc) result(ranges)
      type(AxisRanges) :: ranges
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: im_world
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=R8) :: delta
      character(:), allocatable :: dateline
      real(kind=ESMF_KIND_R4), allocatable :: t_range(:)
      logical :: has_range
      logical :: has_dateline

      has_range = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
      has_dateline = ESMF_HConfigIsDefined(hconfig, keystring='dateline', _RC)
      _ASSERT(has_range .neqv. has_dateline, 'Exactly one of lon_range or dateline must be defined in hconfig')

      if (has_range) then ! is regional
         t_range = ESMF_HConfigAsI4Seq(hconfig, keyString='lon_range', _RC)
      !   call MAPL_HConfigGet(hconfig, 'lon_range', t_range, _RC)
         _ASSERT(size(t_range) == 2, 'illegal size of lon_range')
         _ASSERT(t_range(1) < t_range(2), 'illegal lon_range')
         delta = (t_range(2) - t_range(1)) / im_world

         ranges%corner_min = t_range(1)
         ranges%corner_max = t_range(2)
         ranges%center_min = t_range(1) + delta/2
         ranges%center_max = t_range(2) - delta/2
         _RETURN(_SUCCESS)
      end if

      delta = 360.d0 / im_world
!      call MAPL_HConfigGet(hconfig, 'dateline', dateline, _RC)
      dateline = ESMF_HConfigAsString(hconfig, keyString='dateline', _RC)
      select case (dateline)
      case ('DC')
         ranges%corner_min = -180.d0 - delta/2
         ranges%corner_max = +180.d0 - delta/2
         ranges%center_min = -180
         ranges%center_max = +180 - delta
      case ('DE')
         ranges%corner_min = -180
         ranges%corner_max = +180
         ranges%center_min = -180 + delta/2
         ranges%center_max = +180 - delta/2
      case ('GC')
         ranges%corner_min = -delta/2
         ranges%corner_max = 360 - delta/2
         ranges%center_min = 0
         ranges%center_max = 360 - delta
      case ('GE')
         ranges%corner_min = 0
         ranges%corner_max = 360 - delta
         ranges%center_min = delta/2
         ranges%center_max = 360 - delta/2
      case default
         _FAIL("Illegal value for dateline: "//dateline)
      end select

      _RETURN(_SUCCESS)
   end function get_lon_range

   elemental logical module function equal_to(a, b)
      type(LonAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to

   elemental logical module function not_equal_to(a, b)
      type(LonAxis), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   logical module function supports_hconfig(hconfig, rc) result(supports)
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_im_world
      logical :: has_lon_range
      logical :: has_dateline

      supports = .true.

      has_im_world = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
!      has_im_world = MAPL_HConfigKeystringFound(hconfig, keystring='im_world', _RC)
      _RETURN_UNLESS(has_im_world)

      has_lon_range = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
      has_dateline = ESMF_HConfigIsDefined(hconfig, keystring='dateline', _RC)
!      has_lon_range = MAPL_HConfigKeystringFound(hconfig, keystring='lon_range', _RC)
!      has_dateline = MAPL_HConfigKeystringFound(hconfig, keystring='dateline', _RC)
      _RETURN_UNLESS(has_lon_range .neqv. has_dateline)
      supports = .true.

      _RETURN(_SUCCESS)
   end function supports_hconfig
   

   logical module function supports_metadata(file_metadata, rc) result(supports)
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dim_name

      supports = .true.
      dim_name = get_dim_name(file_metadata, units='degrees_east', _RC)

      supports = (dim_name /= '')
      _RETURN(_SUCCESS)
   end function supports_metadata


   module function make_LonAxis_from_metadata(file_metadata, rc) result(axis)
      type(LonAxis) :: axis
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)
      integer :: im_world
      integer :: status
      character(:), allocatable :: dim_name

      dim_name = get_dim_name(file_metadata, units='degrees east', _RC)
      centers = get_coordinates(file_metadata, dim_name, _RC)
      im_world = size(centers)
      ! Enforce convention for longitude range.
      if (any((centers(2:im_world) - centers(1:im_world-1)) < 0)) then
         where(centers > 180) centers = centers - 360
      end if
      corners = get_lon_corners(centers)
      axis = LonAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_LonAxis_from_metadata

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



end submodule LonAxis_smod

