#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonAxis) LatLonAxis_smod
   use mapl3g_HConfigUtils
   use mapl_ErrorHandling

contains
   
   pure module function new_LatLonAxis(centers, corners) result(axis)
      type(LatLonAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)

      axis%centers = centers
      axis%corners = corners
   end function new_LatLonAxis


   ! static factory methods
   module function make_LatAxis_from_hconfig(hconfig, rc) result(axis)
      type(LatLonAxis) :: axis
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: jm_world
      real(kind=R8), allocatable :: centers(:), corners(:)
      type(AxisRanges) :: ranges

      call MAPL_GetResource(jm_world, hconfig, 'jm_world', _RC)
      _ASSERT(jm_world > 0, 'jm_world must be greater than 1')

      ranges = get_lat_range(hconfig, jm_world, _RC)
      centers = MAPL_Range(ranges%center_min, ranges%center_max, jm_world, _RC)

      corners = MAPL_Range(ranges%corner_min, ranges%corner_max, jm_world+1, _RC)
      ! IMPORTANT: this fix must be _after the call to MAPL_Range.
      if (corners(1) < -90.d0) corners(1) = -90.0d0
      if (corners(jm_world+1) > 90.d0) corners(jm_world+1) = 90.0d0

      axis = LatLonAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_LatAxis_from_hconfig


   elemental logical module function equal_to(a, b)
      type(LatLonAxis), intent(in) :: a, b

      ! Do the fast checks first
      equal_to = size(a%centers) == size(b%centers)
      if (.not. equal_to) return
      equal_to = size(a%corners) == size(b%corners)
      if (.not. equal_to) return

      equal_to = all(a%centers == b%centers)
      if (.not. equal_to) return
      equal_to = all(a%corners == b%corners)
   end function equal_to

   elemental logical module function not_equal_to(a, b)
      type(LatLonAxis), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

   ! Accessors
   !----------
   ! Note that size(this%corners) might be one larger for non-periodic
   pure module function get_extent(this) result(extent)
      class(LatLonAxis), intent(in) :: this
      integer :: extent
      extent = size(this%centers)
   end function get_extent

   pure module function get_centers(this) result(centers)
      real(kind=R8), allocatable :: centers(:)
      class(LatLonAxis), intent(in) :: this

      centers = this%centers
      
   end function get_centers

      
   pure module function get_corners(this) result(corners)
      real(kind=R8), allocatable :: corners(:)
      class(LatLonAxis), intent(in) :: this

      corners = this%corners
      
   end function get_corners

   pure logical module function is_periodic(this)
      class(LatLonAxis), intent(in) :: this

      integer :: i
      real(kind=R8) :: span, spacing
      real(kind=R8), parameter :: tolerance = 0.01

      
      associate (corners => this%corners)
        associate (n => size(corners))

          if (n == 1) then
             is_periodic = .false.
             return
          end if
        
          span = corners(n) - corners(1)
          spacing = corners(2) - corners(1)

          if (abs(span - 360) < (tolerance * spacing)) then
             is_periodic = .true.
          else
             is_periodic = .false.
          end if

        end associate
      end associate
      
   end function is_periodic

   module function make_LonAxis_from_hconfig(hconfig, rc) result(axis)
      type(LatLonAxis) :: axis
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im_world
      real(kind=R8), allocatable :: centers(:), corners(:)
      type(AxisRanges) :: ranges

      call MAPL_GetResource(im_world, hconfig, 'im_world', _RC)
      _ASSERT(im_world > 0, 'im_world must be greater than 0')

      ranges = get_lon_range(hconfig, im_world, _RC)
      centers = MAPL_Range(ranges%center_min, ranges%center_max, im_world, _RC)
      corners = MAPL_Range(ranges%corner_min, ranges%corner_max, im_world+1, _RC)

      axis = LatLonAxis(centers, corners)

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
         call MAPL_GetResource(t_range, hconfig, 'lon_range', _RC)
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
      call MAPL_GetResource(dateline, hconfig, 'dateline', _RC)
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

   module function get_lat_range(hconfig, jm_world, rc) result(ranges)
      type(AxisRanges) :: ranges
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: jm_world
      integer, optional, intent(out) :: rc
      
      integer :: status
      real(kind=R8) :: delta
      character(:), allocatable :: pole
      real, allocatable :: t_range(:)
      logical :: has_range
      logical :: has_pole

      has_range = ESMF_HConfigIsDefined(hconfig, keystring='lat_range', _RC)
      has_pole = ESMF_HConfigIsDefined(hconfig, keystring='pole', _RC)
      _ASSERT(has_range .neqv. has_pole, 'Exactly one of lon_range or pole must be defined in hconfig')
      
      if (has_range) then ! is_regional
         call MAPL_GetResource(t_range, hconfig, 'lat_range', _RC)
         _ASSERT(size(t_range) == 2, 'illegal size of lon_range')
         _ASSERT(range(1) < range(2), 'illegal lat_range')
         delta = (range(2) - range(1)) / jm_world
         ! t_range is corners; need centers
         ranges%center_min = t_range(1) + delta/2
         ranges%center_max = t_range(2) - delta/2
         ranges%corner_min = t_range(1)
         ranges%corner_max = t_range(2)
         _RETURN(_SUCCESS)
      end if
         
      call MAPL_GetResource(pole, hconfig, 'pole', _RC)
      select case (pole)
      case ('PE')
         delta = 180.d0 / jm_world
         ranges%center_min = -90 + delta/2
         ranges%center_max = +90 - delta/2
         ranges%corner_min = -90
         ranges%corner_max = +90
      case ('PC')
         delta = 180.d0 / (jm_world-1)
         ranges%center_min = -90
         ranges%center_max = +90
         ranges%corner_min = -90 - delta/2
         ranges%corner_max = +90 + delta/2
      case default
         _FAIL("Illegal value for pole: "//pole)
      end select

      _RETURN(_SUCCESS)
   end function get_lat_range
 

end submodule LatLonAxis_smod
