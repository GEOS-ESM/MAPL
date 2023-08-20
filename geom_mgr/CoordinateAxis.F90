module mapl3g_CoordinateAxis
   use mapl_RangeMod
   use esmf, only: ESMF_KIND_R8
   use esmf, only: ESMF_HConfig
   implicit none
   private

   public :: CoordinateAxis
   public :: operator(==)
   public :: operator(/=)

   ! Public just to enable testing
   public :: AxisRanges

   integer, parameter :: R8 = ESMF_KIND_R8

   type :: AxisRanges
      real(kind=R8) :: center_min
      real(kind=R8) :: center_max
      real(kind=R8) :: corner_min
      real(kind=R8) :: corner_max
   end type AxisRanges

   type :: CoordinateAxis
      private
      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)
   contains
      procedure :: get_extent
      procedure :: get_centers
      procedure :: get_corners
      procedure :: is_periodic
   end type CoordinateAxis

   interface CoordinateAxis
      procedure new_CoordinateAxis
   end interface CoordinateAxis

   interface make_LatAxis
      procedure make_LatAxis_from_hconfig
   end interface make_LatAxis

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)


   ! Submodule
   interface

      pure module function new_CoordinateAxis(centers, corners) result(axis)
         type(CoordinateAxis) :: axis
         real(kind=R8), intent(in) :: centers(:)
         real(kind=R8), intent(in) :: corners(:)
      end function new_CoordinateAxis

      module function make_LatAxis_from_hconfig(hconfig, rc) result(axis)
         type(CoordinateAxis) :: axis
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_LatAxis_from_hconfig

      elemental logical module function equal_to(a, b)
         type(CoordinateAxis), intent(in) :: a, b
      end function equal_to

      elemental logical module function not_equal_to(a, b)
         type(CoordinateAxis), intent(in) :: a, b
      end function not_equal_to

      ! Accessors
      !----------
      ! Note that size(this%corners) might be one larger for non-periodic
      pure module function get_extent(this) result(extent)
         class(CoordinateAxis), intent(in) :: this
         integer :: extent
      end function get_extent

      pure module function get_centers(this) result(centers)
         real(kind=R8), allocatable :: centers(:)
         class(CoordinateAxis), intent(in) :: this
      end function get_centers

      pure module function get_corners(this) result(corners)
         real(kind=R8), allocatable :: corners(:)
         class(CoordinateAxis), intent(in) :: this
      end function get_corners

      pure logical module function is_periodic(this)
         class(CoordinateAxis), intent(in) :: this
      end function is_periodic

   end interface

end module mapl3g_CoordinateAxis

