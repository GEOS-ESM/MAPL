module mapl3g_LonAxis
   use mapl3g_CoordinateAxis
   use pfio
   use esmf
   implicit none
   private

   ! Constructor
   public :: LonAxis
   public :: operator(==)
   public :: make_LonAxis

   ! Helper procedure
   public :: get_lon_range


   type, extends(CoordinateAxis) :: LonAxis
      private
   contains
      procedure, nopass :: supports_hconfig
      procedure, nopass :: supports_metadata
      generic :: supports => supports_hconfig, supports_metadata
   end type LonAxis

   interface LonAxis
      procedure new_LonAxis
   end interface LonAxis

   interface make_LonAxis
      procedure make_LonAxis_from_hconfig
      procedure make_LonAxis_from_metadata
   end interface make_LonAxis

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)

   integer, parameter :: R8 = ESMF_KIND_R8

   interface

      module logical function supports_hconfig(hconfig, rc) result(supports)
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function supports_hconfig

      module logical function supports_metadata(file_metadata, rc) result(supports)
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata

      ! static factory methods
      module function make_LonAxis_from_hconfig(hconfig, rc) result(axis)
         type(LonAxis) :: axis
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_LonAxis_from_hconfig

      module function make_LonAxis_from_metadata(file_metadata, rc) result(axis)
         type(LonAxis) :: axis
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_LonAxis_from_metadata


      module function get_lon_corners(centers) result(corners)
         real(kind=R8), intent(in) :: centers(:)
         real(kind=R8), allocatable :: corners(:)
      end function get_lon_corners


      ! helper functions
      module function get_lon_range(hconfig, im_world, rc) result(ranges)
         use esmf, only: ESMF_HConfig
         type(AxisRanges) :: ranges
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, intent(in) :: im_world
         integer, optional, intent(out) :: rc
      end function get_lon_range

   end interface

   CONTAINS

   ! Constructor
   pure function new_LonAxis(centers, corners) result(axis)
      type(LonAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LonAxis

   elemental logical function equal_to(a, b)
      type(LonAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(LonAxis), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

end module mapl3g_LonAxis

