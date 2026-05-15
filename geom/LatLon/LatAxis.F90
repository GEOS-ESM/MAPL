module mapl3g_LatAxis
   use mapl3g_CoordinateAxis
   use pfio
   use esmf
   implicit none
   private

   ! Constructor
   public :: LatAxis
   public :: operator(==)
   public :: make_LatAxis

   ! Helper procedure
   public :: get_lat_range
   

   type, extends(CoordinateAxis) :: LatAxis
      private
   contains
      procedure, nopass :: supports_hconfig
      procedure, nopass :: supports_metadata
      generic :: supports => supports_hconfig, supports_metadata
   end type LatAxis

   interface LatAxis
      procedure :: new_LatAxis
   end interface LatAxis

   interface make_LatAxis
      procedure make_LatAxis_from_hconfig
      procedure make_LatAxis_from_metadata
   end interface make_LatAxis
   
   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)

   integer, parameter :: R8 = ESMF_KIND_R8

   interface

      logical module function supports_hconfig(hconfig, rc) result(supports)
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function supports_hconfig

      logical module function supports_metadata(file_metadata, rc) result(supports)
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata

    ! static factory methods
      module function make_LatAxis_from_hconfig(hconfig, rc) result(axis)
         type(LatAxis) :: axis
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_LatAxis_from_hconfig

      module function make_LatAxis_from_metadata(file_metadata, rc) result(axis)
         type(LatAxis) :: axis
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_LatAxis_from_metadata

      ! helper functions
      module function get_lat_range(hconfig, jm_world, rc) result(ranges)
         use esmf, only: ESMF_HConfig
         type(AxisRanges) :: ranges
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, intent(in) :: jm_world
         integer, optional, intent(out) :: rc
      end function get_lat_range

      module function get_lat_corners(centers) result(corners)
         real(kind=R8), intent(in) :: centers(:)
         real(kind=R8), allocatable :: corners(:)
      end function get_lat_corners


      module subroutine fix_bad_pole(centers)
         real(kind=R8), intent(inout) :: centers(:)
      end subroutine fix_bad_pole

   end interface

   CONTAINS

   ! Constructor
   pure function new_LatAxis(centers, corners) result(axis)
      type(LatAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LatAxis

   elemental logical function equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to
   
end module mapl3g_LatAxis
