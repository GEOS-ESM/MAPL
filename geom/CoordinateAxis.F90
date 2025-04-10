module mapl3g_CoordinateAxis
   use mapl_RangeMod
   use esmf, only: ESMF_KIND_R8
   use pfio
   implicit none
   private

   public :: CoordinateAxis
   public :: operator(==)
   public :: operator(/=)

   public :: get_coordinates
   public :: get_dim_name
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

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)

   interface get_coordinates
      procedure get_coordinates_dim
   end interface get_coordinates

   ! Submodule
   interface

      pure module function new_CoordinateAxis(centers, corners) result(axis)
         type(CoordinateAxis) :: axis
         real(kind=R8), intent(in) :: centers(:)
         real(kind=R8), intent(in) :: corners(:)
      end function new_CoordinateAxis

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

      module function get_dim_name(file_metadata, units, rc) result(dim_name)
         character(:), allocatable :: dim_name
         type(FileMetadata), target, intent(in) :: file_metadata
         character(*), intent(in) :: units
         integer, optional, intent(out) :: rc
      end function get_dim_name

      module function get_coordinates_dim(file_metadata, dim_name, rc) result(coordinates)
         use pfio, only: FileMetadata
         real(kind=R8), dimension(:), allocatable :: coordinates
         type(FileMetadata), intent(in) :: file_metadata
         character(len=*), intent(in) :: dim_name
         integer, optional, intent(out) :: rc
      end function get_coordinates_dim


   end interface

end module mapl3g_CoordinateAxis

