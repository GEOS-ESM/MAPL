module mapl_CoordinateAxis_mod
   use mapl_Range_mod
   use esmf, only: ESMF_KIND_R8
   use pfio
   implicit none(type,external)
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
      ! Coordinate-comparison tolerance for equal_to(), expressed as a
      ! FRACTION of this axis's own local grid spacing (DX) - not an
      ! absolute coordinate difference. Default of 0 preserves exact
      ! (bitwise) comparison; a nonzero value (sourced from an external
      ! "coordinate_tolerance" attribute, e.g. by ExtData) allows
      ! near-identical coordinates to compare equal, scaled by
      ! resolution: e.g. 0.01 means "within 1% of the minimum spacing
      ! between adjacent centers".
      !
      ! IMPORTANT (directional, not symmetric): equal_to(a, b) is called
      ! by GeomManager as `already_registered == candidate` (see gFTL's
      ! find(): it compares each existing container element, "a", against
      ! the new lookup value, "b"). Only the CANDIDATE's ("b"'s) own
      ! tolerance and own spacing are used - each grid decides for
      ! itself, using its own resolution, whether an existing registry
      ! entry is close enough to reuse; the already-registered entry's
      ! tolerance is irrelevant to that decision (it already made its own
      ! decision when it was inserted). Consequently `a == b` and
      ! `b == a` are generally NOT equivalent when the two sides declare
      ! different tolerances.
      real(kind=R8) :: tolerance = 0.0_R8
   contains
      procedure :: get_extent
      procedure :: get_centers
      procedure :: get_corners
      procedure :: get_tolerance
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

      pure module function new_CoordinateAxis(centers, corners, tolerance) result(axis)
         type(CoordinateAxis) :: axis
         real(kind=R8), intent(in) :: centers(:)
         real(kind=R8), intent(in) :: corners(:)
         real(kind=R8), optional, intent(in) :: tolerance
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

      pure module function get_tolerance(this) result(tolerance)
         real(kind=R8) :: tolerance
         class(CoordinateAxis), intent(in) :: this
      end function get_tolerance

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

end module mapl_CoordinateAxis_mod

