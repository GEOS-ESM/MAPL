module mapl3g_LatLonAxis
   use esmf, only: ESMF_KIND_R8
   implicit none
   private

   public :: LatLonAxis
   public :: operator(==)
   public :: operator(/=)

   type :: LatLonAxis
      private
      real(kind=ESMF_KIND_R8), allocatable :: centers(:)
      real(kind=ESMF_KIND_R8), allocatable :: corners(:)
      integer, allocatable :: distribution(:)
   contains
      procedure :: get_extent
      procedure :: get_centers
      procedure :: get_corners
      procedure :: get_npes
      procedure :: get_distribution
      procedure :: is_periodic
   end type LatLonAxis

   interface LatLonAxis
      procedure new_LatLonAxis
      procedure new_LatLonAxis_serial
   end interface LatLonAxis

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)


   ! Submodule
   interface

      pure module function new_LatLonAxis(centers, corners, distribution) result(axis)
         type(LatLonAxis) :: axis
         real(kind=ESMF_KIND_R8), intent(in) :: centers(:)
         real(kind=ESMF_KIND_R8), intent(in) :: corners(:)
         integer, intent(in) :: distribution(:)
      end function new_LatLonAxis

      pure module function new_LatLonAxis_serial(centers, corners) result(axis)
         type(LatLonAxis) :: axis
         real(kind=ESMF_KIND_R8), intent(in) :: centers(:)
         real(kind=ESMF_KIND_R8), intent(in) :: corners(:)
      end function new_LatLonAxis_serial

      pure logical module function equal_to(a, b)
         type(LatLonAxis), intent(in) :: a, b
      end function equal_to

      pure logical module function not_equal_to(a, b)
         type(LatLonAxis), intent(in) :: a, b
      end function not_equal_to

      ! Accessors
      !----------
      ! Note that size(this%corners) might be one larger for non-periodic
      pure module function get_extent(this) result(extent)
         class(LatLonAxis), intent(in) :: this
         integer :: extent
      end function get_extent

      pure module function get_centers(this, rank) result(centers)
         use esmf, only: ESMF_KIND_R8
         real(kind=ESMF_KIND_R8), allocatable :: centers(:)
         class(LatLonAxis), intent(in) :: this
         integer, intent(in), optional :: rank ! starting from 0
      end function get_centers

      pure module function get_corners(this, rank) result(corners)
         use esmf, only: ESMF_KIND_R8
         real(kind=ESMF_KIND_R8), allocatable :: corners(:)
         class(LatLonAxis), intent(in) :: this
         integer, intent(in), optional :: rank ! starting from 0
      end function get_corners

      pure module function get_npes(this) result(npes)
         class(LatLonAxis), intent(in) :: this
         integer :: npes
      end function get_npes

      pure module function get_distribution(this) result(distribution)
         class(LatLonAxis), intent(in) :: this
         integer, allocatable :: distribution(:)
      end function get_distribution

      pure logical module function is_periodic(this)
         class(LatLonAxis), intent(in) :: this
      end function is_periodic

   end interface

end module mapl3g_LatLonAxis

