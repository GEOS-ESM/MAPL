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

contains

   pure function new_LatLonAxis(centers, corners, distribution) result(axis)
      type(LatLonAxis) :: axis
      real(kind=ESMF_KIND_R8), intent(in) :: centers(:)
      real(kind=ESMF_KIND_R8), intent(in) :: corners(:)
      integer, intent(in) :: distribution(:)

      axis%centers = centers
      axis%corners = corners
      axis%distribution = distribution
   end function new_LatLonAxis

   pure function new_LatLonAxis_serial(centers, corners) result(axis)
      type(LatLonAxis) :: axis
      real(kind=ESMF_KIND_R8), intent(in) :: centers(:)
      real(kind=ESMF_KIND_R8), intent(in) :: corners(:)

      axis = LatLonAxis(centers, corners, distribution=[1])
   end function new_LatLonAxis_serial


   pure logical function equal_to(a, b)
      type(LatLonAxis), intent(in) :: a, b

      ! Do the fast checks first
      equal_to = size(a%centers) == size(b%centers)
      if (.not. equal_to) return
      equal_to = size(a%corners) == size(b%corners)
      if (.not. equal_to) return
      equal_to = size(a%distribution) == size(b%distribution)
      if (.not. equal_to) return

      equal_to = all(a%centers == b%centers)
      if (.not. equal_to) return
      equal_to = all(a%corners == b%corners)
      if (.not. equal_to) return
      equal_to = all(a%distribution == b%distribution)
      
   end function equal_to

   pure logical function not_equal_to(a, b)
      type(LatLonAxis), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

   ! Accessors
   !----------
   ! Note that size(this%corners) might be one larger for non-periodic
   pure function get_extent(this) result(extent)
      class(LatLonAxis), intent(in) :: this
      integer :: extent
      extent = size(this%centers)
   end function get_extent

   pure function get_centers(this, rank) result(centers)
      real(kind=ESMF_KIND_R8), allocatable :: centers(:)
      class(LatLonAxis), intent(in) :: this
      integer, intent(in), optional :: rank ! starting from 0

      if (present(rank)) then
         associate (d => this%distribution)
           associate(i0 => 1 + sum(d(1:rank)), i1 => sum(d(1:rank+1)))
             centers = this%centers(i0:i1)
           end associate
         end associate
       else
          centers = this%centers
       end if
      
   end function get_centers

   pure function get_corners(this, rank) result(corners)
      real(kind=ESMF_KIND_R8), allocatable :: corners(:)
      class(LatLonAxis), intent(in) :: this
      integer, intent(in), optional :: rank ! starting from 0

      integer :: i0, i1

      if (present(rank)) then
         associate (d => this%distribution)
           i0 = 1 + sum(d(1:rank))
           i1 = sum(d(1:rank+1))
           if (rank == size(d)-1) then ! last rank get the extra corner
              i1 = i1 + 1
           end if
           corners = this%corners(i0:i1)
         end associate
      else
         corners = this%corners
      end if
      
   end function get_corners

   pure function get_npes(this) result(npes)
      class(LatLonAxis), intent(in) :: this
      integer :: npes
      npes = size(this%distribution)
   end function get_npes

   pure function get_distribution(this) result(distribution)
      class(LatLonAxis), intent(in) :: this
      integer, allocatable :: distribution(:)
      distribution = this%distribution
   end function get_distribution

   pure logical function is_periodic(this)
      class(LatLonAxis), intent(in) :: this

      integer :: i
      real(kind=ESMF_KIND_R8) :: span, spacing
      real(kind=ESMF_KIND_R8), parameter :: tolerance = 0.01

      associate (corners => this%corners)
        associate (n => size(corners))
        
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
end module mapl3g_LatLonAxis
