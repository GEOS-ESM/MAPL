submodule (mapl3g_LatLonAxis) LatLonAxis_smod

contains
   
   pure module function new_LatLonAxis(centers, corners, distribution) result(axis)
      type(LatLonAxis) :: axis
      real(kind=ESMF_KIND_R8), intent(in) :: centers(:)
      real(kind=ESMF_KIND_R8), intent(in) :: corners(:)
      integer, intent(in) :: distribution(:)

      axis%centers = centers
      axis%corners = corners
      axis%distribution = distribution
   end function new_LatLonAxis

   pure module function new_LatLonAxis_serial(centers, corners) result(axis)
      type(LatLonAxis) :: axis
      real(kind=ESMF_KIND_R8), intent(in) :: centers(:)
      real(kind=ESMF_KIND_R8), intent(in) :: corners(:)

      axis = LatLonAxis(centers, corners, distribution=[1])
   end function new_LatLonAxis_serial


   pure logical module function equal_to(a, b)
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

   pure logical module function not_equal_to(a, b)
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

   pure module function get_centers(this, rank) result(centers)
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

   pure module function get_corners(this, rank) result(corners)
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

   pure module function get_npes(this) result(npes)
      class(LatLonAxis), intent(in) :: this
      integer :: npes
      npes = size(this%distribution)
   end function get_npes

   pure module function get_distribution(this) result(distribution)
      class(LatLonAxis), intent(in) :: this
      integer, allocatable :: distribution(:)
      distribution = this%distribution
   end function get_distribution

   pure logical module function is_periodic(this)
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

end submodule LatLonAxis_smod

