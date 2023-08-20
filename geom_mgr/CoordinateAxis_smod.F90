#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) CoordinateAxis_smod
   use mapl3g_HConfigUtils
   use mapl_ErrorHandling

contains
   
   pure module function new_CoordinateAxis(centers, corners) result(axis)
      type(CoordinateAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)

      axis%centers = centers
      axis%corners = corners
   end function new_CoordinateAxis


   elemental logical module function equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

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
      type(CoordinateAxis), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

   ! Accessors
   !----------
   ! Note that size(this%corners) might be one larger for non-periodic
   pure module function get_extent(this) result(extent)
      class(CoordinateAxis), intent(in) :: this
      integer :: extent
      extent = size(this%centers)
   end function get_extent

   pure module function get_centers(this) result(centers)
      real(kind=R8), allocatable :: centers(:)
      class(CoordinateAxis), intent(in) :: this

      centers = this%centers
      
   end function get_centers

      
   pure module function get_corners(this) result(corners)
      real(kind=R8), allocatable :: corners(:)
      class(CoordinateAxis), intent(in) :: this

      corners = this%corners
      
   end function get_corners

   pure logical module function is_periodic(this)
      class(CoordinateAxis), intent(in) :: this

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

 

end submodule CoordinateAxis_smod
