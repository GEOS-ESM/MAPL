#include "MAPL.h"
module mapl3g_VerticalCoordinateDirection
   implicit none
   private

   public :: VerticalCoordinateDirection
   public :: VCOORD_DIRECTION_UP
   public :: VCOORD_DIRECTION_DOWN
   public :: VCOORD_DIRECTION_UNSPECIFIED
   public :: VCOORD_DIRECTION_INVALID

   public :: operator(==)
   public :: operator(/=)

   enum, bind(c)
      enumerator :: UP=1
      enumerator :: DOWN=2
      enumerator :: UNSPECIFIED=0
      enumerator :: INVALID=-1
   end enum

   ! Enum-like type for vertical coordinate direction
   ! Supports upward, downward, and unspecified directions
   type :: VerticalCoordinateDirection
      private
      integer :: id = DOWN  ! Default to downward (GEOS convention)
      character(32) :: name = "VCOORD_DIRECTION_DOWN"
   contains
      procedure :: to_string
   end type VerticalCoordinateDirection

   interface VerticalCoordinateDirection
      procedure :: new_VerticalCoordinateDirection
   end interface VerticalCoordinateDirection

   interface operator(==)
      procedure are_equal
   end interface operator(==)

   interface operator(/=)
      procedure are_not_equal
   end interface operator(/=)

   type(VerticalCoordinateDirection), parameter :: VCOORD_DIRECTION_UP = &
        VerticalCoordinateDirection(UP, "VCOORD_DIRECTION_UP")
   type(VerticalCoordinateDirection), parameter :: VCOORD_DIRECTION_DOWN = &
        VerticalCoordinateDirection(DOWN, "VCOORD_DIRECTION_DOWN")
   type(VerticalCoordinateDirection), parameter :: VCOORD_DIRECTION_UNSPECIFIED = &
        VerticalCoordinateDirection(UNSPECIFIED, "VCOORD_DIRECTION_UNSPECIFIED")
   type(VerticalCoordinateDirection), parameter :: VCOORD_DIRECTION_INVALID = &
        VerticalCoordinateDirection(INVALID, "VCOORD_DIRECTION_INVALID")

contains

   ! Constructor that accepts string representations
   ! Supports full names and shortcuts (U/D)
   function new_VerticalCoordinateDirection(str) result(direction)
      type(VerticalCoordinateDirection) :: direction
      character(*), intent(in) :: str

      select case (trim(str))
      case ('upward', 'UPWARD', 'U', 'u', 'UP', 'up')
         direction = VCOORD_DIRECTION_UP
      case ('downward', 'DOWNWARD', 'D', 'd', 'DOWN', 'down')
         direction = VCOORD_DIRECTION_DOWN
      case ('unspecified', 'UNSPECIFIED')
         direction = VCOORD_DIRECTION_UNSPECIFIED
      case (VCOORD_DIRECTION_UP%name)
         direction = VCOORD_DIRECTION_UP
      case (VCOORD_DIRECTION_DOWN%name)
         direction = VCOORD_DIRECTION_DOWN
      case (VCOORD_DIRECTION_UNSPECIFIED%name)
         direction = VCOORD_DIRECTION_UNSPECIFIED
      case default
         direction = VCOORD_DIRECTION_INVALID
      end select
   end function new_VerticalCoordinateDirection

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VerticalCoordinateDirection), intent(in) :: this

      select case(this%id)
      case (UP)
         s = "upward"
      case (DOWN)
         s = "downward"
      case (UNSPECIFIED)
         s = "unspecified"
      case default
         s = "invalid"
      end select
   end function to_string

   elemental logical function are_equal(this, that)
      type(VerticalCoordinateDirection), intent(in) :: this
      type(VerticalCoordinateDirection), intent(in) :: that

      are_equal = (this%id == that%id)
   end function are_equal

   elemental logical function are_not_equal(this, that)
      type(VerticalCoordinateDirection), intent(in) :: this
      type(VerticalCoordinateDirection), intent(in) :: that
      
      are_not_equal = .not. (this == that)
   end function are_not_equal

end module mapl3g_VerticalCoordinateDirection
