#include "MAPL.h"
module mapl3g_VerticalAlignment
   use mapl3g_VerticalCoordinateDirection
   implicit none
   private

   public :: VerticalAlignment
   public :: VALIGN_WITH_GRID
   public :: VALIGN_UP
   public :: VALIGN_DOWN
   public :: VALIGN_INVALID

   public :: operator(==)
   public :: operator(/=)

   enum, bind(c)
      enumerator :: WITH_GRID=0
      enumerator :: ALIGN_UP=1
      enumerator :: ALIGN_DOWN=2
      enumerator :: ALIGN_INVALID=-1
   end enum

   ! Enum-like type for field vertical alignment
   ! Specifies how a field aligns with its vertical grid
   type :: VerticalAlignment
      private
      integer :: id = WITH_GRID  ! Default to with_grid
      character(32) :: name = "VALIGN_WITH_GRID"
   contains
      procedure :: to_string
      procedure :: resolve
   end type VerticalAlignment

   interface VerticalAlignment
      procedure :: new_VerticalAlignment
   end interface VerticalAlignment

   interface operator(==)
      procedure are_equal
   end interface operator(==)

   interface operator(/=)
      procedure are_not_equal
   end interface operator(/=)

   type(VerticalAlignment), parameter :: VALIGN_WITH_GRID = &
        VerticalAlignment(WITH_GRID, "VALIGN_WITH_GRID")
   type(VerticalAlignment), parameter :: VALIGN_UP = &
        VerticalAlignment(ALIGN_UP, "VALIGN_UP")
   type(VerticalAlignment), parameter :: VALIGN_DOWN = &
        VerticalAlignment(ALIGN_DOWN, "VALIGN_DOWN")
   type(VerticalAlignment), parameter :: VALIGN_INVALID = &
        VerticalAlignment(ALIGN_INVALID, "VALIGN_INVALID")

contains

   ! Constructor that accepts optional string representations
   ! Supports full names and shortcuts (U/D)
   ! Returns with_grid (default) if no argument provided
   function new_VerticalAlignment(str) result(alignment)
      type(VerticalAlignment) :: alignment
      character(*), optional, intent(in) :: str

      if (.not. present(str)) then
         alignment = VALIGN_WITH_GRID
         return
      end if

      select case (trim(str))
      case ('upward', 'UPWARD', 'U', 'u', 'UP', 'up')
         alignment = VALIGN_UP
      case ('downward', 'DOWNWARD', 'D', 'd', 'DOWN', 'down')
         alignment = VALIGN_DOWN
      case ('with_grid', 'WITH_GRID', 'with-grid', 'WITH-GRID')
         alignment = VALIGN_WITH_GRID
      case (VALIGN_UP%name)
         alignment = VALIGN_UP
      case (VALIGN_DOWN%name)
         alignment = VALIGN_DOWN
      case (VALIGN_WITH_GRID%name)
         alignment = VALIGN_WITH_GRID
      case default
         alignment = VALIGN_INVALID
      end select
   end function new_VerticalAlignment

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VerticalAlignment), intent(in) :: this

      select case(this%id)
      case (ALIGN_UP)
         s = "upward"
      case (ALIGN_DOWN)
         s = "downward"
      case (WITH_GRID)
         s = "with_grid"
      case default
         s = "invalid"
      end select
   end function to_string

   ! Resolve alignment to actual coordinate direction
   ! If alignment is WITH_GRID, return the grid's coordinate direction
   ! Otherwise return the alignment as a coordinate direction
   function resolve(this, grid_direction) result(direction)
      type(VerticalCoordinateDirection) :: direction
      class(VerticalAlignment), intent(in) :: this
      type(VerticalCoordinateDirection), intent(in) :: grid_direction

      select case(this%id)
      case (WITH_GRID)
         direction = grid_direction
      case (ALIGN_UP)
         direction = VCOORD_DIRECTION_UP
      case (ALIGN_DOWN)
         direction = VCOORD_DIRECTION_DOWN
      case default
         direction = VCOORD_DIRECTION_INVALID
      end select
   end function resolve

   elemental logical function are_equal(this, that)
      type(VerticalAlignment), intent(in) :: this
      type(VerticalAlignment), intent(in) :: that

      are_equal = (this%id == that%id)
   end function are_equal

   elemental logical function are_not_equal(this, that)
      type(VerticalAlignment), intent(in) :: this
      type(VerticalAlignment), intent(in) :: that
      
      are_not_equal = .not. (this == that)
   end function are_not_equal

end module mapl3g_VerticalAlignment
