module sf_Direction
   implicit none(type,external)
   private

   public :: N_DIR
   public :: EAST, WEST, NORTH, SOUTH
   public :: reverse
   public :: next_direction
   public :: dir_string

   enum, bind(c)
      enumerator :: EAST = 1
      enumerator :: SOUTH
      enumerator :: WEST
      enumerator :: NORTH
   end enum

   integer, parameter :: N_DIR = 4

contains

   integer function reverse(dir)
      integer, intent(in) :: dir
      reverse = 1 + mod(dir-1 + 2, N_DIR)
   end function reverse

   integer function next_direction(dir)
      integer, intent(in) :: dir
      next_direction = 1 + mod(dir-1 + 1, N_DIR)
   end function next_direction

   function dir_string(dir) result(s)
      character(1) :: s
      integer, intent(in) :: dir

      select case (dir)
      case (EAST)
         s = 'E'
      case (SOUTH)
         s = 'S'
      case (WEST)
         s = 'W'
      case (NORTH)
         s = 'N'
      end select
   end function dir_string

end module sf_Direction
