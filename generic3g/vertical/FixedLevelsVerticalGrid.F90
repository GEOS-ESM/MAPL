module mapl3g_FixedLevelsVerticalGrid
   use mapl3g_VerticalGrid
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: FixedLevelsVerticalGrid

   type, extends(VerticalGrid) :: FixedLevelsVerticalGrid
      private
      real, allocatable :: levels(:)
      character(:), allocatable :: standard_name ! air_pressure, height, etc.
!#      character(:), allocatable :: units
!#      character(:), allocatable :: coordinate_name
   contains
      procedure :: get_num_levels
   end type FixedLevelsVerticalGrid

   interface FixedLevelsVerticalGrid
      procedure new_FixedLevelsVerticalGrid_r32
   end interface FixedLevelsVerticalGrid

contains

   function new_FixedLevelsVerticalGrid_r32(standard_name, levels) result(grid)
      type(FixedLevelsVerticalGrid) :: grid
      real(REAL32), intent(in) :: levels(:)
      character(*), intent(in) :: standard_name

      grid%standard_name = standard_name
      grid%levels = levels

   end function new_FixedLevelsVerticalGrid_r32

   integer function get_num_levels(this) result(num_levels)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      num_levels = size(this%levels)
   end function get_num_levels

end module mapl3g_FixedLevelsVerticalGrid
   
