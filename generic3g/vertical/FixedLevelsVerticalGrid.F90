#include "MAPL_Generic.h"

module mapl3g_FixedLevelsVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_GriddedComponentDriver
   use esmf, only: ESMF_TypeKind_Flag
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_Geom
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: FixedLevelsVerticalGrid
   public :: operator(==)
   public :: operator(/=)

   type, extends(VerticalGrid) :: FixedLevelsVerticalGrid
      private
      real(kind=REAL32), allocatable :: levels(:)
      character(:), allocatable :: standard_name ! air_pressure, height, etc.
      character(:), allocatable :: units
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to
   end type FixedLevelsVerticalGrid

   interface FixedLevelsVerticalGrid
      procedure new_FixedLevelsVerticalGrid_r32
   end interface FixedLevelsVerticalGrid

   interface operator(==)
      module procedure equal_FixedLevelsVerticalGrid
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_FixedLevelsVerticalGrid
   end interface operator(/=)

contains

   function new_FixedLevelsVerticalGrid_r32(standard_name, levels, units) result(grid)
      type(FixedLevelsVerticalGrid) :: grid
      real(REAL32), intent(in) :: levels(:)
      character(*), intent(in) :: standard_name
      character(*), intent(in) :: units

      call grid%set_id()
      grid%standard_name = standard_name
      grid%levels = levels
      grid%units = units
   end function new_FixedLevelsVerticalGrid_r32

   integer function get_num_levels(this) result(num_levels)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      num_levels = size(this%levels)
   end function get_num_levels

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, rc)
       class(FixedLevelsVerticalGrid), intent(in) :: this
       type(ESMF_Field), intent(out) :: field
       type(GriddedComponentDriver), pointer, intent(out) :: coupler
       character(*), intent(in) :: standard_name
       type(ESMF_Geom), intent(in) :: geom
       type(ESMF_TypeKind_Flag), intent(in) :: typekind
       character(*), intent(in) :: units
       integer, optional, intent(out) :: rc

       _FAIL('not implemented')

       _UNUSED_DUMMY(this)
       _UNUSED_DUMMY(field)
       _UNUSED_DUMMY(coupler)
       _UNUSED_DUMMY(standard_name)
       _UNUSED_DUMMY(geom)
       _UNUSED_DUMMY(typekind)
       _UNUSED_DUMMY(units)
    end subroutine get_coordinate_field

    logical function can_connect_to(this, src, rc)
       class(FixedLevelsVerticalGrid), intent(in) :: this
       class(VerticalGrid), intent(in) :: src
       integer, optional, intent(out) :: rc

       can_connect_to = .false.
       _FAIL('not implemented')
       _UNUSED_DUMMY(this)
       _UNUSED_DUMMY(src)
    end function can_connect_to

    impure elemental logical function equal_FixedLevelsVerticalGrid(a, b) result(equal)
       type(FixedLevelsVerticalGrid), intent(in) :: a, b

       equal = a%standard_name == b%standard_name
       if (.not. equal) return       
       equal = a%units == b%units
       if (.not. equal) return       
       equal = size(a%levels) == size(b%levels)
       if (.not. equal) return       
       equal = all(a%levels == b%levels)
    end function equal_FixedLevelsVerticalGrid 

    impure elemental logical function not_equal_FixedLevelsVerticalGrid(a, b) result(not_equal)
       type(FixedLevelsVerticalGrid), intent(in) :: a, b

       not_equal = .not. (a==b)

    end function not_equal_FixedLevelsVerticalGrid 

end module mapl3g_FixedLevelsVerticalGrid

