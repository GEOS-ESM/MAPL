#include "MAPL_Generic.h"

module mapl3g_FixedLevelsVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_GriddedComponentDriver
   use mapl3g_VerticalDimSpec
   use mapl3g_InfoUtilities, only: MAPL_InfoSetInternal
   use mapl3g_esmf_info_keys, only: KEY_VLOC, KEY_NUM_LEVELS
   use esmf

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
      procedure :: write_formatted
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
      character(*), intent(in) :: standard_name
      real(REAL32), intent(in) :: levels(:)
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

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_dim_spec, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      type(GriddedComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      integer, optional, intent(out) :: rc

      real(kind=REAL32), allocatable :: adjusted_levels(:)
      character(:), allocatable :: vloc
      integer :: status
      type(ESMF_Info) :: info

      if (vertical_dim_spec == VERTICAL_DIM_CENTER) then
         adjusted_levels = this%levels
         vloc = "VERTICAL_DIM_CENTER"
      else if (vertical_dim_spec == VERTICAL_DIM_EDGE) then
         adjusted_levels = [this%levels, this%levels(size(this%levels))]
         vloc = "VERTICAL_DIM_CENTER"
      else
         _FAIL("unsupported vertical_dim_spec")
      end if

      ! Add the 1D array, levels(:), to an ESMF Field
      field = ESMF_FieldEmptyCreate(name="FixedLevelsVerticalGrid", _RC)
      call ESMF_FieldEmptySet(field, geom=geom, _RC)
      call ESMF_FieldEmptyComplete( &
           field, &
           farray=adjusted_levels, &
           indexflag=ESMF_INDEX_DELOCAL, &
           datacopyFlag=ESMF_DATACOPY_VALUE, &
           gridToFieldMap=[0, 0], &
           ungriddedLBound=[1], &
           ungriddedUBound=[size(adjusted_levels)], &
           _RC)
      call MAPL_InfoSetInternal(field, key=KEY_VLOC, value=vloc, _RC)
      call MAPL_InfoSetInternal(field, key=KEY_NUM_LEVELS, value=size(adjusted_levels), _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(coupler)
      _UNUSED_DUMMY(standard_name)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(units)
   end subroutine get_coordinate_field

   logical function can_connect_to(this, src, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: src
      integer, optional, intent(out) :: rc

      can_connect_to = .false.
      _FAIL("not implemented")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src)
   end function can_connect_to

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a, a, 3x, a, a, a, 3x, a, a, a, 3x, a, *(g0, 1x), a, a)", iostat=iostat, iomsg=iomsg) &
           "FixedLevelsVerticalGrid(", new_line("a"), &
           "standard name: ", this%standard_name, new_line("a"), &
           "units: ", this%units, new_line("a"), &
           "levels: ", this %levels, new_line("a"), &
           ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

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
