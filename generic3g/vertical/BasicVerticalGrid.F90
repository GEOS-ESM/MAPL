#include "MAPL_Generic.h"

module mapl3g_BasicVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_MirrorVerticalGrid
   use mapl3g_ComponentDriver
   use mapl3g_VerticalStaggerLoc
   use esmf, only: ESMF_TypeKind_Flag
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_Geom

   implicit none
   private

   public :: BasicVerticalGrid

   type, extends(VerticalGrid) :: BasicVerticalGrid
      private
      integer :: num_levels = 0
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to
      procedure :: is_identical_to
      procedure :: write_formatted
   end type BasicVerticalGrid

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

   interface BasicVerticalGrid
      module procedure new_BasicVerticalGrid
   end interface BasicVerticalGrid

contains

   function new_BasicVerticalGrid(num_levels) result(vertical_grid)
      type(BasicVerticalGrid) :: vertical_grid
      integer, intent(in) :: num_levels
      call vertical_grid%set_id()
      vertical_grid%num_levels = num_levels
   end function

   function get_num_levels(this) result(num_levels)
      integer :: num_levels
      class(BasicVerticalGrid), intent(in) :: this
      num_levels = this%num_levels
   end function

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_stagger, rc)
      class(BasicVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      class(ComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      integer, optional, intent(out) :: rc

      _FAIL('BasicVerticalGrid should have been connected to a different subclass before this is called.')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(field)
      _UNUSED_DUMMY(coupler)
      _UNUSED_DUMMY(standard_name)
      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(units)
      _UNUSED_DUMMY(vertical_stagger)
   end subroutine get_coordinate_field

   logical function can_connect_to(this, dst, rc)
      class(BasicVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: dst
      integer, optional, intent(out) :: rc

      if (this%same_id(dst)) then
         can_connect_to = .true.
         _RETURN(_SUCCESS)
      end if

      _FAIL("BasicVerticalGrid::can_connect_to - NOT implemented yet")
   end function can_connect_to

   logical function is_identical_to(this, that, rc)
      class(BasicVerticalGrid), intent(in) :: this
      class(VerticalGrid), allocatable, intent(in) :: that
      integer, optional, intent(out) :: rc

      is_identical_to = .false.

      ! Mirror grid
      if (.not. allocated(that)) then
         is_identical_to = .true.
         _RETURN(_SUCCESS) ! mirror grid
      end if

      ! Same id
      is_identical_to = this%same_id(that)
      if (is_identical_to) then
         _RETURN(_SUCCESS)
      end if

      select type(that)
      type is(BasicVerticalGrid)
         is_identical_to = (this == that)
      end select

      _RETURN(_SUCCESS)
   end function is_identical_to

   elemental logical function equal_to(a, b)
      type(BasicVerticalGrid), intent(in) :: a, b
      equal_to = a%num_levels == b%num_levels
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(BasicVerticalGrid), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(BasicVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a, a, g0, a)", iostat=iostat, iomsg=iomsg) &
           "BasicVerticalGrid(", &
           "num levels: ", this%num_levels, &
           ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

end module mapl3g_BasicVerticalGrid
