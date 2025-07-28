#include "MAPL.h"

module mapl3g_FixedLevelsVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_MirrorVerticalGrid
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldCreate
   use mapl3g_ComponentDriver
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use esmf

   implicit none
   private

   public :: FixedLevelsVerticalGrid
   public :: operator(==)
   public :: operator(/=)

   type, extends(VerticalGrid) :: FixedLevelsVerticalGrid
      private
      real(kind=ESMF_KIND_R4), allocatable :: levels(:)
      character(:), allocatable :: standard_name ! air_pressure, height, etc.
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to
      procedure :: is_identical_to
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

   function new_FixedLevelsVerticalGrid_r32(standard_name, levels, units) result(vgrid)
      type(FixedLevelsVerticalGrid) :: vgrid
      character(*), intent(in) :: standard_name
      real(kind=ESMF_KIND_R4), intent(in) :: levels(:)
      character(*), intent(in) :: units

      call vgrid%set_id()
      vgrid%standard_name = standard_name
      vgrid%levels = levels
      call vgrid%set_units(units)
   end function new_FixedLevelsVerticalGrid_r32

   integer function get_num_levels(this) result(num_levels)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      num_levels = size(this%levels)
   end function get_num_levels

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_stagger, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      class(ComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R4), pointer :: farray3d(:, :, :)
      integer :: shape_(3), horz, ungrd, status

      field = MAPL_FieldCreate( &
           geom=geom, &
           typekind=ESMF_TYPEKIND_R4, &
           num_levels=size(this%levels), &
           vert_staggerloc=VERTICAL_STAGGER_CENTER, &
           _RC)
      ! Copy the 1D array, levels(:), to each point of the horz grid
      call assign_fptr_condensed_array(field, farray3d, _RC)
      shape_ = shape(farray3d)
      do concurrent (horz=1:shape_(1), ungrd=1:shape_(3))
         farray3d(horz, :, ungrd) = this%levels(:)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(coupler)
      _UNUSED_DUMMY(standard_name)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(units)
      _UNUSED_DUMMY(vertical_stagger)
   end subroutine get_coordinate_field

   logical function can_connect_to(this, dst, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: dst
      integer, optional, intent(out) :: rc

      if (this%same_id(dst)) then
         can_connect_to = .true.
         _RETURN(_SUCCESS)
      end if

      select type(dst)
      type is (FixedLevelsVerticalGrid)
         can_connect_to = .true.
      type is (MirrorVerticalGrid)
         can_connect_to = .true.
      class default
         _FAIL("FixedLevelsVerticalGrid can only connect to FixedLevelsVerticalGrid, or MirrorVerticalGrid")
      end select

      _RETURN(_SUCCESS)
   end function can_connect_to

   logical function is_identical_to(this, that, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
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
      type is(FixedLevelsVerticalGrid)
         is_identical_to = (this == that)
      end select

      _RETURN(_SUCCESS)
   end function is_identical_to

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a, a, 3x, a, a, a, 3x, a, a, a, 3x, a, *(g0, 1x))", iostat=iostat, iomsg=iomsg) &
           "FixedLevelsVerticalGrid(", new_line("a"), &
           "standard name: ", this%standard_name, new_line("a"), &
           "units: ", this%get_units(), new_line("a"), &
           "levels: ", this %levels
      write(unit, "(a)", iostat=iostat, iomsg=iomsg) ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   impure elemental logical function equal_FixedLevelsVerticalGrid(a, b) result(equal)
      type(FixedLevelsVerticalGrid), intent(in) :: a, b

      equal = a%standard_name == b%standard_name
      if (.not. equal) return
      equal = a%get_units() == b%get_units()
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
