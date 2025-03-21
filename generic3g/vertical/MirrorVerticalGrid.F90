#include "MAPL_Generic.h"

! MirrorVerticalGrid objects should always have been replaced with an
! object of a different subclass by the timet they are used.  As such,
! it should only be used with import stateIntent, and will be replaced
! by whatever source grid is connected to it.

module mapl3g_MirrorVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_ComponentDriver
   use mapl3g_VerticalStaggerLoc
   use esmf, only: ESMF_TypeKind_Flag
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_Geom

   implicit none
   private

   public :: MirrorVerticalGrid

   type, extends(VerticalGrid) :: MirrorVerticalGrid
      private
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to
      procedure :: is_identical_to
      procedure :: write_formatted
   end type MirrorVerticalGrid

   interface MirrorVerticalGrid
      module procedure new_MirrorVerticalGrid
   end interface MirrorVerticalGrid

contains

   function new_MirrorVerticalGrid() result(vertical_grid)
      type(MirrorVerticalGrid) :: vertical_grid
   end function

   function get_num_levels(this) result(num_levels)
      integer :: num_levels
      class(MirrorVerticalGrid), intent(in) :: this
      num_levels = -1
      _UNUSED_DUMMY(this)
   end function
      
   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_stagger, rc)
      class(MirrorVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      class(ComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      integer, optional, intent(out) :: rc

      _FAIL('MirrorVerticalGrid should have been replaced before this procedure was called.')
 
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
      class(MirrorVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: dst
      integer, optional, intent(out) :: rc

      can_connect_to = .false.
      _RETURN(_SUCCESS)
      
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(dst)
   end function can_connect_to

   logical function is_identical_to(this, that, rc)
      class(MirrorVerticalGrid), intent(in) :: this
      class(VerticalGrid), allocatable, intent(in) :: that
      integer, optional, intent(out) :: rc

      is_identical_to = .false.

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(that)
   end function is_identical_to

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(MirrorVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "MirrorVerticalGrid()"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

end module mapl3g_MirrorVerticalGrid
