#include "MAPL.h"

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
      procedure :: get_num_layers
      procedure :: get_coordinate_field
      procedure :: get_supported_physical_dimensions
      procedure :: get_units
      procedure :: can_connect_to
      procedure :: is_identical_to
      procedure :: write_formatted
      procedure :: matches
   end type MirrorVerticalGrid

   interface MirrorVerticalGrid
      module procedure new_MirrorVerticalGrid
   end interface MirrorVerticalGrid

contains

   function new_MirrorVerticalGrid() result(vertical_grid)
      type(MirrorVerticalGrid) :: vertical_grid
   end function

   function get_num_layers(this) result(num_layers)
      integer :: num_layers
      class(MirrorVerticalGrid), intent(in) :: this
      num_layers = -1
      _UNUSED_DUMMY(this)
   end function
      
   function get_coordinate_field(this, physical_dimension, aspects, coupler, rc) result(field)
      type(ESMF_Field) :: field
      class(MirrorVerticalGrid), intent(in) :: this
      character(*), intent(in) :: physical_dimension
      class(*), intent(in) :: aspects
      class(ComponentDriver), pointer, intent(out) :: coupler
      integer, optional, intent(out) :: rc

      _FAIL('MirrorVerticalGrid should have been replaced before this procedure was called.')
 
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(field)
      _UNUSED_DUMMY(physical_dimension)
      _UNUSED_DUMMY(aspects)
      _UNUSED_DUMMY(coupler)
   end function get_coordinate_field

   function get_supported_physical_dimensions(this) result(dimensions)
      use gftl2_StringVector, only: StringVector
      implicit none
      type(StringVector) :: dimensions
      class(VerticalGrid), target, intent(in) :: this
      
      _FAIL('MirrorVerticalGrid should have been replaced before this procedure was called.')
 
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(dimensions)
   end function get_supported_physical_dimensions

   function get_units(this, physical_dimension, rc) result(units)
      character(len=:), allocatable :: units
      class(VerticalGrid), intent(in) :: this
      character(len=*), intent(in) :: physical_dimension
      integer, optional, intent(out) :: rc

      _FAIL('MirrorVerticalGrid should have been replaced before this procedure was called.')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(physical_dimension)
      _UNUSED_DUMMY(units)
   end function get_units

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

   function matches(this, other) result(matches)
      logical :: matches
      class(VerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: other
   end function matches
end module mapl3g_MirrorVerticalGrid
