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
   use mapl_ErrorHandling
   use gftl2_StringVector, only: StringVector
   use esmf, only: ESMF_TypeKind_Flag
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_Geom

   implicit none
   private

   public :: MirrorVerticalGrid

   type, extends(VerticalGrid) :: MirrorVerticalGrid
      private
   contains
      procedure :: get_coordinate_field
      procedure :: get_supported_physical_dimensions
      procedure :: get_units
      procedure :: get_num_layers
      procedure :: matches
   end type MirrorVerticalGrid

   interface MirrorVerticalGrid
      module procedure :: new_MirrorVerticalGrid
   end interface MirrorVerticalGrid

contains

   function new_MirrorVerticalGrid() result(vertical_grid)
      type(MirrorVerticalGrid) :: vertical_grid
      call vertical_grid%set_id(VERTICAL_GRID_NOT_FOUND)
   end function new_MirrorVerticalGrid

   function get_num_layers(this) result(num_layers)
      integer :: num_layers
      class(MirrorVerticalGrid), intent(in) :: this
      num_layers = 0
      _UNUSED_DUMMY(this)
   end function get_num_layers
      
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
      type(StringVector) :: dimensions
      class(MirrorVerticalGrid), target, intent(in) :: this
      
      ! no op
 
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(dimensions)
   end function get_supported_physical_dimensions

   function get_units(this, physical_dimension, rc) result(units)
      character(len=:), allocatable :: units
      class(MirrorVerticalGrid), intent(in) :: this
      character(len=*), intent(in) :: physical_dimension
      integer, optional, intent(out) :: rc

      units = ''
      _FAIL('MirrorVerticalGrid should have been replaced before this procedure was called.')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(physical_dimension)
   end function get_units

   function matches(this, other)
      logical :: matches
      class(MirrorVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: other

      matches = .FALSE.
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(other)
   end function matches

end module mapl3g_MirrorVerticalGrid
