#include "MAPL_Generic.h"

! MirrorVerticalGrid objects should always have been replaced with an
! object of a different subclass by the timet they are used.  As such,
! it should only be used with import stateIntent, and will be replaced
! by whatever source grid is connected to it.

module mapl3g_MirrorVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_GriddedComponentDriver
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
      
   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, rc)
      class(MirrorVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      type(GriddedComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      integer, optional, intent(out) :: rc

      _FAIL('MirrorVerticalGrid should have been replaced before this procedure was called.')
 
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(field)
      _UNUSED_DUMMY(coupler)
      _UNUSED_DUMMY(standard_name)
      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(units)
   end subroutine get_coordinate_field

   logical function can_connect_to(this, src, rc)
      class(MirrorVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: src
      integer, optional, intent(out) :: rc

      can_connect_to = .false.
      _RETURN(_SUCCESS)
      
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src)
   end function can_connect_to

end module mapl3g_MirrorVerticalGrid
