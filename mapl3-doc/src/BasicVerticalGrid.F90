#include "MAPL_Generic.h"

module mapl3g_BasicVerticalGrid
   use mapl3g_VerticalGrid
   use mapl3g_GriddedComponentDriver
   use mapl_ErrorHandling
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
!#      procedure :: make_filters
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

   interface
      module function can_connect_to(this, src, rc)
         logical :: can_connect_to
         class(BasicVerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: src
         integer, optional, intent(out) :: rc
      end function
   end interface

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
      
   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, rc)
      class(BasicVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      type(GriddedComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      integer, optional, intent(out) :: rc

      _FAIL('BasicVerticalGrid should have been connected to a different subclass before this is called.')
      _UNUSED_DUMMY(field)
      _UNUSED_DUMMY(coupler)
      _UNUSED_DUMMY(standard_name)
      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(units)
   end subroutine get_coordinate_field

   elemental logical function equal_to(a, b)
      type(BasicVerticalGrid), intent(in) :: a, b
      equal_to = a%num_levels == b%num_levels
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(BasicVerticalGrid), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to


!#   function make_filters(this, goal_grid, rc) result(filters)
!#      type(StateItemFilterWrapper), allocatable :: filters(:)
!#      class(BasicVerticalGrid), intent(in) :: this
!#      class(VerticalGrid), intent(in) :: goal_grid
!#      integer, optional, intent(out) :: rc
!#
!#      filters = 
!#      select
!#   end function make_filters

end module mapl3g_BasicVerticalGrid
