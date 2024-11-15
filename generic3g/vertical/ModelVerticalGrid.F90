#include "MAPL_Generic.h"

module mapl3g_ModelVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_StateRegistry
   use mapl3g_MultiState
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemSpec
   use mapl3g_FieldSpec
   use mapl3g_UngriddedDims
   use mapl3g_StateItemExtension
   use mapl3g_ExtensionFamily
   use mapl3g_ExtensionAction
   use mapl3g_StateItemExtensionPtrVector
   use mapl3g_GriddedComponentDriver
   use mapl3g_VerticalDimSpec
   use gftl2_StringVector
   use esmf

   implicit none
   private

   public :: ModelVerticalGrid

   type, extends(VerticalGrid) :: ModelVerticalGrid
      private
      character(:), allocatable :: standard_name
      integer :: num_levels = -1
      character(:), allocatable :: short_name_edge
      character(:), allocatable :: short_name_center
      type(StateRegistry), pointer :: registry => null()
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to
      procedure :: write_formatted

      ! subclass-specific methods
      procedure :: add_short_names
      procedure :: get_short_name
      procedure :: set_registry
      procedure :: get_registry
   end type ModelVerticalGrid

   interface ModelVerticalGrid
      procedure new_ModelVerticalGrid_basic
   end interface ModelVerticalGrid

   interface
      module function can_connect_to(this, src, rc)
         logical :: can_connect_to
         class(ModelVerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: src
         integer, optional, intent(out) :: rc
      end function
   end interface

   integer, parameter :: NDX_EDGE=1, NDX_CENTER=2

   ! TODO:
   ! - Ensure that there really is a vertical dimension

contains

   function new_ModelVerticalGrid_basic(standard_name, units, num_levels) result(vgrid)
      type(ModelVerticalGrid) :: vgrid
      character(*), intent(in) :: standard_name
      character(*) , intent(in) :: units
      integer, intent(in) :: num_levels

      call vgrid%set_id()
      vgrid%standard_name = standard_name
      call vgrid%set_units(units)
      vgrid%num_levels = num_levels
   end function new_ModelVerticalGrid_basic

   integer function get_num_levels(this) result(num_levels)
      class(ModelVerticalGrid), intent(in) :: this
      num_levels = this%num_levels
   end function get_num_levels

   subroutine add_short_names(this, edge, center)
      class(ModelVerticalGrid), intent(inout) :: this
      character(*), optional, intent(in) :: edge
      character(*), optional, intent(in) :: center

      if (present(edge)) this%short_name_edge = edge
      if (present(center)) this%short_name_center = center
   end subroutine add_short_names

   function get_short_name(this, vertical_dim_spec, rc) result(short_name)
      character(:), allocatable :: short_name
      class(ModelVerticalGrid), intent(in) :: this
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      integer, optional :: rc

      if (vertical_dim_spec == VERTICAL_DIM_EDGE) then
         short_name = this%short_name_edge
         _RETURN(_SUCCESS)
      else if (vertical_dim_spec == VERTICAL_DIM_CENTER) then
         short_name = this%short_name_center
         _RETURN(_SUCCESS)
      else
         _FAIL("unsupported vertical_dim_spec")
      end if
   end function get_short_name

   subroutine set_registry(this, registry)
      class(ModelVerticalGrid), intent(inout) :: this
      type(StateRegistry), target, intent(in) :: registry

      this%registry => registry
   end subroutine set_registry

   function get_registry(this) result(registry)
      class(ModelVerticalGrid), intent(in) :: this
      type(StateRegistry), pointer :: registry
      registry => this%registry
   end function get_registry

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_dim_spec, rc)
      class(ModelVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      type(GriddedComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: short_name
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: new_extension
      class(StateItemSpec), pointer :: new_spec
      type(FieldSpec) :: goal_spec

      short_name = this%get_short_name(vertical_dim_spec)
      v_pt = VirtualConnectionPt(state_intent="export", short_name=short_name)

      goal_spec = FieldSpec( &
           geom=geom, vertical_grid=this, vertical_dim_spec=vertical_dim_spec, &
           typekind=typekind, standard_name=standard_name, units=units, ungridded_dims=UngriddedDims())

      new_extension => this%registry%extend(v_pt, goal_spec, _RC)
      coupler => new_extension%get_producer()
      new_spec => new_extension%get_spec()
      select type (new_spec)
      type is (FieldSpec)
         field = new_spec%get_payload()
      class default
         _FAIL("unsupported spec type; must be FieldSpec")
      end select

      _RETURN(_SUCCESS)
   end subroutine get_coordinate_field

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(ModelVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a, a, g0, a)", iostat=iostat, iomsg=iomsg) &
           "ModelVerticalGrid(", &
           "num levels: ", this%num_levels, &
           ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

end module mapl3g_ModelVerticalGrid
