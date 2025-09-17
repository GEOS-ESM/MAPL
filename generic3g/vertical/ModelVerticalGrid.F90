#include "MAPL.h"

module mapl3g_ModelVerticalGrid

   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_VerticalGrid
   use mapl3g_MirrorVerticalGrid
   use mapl3g_FixedLevelsVerticalGrid
   use mapl3g_StateRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_StateItemSpec
   use mapl3g_StateItemSpec
   use mapl3g_UngriddedDims
   use mapl3g_StateItemExtension
   use mapl3g_ExtensionFamily
   use mapl3g_ComponentDriver
   use mapl3g_VerticalStaggerLoc
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_GeomAspect
   use mapl3g_UnitsAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_AttributesAspect
   use mapl3g_TypekindAspect
   use mapl3g_VerticalGridAspect
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
      procedure :: is_identical_to
      procedure :: write_formatted

      ! subclass-specific methods
      procedure :: add_short_name
      procedure :: get_short_name
      procedure :: set_registry
      procedure :: get_registry
   end type ModelVerticalGrid

   interface ModelVerticalGrid
      procedure new_ModelVerticalGrid_basic
   end interface ModelVerticalGrid

   interface operator(==)
      module procedure equal_ModelVerticalGrid
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_ModelVerticalGrid
   end interface operator(/=)

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

   subroutine add_short_name(this, unusable, edge, center)
      class(ModelVerticalGrid), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: edge
      character(*), optional, intent(in) :: center

      if (present(edge)) this%short_name_edge = edge
      if (present(center)) this%short_name_center = center
      _UNUSED_DUMMY(unusable)
   end subroutine add_short_name

   function get_short_name(this, vertical_stagger, rc) result(short_name)
      character(:), allocatable :: short_name
      class(ModelVerticalGrid), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      integer, optional :: rc

      if (vertical_stagger == VERTICAL_STAGGER_EDGE) then
         short_name = this%short_name_edge
      else if (vertical_stagger == VERTICAL_STAGGER_CENTER) then
         short_name = this%short_name_center
      else
         _FAIL("unsupported vertical_stagger")
      end if

      _RETURN(_SUCCESS)
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

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_stagger, rc)
      class(ModelVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      class(ComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: short_name
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: new_extension
      type(StateItemSpec), pointer :: new_spec
      type(StateItemSpec), target :: goal_spec
      type(AspectMap), pointer :: aspects
      class(StateItemAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field_

      short_name = this%get_short_name(vertical_stagger)
      v_pt = VirtualConnectionPt(state_intent="export", short_name=short_name)

      aspects => goal_spec%get_aspects()
      call aspects%insert(CLASS_ASPECT_ID, FieldClassAspect(standard_name='', long_name=''))
      call aspects%insert(GEOM_ASPECT_ID, GeomAspect(geom))
      call aspects%insert(VERTICAL_GRID_ASPECT_ID, VerticalGridAspect(vertical_grid=this, vertical_stagger=vertical_stagger))
      call aspects%insert(TYPEKIND_ASPECT_ID, TypekindAspect(typekind))
      call aspects%insert(UNITS_ASPECT_ID, UnitsAspect(units))
      call aspects%insert(UNGRIDDED_DIMS_ASPECT_ID, UngriddedDimsAspect(UngriddedDimS()))
      call aspects%insert(ATTRIBUTES_ASPECT_ID, AttributesAspect())
      
      new_extension => this%registry%extend(v_pt, goal_spec, _RC)
      coupler => new_extension%get_producer()
      new_spec => new_extension%get_spec()

      class_aspect => new_spec%get_aspect(CLASS_ASPECT_ID, _RC)
      select type (class_aspect)
      type is (FieldClassAspect)
         call class_aspect%get_payload(field_, _RC)
         field = field_
      class default
         _FAIL("unsupported aspect type; must be FieldClassAspect")
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

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "ModelVerticalGrid("
      if (allocated(this%standard_name)) then
         write(unit, "(a, 3x, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "standard name: ", this%standard_name
      end if
      write(unit, "(a, 3x, a, g0)", iostat=iostat, iomsg=iomsg) new_line("a"), "num_levels: ", this%num_levels
      if (allocated(this%short_name_edge)) then
         write(unit, "(a, 3x, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "field (edge): ", this%short_name_edge
      end if
      if (allocated(this%short_name_center)) then
         write(unit, "(a, 3x, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "field (center): ", this%short_name_center
      end if
      write(unit, "(a)") ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   logical function can_connect_to(this, dst, rc)
      class(ModelVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: dst
      integer, optional, intent(out) :: rc

      if (this%same_id(dst)) then
         can_connect_to = .true.
         _RETURN(_SUCCESS)
      end if

      select type (dst)
      type is (MirrorVerticalGrid)
         can_connect_to = .true.
      type is (FixedLevelsVerticalGrid)
         can_connect_to = .true.
      class default
         _FAIL("ModelVerticalGrid can only connect to FixedLevelsVerticalGrid, or MirrorVerticalGrid")
      end select

      _RETURN(_SUCCESS)
   end function can_connect_to

   logical function is_identical_to(this, that, rc)
      class(ModelVerticalGrid), intent(in) :: this
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
      type is(ModelVerticalGrid)
         is_identical_to = (this == that)
      end select

      _RETURN(_SUCCESS)
   end function is_identical_to

   impure elemental logical function equal_ModelVerticalGrid(a, b) result(equal)
      type(ModelVerticalGrid), intent(in) :: a, b

      equal = a%standard_name == b%standard_name
      if (.not. equal) return
      equal = (a%get_units() == b%get_units())
      if (.not. equal) return
      equal = (a%num_levels == b%num_levels)
      if (.not. equal) return
      equal = (a%short_name_edge == b%short_name_edge)
      if (.not. equal) return
      equal = (a%short_name_center == b%short_name_center)
   end function equal_ModelVerticalGrid

   impure elemental logical function not_equal_ModelVerticalGrid(a, b) result(not_equal)
      type(ModelVerticalGrid), intent(in) :: a, b

      not_equal = .not. (a==b)
   end function not_equal_ModelVerticalGrid

end module mapl3g_ModelVerticalGrid
