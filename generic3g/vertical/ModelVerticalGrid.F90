#include "MAPL.h"

module mapl3g_ModelVerticalGrid

   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_VerticalGrid_API
   use mapl3g_Field_API
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
   use pfio
   use esmf
   use gftl2_StringVector, only: StringVector
   implicit none(type,external)
   private

   public :: ModelVerticalGridSpec
   public :: ModelVerticalGrid
   public :: ModelVerticalGridFactory

   type, extends(VerticalGridSpec) :: ModelVerticalGridSpec
      private
      type(StringVector) :: names
      type(StringVector) :: physical_dimensions
      integer :: num_levels = -1
   end type ModelVerticalGridSpec

   type, extends(VerticalGrid) :: ModelVerticalGrid
      private
      type(ModelVerticalGridSpec) :: spec
      type(StateRegistry), pointer :: registry => null()
   contains
      procedure :: initialize
      procedure :: get_num_levels
      procedure :: get_units
      procedure :: get_coordinate_field
!#      procedure :: is_identical_to
      procedure :: write_formatted
      procedure :: get_supported_physical_dimensions
      procedure :: matches

      ! subclass-specific methods
      procedure :: add_field
      procedure :: set_registry
      procedure :: get_registry
   end type ModelVerticalGrid

   ! Factory type
   type, extends(VerticalGridFactory) :: ModelVerticalGridFactory
   contains
      procedure :: get_name
      procedure :: supports_spec
      procedure :: supports_file_metadata
      procedure :: supports_config
      procedure :: create_spec_from_config
      procedure :: create_spec_from_file_metadata
      procedure :: create_grid_from_spec
   end type ModelVerticalGridFactory


  interface ModelVerticalGrid
      procedure new_ModelVerticalGrid_basic
   end interface ModelVerticalGrid

!#   interface operator(==)
!#      module procedure equal_ModelVerticalGrid
!#   end interface operator(==)

!#   interface operator(/=)
!#      module procedure not_equal_ModelVerticalGrid
!#   end interface operator(/=)

   ! TODO:
   ! - Ensure that there really is a vertical dimension

contains

   function new_ModelVerticalGrid_basic(physical_dimension, short_name, num_levels) result(vgrid)
      type(ModelVerticalGrid) :: vgrid
      character(*), intent(in) :: physical_dimension
      character(*), intent(in) :: short_name
      integer, intent(in) :: num_levels

      vgrid%spec%num_levels = num_levels
      call vgrid%spec%names%push_back(short_name)
      call vgrid%spec%physical_dimensions%push_back(physical_dimension)
   end function new_ModelVerticalGrid_basic

   integer function get_num_levels(this) result(num_levels)
      class(ModelVerticalGrid), intent(in) :: this
      num_levels = this%spec%num_levels
   end function get_num_levels

   function get_units(this, physical_dimension, rc) result(units)
      character(:), allocatable :: units
      class(ModelVerticalGrid), intent(in) :: this
      character(*), intent(in) :: physical_dimension
      integer, optional, intent(out) :: rc
      
      character(:), allocatable :: short_name
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: primary
      type(StateItemSpec), pointer :: spec
      class(StateItemAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      integer :: i, n
      integer :: status

      units = '<invalid>'

      n = this%spec%physical_dimensions%size()
      do i = 1, n
         if (this%spec%physical_dimensions%of(i) == physical_dimension) then
            short_name = this%spec%names%of(i)
            exit
         end if
      end do
      _ASSERT(i <= n, 'Physical dimension not found.')

      v_pt = VirtualConnectionPt(state_intent="export", short_name=short_name)
      primary => this%registry%get_primary_extension(v_pt, _RC)
      spec => primary%get_spec()

      class_aspect => spec%get_aspect(CLASS_ASPECT_ID, _RC)
      select type (class_aspect)
      type is (FieldClassAspect)
         call class_aspect%get_payload(field=field, _RC)
         call mapl_FieldGet(field, units=units, _RC)
      class default
         _FAIL("unsupported aspect type; must be FieldClassAspect")
      end select

      _RETURN(_SUCCESS)
   end function get_units


   subroutine add_field(this, short_name, physical_dimension)
      class(ModelVerticalGrid), intent(inout) :: this
      character(len=*), intent(in) :: short_name
      character(len=*), intent(in) :: physical_dimension

      call this%spec%names%push_back(short_name)
      call this%spec%physical_dimensions%push_back(physical_dimension)
   end subroutine add_field


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

   function get_coordinate_field(this, geom, physical_dimension, units, typekind, coupler, rc) result(field)
      type(ESMF_Field) :: field
      class(ModelVerticalGrid), intent(in) :: this
      character(*), intent(in) :: physical_dimension
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      class(ComponentDriver), pointer, intent(out) :: coupler
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n
      character(:), allocatable :: short_name
      type(VirtualConnectionPt) :: v_pt
      type(StateItemExtension), pointer :: new_extension
      type(StateItemSpec), pointer :: primary, new_spec
      type(StateItemSpec), target :: goal_spec
      type(AspectMap), pointer :: aspects
      class(StateItemAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field_

      n = this%spec%physical_dimensions%size()
      do i = 1, n
         if (this%spec%physical_dimensions%of(i) == physical_dimension) then
            short_name = this%spec%names%of(i)
            exit
         end if
      end do
      _ASSERT(i <= n, 'Physical dimension not found.')

      v_pt = VirtualConnectionPt(state_intent="export", short_name=short_name)

      aspects => goal_spec%get_aspects()
      call aspects%insert(CLASS_ASPECT_ID, FieldClassAspect(standard_name='', long_name=''))
      call aspects%insert(GEOM_ASPECT_ID, GeomAspect(geom))
      call aspects%insert(VERTICAL_GRID_ASPECT_ID, VerticalGridAspect(vertical_grid=this, vertical_stagger=VERTICAL_STAGGER_EDGE))
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
         call class_aspect%get_payload(field=field_, _RC)
         _ASSERT(allocated(field_), 'expected payload to have a field')
         field = field_
      class default
         _FAIL("unsupported aspect type; must be FieldClassAspect")
      end select

      _RETURN(_SUCCESS)
   end function get_coordinate_field

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(ModelVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

!#      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "ModelVerticalGrid("
!#      if (allocated(this%physical_dimension)) then
!#         write(unit, "(a, 3x, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "physical_dimension: ", this%physical_dimension
!#      end if
!#      write(unit, "(a, 3x, a, g0)", iostat=iostat, iomsg=iomsg) new_line("a"), "num_levels: ", this%num_levels
!#      if (allocated(this%short_name)) then
!#         write(unit, "(a, 3x, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "field: ", this%short_name
!#      end if
!#      write(unit, "(a)") ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted


   function get_supported_physical_dimensions(this) result(dimensions)
      type(StringVector) :: dimensions
      class(ModelVerticalGrid), target, intent(in) :: this

      dimensions = this%spec%physical_dimensions

   end function get_supported_physical_dimensions

   ! Factory methods
   subroutine initialize(this, spec)
      class(ModelVerticalGrid), intent(inout) :: this
      type(ModelVerticalGridSpec), intent(in) :: spec

      this%spec = spec

   end subroutine initialize

   logical function matches(this, other)
      class(ModelVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: other

      matches = this%get_num_levels() == other%get_num_levels()
      if (.not. matches) return

      select type (other)
      type is (BasicVerticalGrid)
         matches = .true.
         return
      class default
         matches = .false.
      end select

   end function matches

   function get_name(this) result(name)
      character(len=:), allocatable :: name
      class(ModelVerticalGridFactory), intent(in) :: this
      
      name = "ModelVerticalGrid"
   end function get_name

   function supports_spec(this, spec, rc) result(is_supported)
      logical :: is_supported
      class(ModelVerticalGridFactory), intent(in) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ModelVerticalGridSpec) :: fixed_spec

      is_supported = same_type_as(spec, fixed_spec)

      _RETURN(_SUCCESS)
   end function supports_spec

   function supports_file_metadata(this, file_metadata, rc) result(is_supported)
      logical :: is_supported
      class(ModelVerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, optional, intent(out) :: rc
      
      ! Implementation would check if file_metadata contains required information
      is_supported = .false.  ! Placeholder
      _RETURN(_SUCCESS)
   end function supports_file_metadata

   function supports_config(this, config, rc) result(is_supported)
      logical :: is_supported
      class(ModelVerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_grid_type
      logical :: has_fields
      logical :: has_num_levels
      character(len=:), allocatable :: grid_type

      is_supported = .false.

      has_grid_type = esmf_HConfigIsDefined(config, keyString="grid_type", _RC)
      _RETURN_UNLESS(has_grid_type)

      grid_type = esmf_HConfigAsString(config, keyString="grid_type", _RC)
      _RETURN_UNLESS(grid_type == 'model')

      has_fields = esmf_HConfigIsDefined(config, keyString="fields", _RC)
      _RETURN_UNLESS(has_fields)

      ! We need num_levels to bootstrap, as field gets num levels from grid
      has_num_levels = esmf_HConfigIsDefined(config, keyString="num_levels", _RC)
      _RETURN_UNLESS(has_num_levels)
      
      is_supported = .true.

      _RETURN(_SUCCESS)
   end function supports_config

   function create_spec_from_config(this, config, rc) result(spec)
      class(VerticalGridSpec), allocatable :: spec
      class(ModelVerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in), target :: config
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_HConfig) :: fields_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      character(len=:), allocatable :: physical_dimension
      character(len=:), allocatable :: field_name

      allocate(ModelVerticalGridSpec :: spec)

      select type (spec)
      type is (ModelVerticalGridSpec)
         
         spec%num_levels = esmf_HConfigAsI4(config, keyString="num_levels", _RC)
         
         fields_cfg = esmf_HConfigCreateAt(config, keyString="fields", _RC)
         
         b = esmf_HConfigIterBegin(fields_cfg)
         e = ESMF_HConfigIterEnd(fields_cfg)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            physical_dimension = ESMF_HConfigAsStringMapKey(iter, _RC)
            field_name = ESMF_HConfigAsStringMapVal(iter, _RC)
            call spec%names%push_back(field_name)
            call spec%physical_dimensions%push_back(physical_dimension)
         end do
         call esmf_HConfigDestroy(fields_cfg, _RC)
      end select

      _RETURN(_SUCCESS)
   end function create_spec_from_config

   function create_spec_from_file_metadata(this, file_metadata, rc) result(spec)
      class(VerticalGridSpec), allocatable :: spec
      class(ModelVerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, intent(out), optional :: rc
      
      ! Placeholder implementation
      integer :: status
      _RETURN(_FAILURE)
   end function create_spec_from_file_metadata

   function create_grid_from_spec(this, spec, rc) result(grid)
      class(VerticalGrid), allocatable :: grid
      class(ModelVerticalGridFactory), intent(in) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, intent(out), optional :: rc
      
      type(ModelVerticalGrid) :: local_grid
      integer :: status
      
      select type (spec)
      type is (ModelVerticalGridSpec)
         call local_grid%initialize(spec)
         allocate(grid, source=local_grid)
      class default
         _RETURN(_FAILURE)
      end select
      _RETURN(_SUCCESS)
   end function create_grid_from_spec

   ! Helper function to get default units for a physical dimension
   function get_default_units(physical_dimension) result(units)
      character(len=:), allocatable :: units
      character(len=*), intent(in) :: physical_dimension
      
      select case (physical_dimension)
         case ('pressure')
            units = 'Pa'
         case ('height', 'altitude')
            units = 'm'
         case ('depth')
            units = 'm'
         case ('layer')
            units = '1'
         case default
            units = '<unknown>'
      end select
   end function get_default_units
   
end module mapl3g_ModelVerticalGrid
