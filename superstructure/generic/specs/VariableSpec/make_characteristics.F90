#include "MAPL.h"

submodule (mapl_VariableSpec_mod) make_characteristics_smod
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
        TYPE_KIND_CHARACTERISTIC_KIND, UNGRIDDED_DIMS_CHARACTERISTIC_KIND, GEOMETRY_CHARACTERISTIC_KIND, &
        VERTICAL_GRID_CHARACTERISTIC_KIND, STANDARD_NAME_CHARACTERISTIC_KIND, ATTRIBUTES_CHARACTERISTIC_KIND, &
        VERTICAL_STAGGER_CHARACTERISTIC_KIND, VERTICAL_ORIENTATION_CHARACTERISTIC_KIND, &
        HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND, &
        ITEM_SHAPE_CHARACTERISTIC_KIND, LONG_NAME_CHARACTERISTIC_KIND, RESTART_MODE_CHARACTERISTIC_KIND, &
        QUANTITY_TYPE_CHARACTERISTIC_KIND, CONSERVATION_CHARACTERISTIC_KIND, &
        NORMALIZATION_CHARACTERISTIC_KIND, EXPRESSION_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED, CHARACTERISTIC_STATUS_MIRRORED, &
        CHARACTERISTIC_STATUS_DEFERRED, operator(==)
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_VERTICALGRID, operator(==)
   use mapl_AttributesCharacteristic_mod, only: AttributesCharacteristic
   use mapl_ItemShapeCharacteristic_mod, only: ItemShapeCharacteristic
   use mapl_StandardNameCharacteristic_mod, only: StandardNameCharacteristic, UNKNOWN_STANDARD_NAME
   use mapl_LongNameCharacteristic_mod, only: LongNameCharacteristic
   use mapl_RestartModeCharacteristic_mod, only: RestartModeCharacteristic
   use mapl_RestartModes_mod, only: RESTART_REQUIRED
   use mapl_ExpressionCharacteristic_mod, only: ExpressionCharacteristic
   use mapl_QuantityTypeCharacteristic_mod, only: QuantityTypeCharacteristic
   use mapl_QuantityType_mod, only: QUANTITY_UNKNOWN, operator(==)
   use mapl_ConservationCharacteristic_mod, only: ConservationCharacteristic
   use mapl_NormalizationCharacteristic_mod, only: NormalizationCharacteristic
   use mapl_GeometryCharacteristic_mod, only: GeometryCharacteristic
   use mapl_HorizontalDimsSpecCharacteristic_mod, only: HorizontalDimsSpecCharacteristic
   use mapl_HorizontalDimsSpec_mod, only: HORIZONTAL_DIMS_NONE, operator(==)
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap, &
        StateItemCharacteristicMapIterator, operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic, UNKNOWN_UNITS
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic
   use mapl_typekind_mod, only: MAPL_TYPEKIND_MIRROR
   use mapl_UngriddedDimsCharacteristic_mod, only: UngriddedDimsCharacteristic
   use mapl_VerticalGridCharacteristic_mod, only: VerticalGridCharacteristic
   use mapl_VerticalStaggerCharacteristic_mod, only: VerticalStaggerCharacteristic
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_INVALID
   use mapl_VerticalOrientationCharacteristic_mod, only: VerticalOrientationCharacteristic
   use mapl_VerticalCoordinateDirection_mod, only: VCOORD_DIRECTION_UP, VCOORD_DIRECTION_DOWN
   use mapl_VerticalAlignment_mod, only: VerticalAlignment, VALIGN_UP, VALIGN_DOWN, VALIGN_WITH_GRID, &
        VALIGN_INVALID, operator(/=), operator(==)
   implicit none(type, external)

   abstract interface
      subroutine factory_interface(spec, descriptor, rc)
         import :: VariableSpec, StateItemDescriptor
         class(VariableSpec), intent(in) :: spec
         type(StateItemDescriptor), intent(inout) :: descriptor
         integer, optional, intent(out) :: rc
      end subroutine factory_interface
   end interface

   type :: Factory
      procedure(factory_interface), pointer, nopass :: make => null()
   end type Factory

contains

   ! Runs every characteristic factory against the descriptor. Each factory
   ! decides for itself whether the item shape carries its characteristic.
   module subroutine make_characteristics(this, descriptor, rc)
      class(VariableSpec), intent(in) :: this
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(Factory) :: factories(17)

      factories(1)%make => make_units_characteristic
      factories(2)%make => make_typekind_characteristic
      factories(3)%make => make_ungridded_dims_characteristic
      factories(4)%make => make_geometry_characteristic
      factories(5)%make => make_vertical_grid_characteristic
      factories(6)%make => make_standard_name_characteristic
      factories(7)%make => make_attributes_characteristic
      factories(8)%make => make_vertical_stagger_characteristic
      factories(9)%make => make_vertical_orientation_characteristic
      factories(10)%make => make_horizontal_dims_spec_characteristic
      factories(11)%make => make_item_shape_characteristic
      factories(12)%make => make_long_name_characteristic
      factories(13)%make => make_restart_mode_characteristic
      factories(14)%make => make_quantity_type_characteristic
      factories(15)%make => make_conservation_characteristic
      factories(16)%make => make_normalization_characteristic
      factories(17)%make => make_expression_characteristic

      do i = 1, size(factories)
         call factories(i)%make(this, descriptor, _RC)
      end do

      call check_intent_rules(this, descriptor, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_characteristics

   ! Backstop for a hand-built spec that bypasses make_VariableSpec, and for
   ! fields set by direct assignment after it (conservation, normalization).
   ! Applies the same intent rules as make_VariableSpec: an explicit request
   ! is rejected even for a shape with no entry of that kind. A MIRRORED entry
   ! the factories resolved is rejected too, and the scan names its kind so
   ! the error points at the characteristic.
   subroutine check_intent_rules(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(in) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemCharacteristicMap), target :: characteristics
      type(StateItemCharacteristicMapIterator) :: iter
      class(StateItemCharacteristic), pointer :: c
      character(:), allocatable :: msg, kind_name, reason
      type(StateItemCharacteristicKind) :: kind

      reason = intent_rule_violation(spec%state_intent, spec%itemType, mirror=.false.)
      _ASSERT(len(reason) == 0, reason // ': ' // spec%short_name)

      reason = intent_rule_violation(spec%state_intent, spec%itemType, mirror=.true.)
      _RETURN_IF(len(reason) == 0)

      characteristics = descriptor%get_characteristics()
      associate (e => characteristics%ftn_end())
         iter = characteristics%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%second()
            if (c%get_status() == CHARACTERISTIC_STATUS_MIRRORED) then
               kind = iter%first()
               kind_name = kind%to_string()
               msg = reason // ': ' // spec%short_name // ' (' // kind_name // ')'
               _FAIL(msg)
            end if
         end do
      end associate

      ! A request on a shape with no entry of that kind leaves nothing to scan.
      _ASSERT(.not. spec%requests_mirror(), reason // ': ' // spec%short_name)

      _RETURN(_SUCCESS)
   end subroutine check_intent_rules

   ! Declared units are SPECIFIED. Undeclared (or UNKNOWN_UNITS) units are
   ! UNCHECKED: they match anything and are never converted. Only shapes
   ! that carry units get an entry.
   subroutine make_units_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(PhysicalUnitsCharacteristic) :: units
      logical :: declared

      _RETURN_UNLESS(carries_field_description(spec%itemType))

      declared = allocated(spec%units)
      if (declared) declared = spec%units /= UNKNOWN_UNITS
      if (declared) then
         units = PhysicalUnitsCharacteristic(spec%units)
      else
         ! A default-initialized characteristic is INVALID and fails freeze.
         call units%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
      end if

      call descriptor%set_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, units, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_units_characteristic

   ! MAPL_TYPEKIND_MIRROR is MIRRORED (the import adopts the matched
   ! export's). Anything else, including the R4 default, is SPECIFIED; it is
   ! never DEFERRED.
   subroutine make_typekind_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(TypeKindCharacteristic) :: type_kind

      _RETURN_UNLESS(carries_field_description(spec%itemType))

      if (spec%typekind == MAPL_TYPEKIND_MIRROR) then
         call type_kind%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         type_kind = TypeKindCharacteristic(spec%typekind)
      end if

      call descriptor%set_characteristic(TYPE_KIND_CHARACTERISTIC_KIND, type_kind, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_typekind_characteristic

   ! An explicitly mirrored UngriddedDims is MIRRORED (the import adopts the
   ! matched export's). Anything else, including the empty default, is
   ! SPECIFIED; it is never DEFERRED. An EXPRESSION gets an entry from its own
   ! declaration too (the empty default if it declares none), so an EXPRESSION
   ! export mismatches a FIELD import that declares ungridded dims.
   subroutine make_ungridded_dims_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(UngriddedDimsCharacteristic) :: ungridded_dims

      _RETURN_UNLESS(has_field_metadata(spec%itemType))

      if (spec%ungridded_dims%is_mirror()) then
         call ungridded_dims%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         ungridded_dims = UngriddedDimsCharacteristic(spec%ungridded_dims)
      end if

      call descriptor%set_characteristic(UNGRIDDED_DIMS_CHARACTERISTIC_KIND, ungridded_dims, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_ungridded_dims_characteristic

   ! A geom declared on the item is SPECIFIED. mirror_geom makes it MIRRORED:
   ! the item adopts the geometry of what it is connected to. Otherwise the
   ! component supplies it after ADVERTISE (its default geom at GEOM_B, or
   ! its own init phase), so the entry is DEFERRED and freeze fails if the
   ! component never provides one. An expression has no geometry of its own
   ! and is MIRRORED (the allowlisted case). This differs from the legacy
   ! aspect, which treats "no geom" as MIRRORED. A declared geom_id with no
   ! geom is SPECIFIED by identity alone. An item with no horizontal
   ! dimensions (horizontal_dims_spec NONE) is never compared but still needs
   ! a geom to be created, so it is UNCHECKED and takes the component's geom
   ! when it declares none. Every shape that carries a field, plus SERVICE,
   ! has the entry; STATE and WILDCARD do not.
   subroutine make_geometry_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeometryCharacteristic) :: geometry
      character(20) :: id_buffer
      logical :: compared

      _RETURN_UNLESS(carries_field_description(spec%itemType) .or. spec%itemType == MAPL_STATEITEM_SERVICE)

      compared = .not. (has_horizontal_dims_spec(spec%itemType) .and. spec%horizontal_dims_spec == HORIZONTAL_DIMS_NONE)

      if (allocated(spec%geom)) then
         if (spec%geom_id%is_assigned()) then
            write(id_buffer, '(I0)') spec%geom_id%get_value()
            geometry = GeometryCharacteristic(geom_id=trim(id_buffer), geom=spec%geom, &
                 regridder_param=spec%regrid_param, compared=compared)
         else
            geometry = GeometryCharacteristic(geom=spec%geom, regridder_param=spec%regrid_param, compared=compared)
         end if
      else if (spec%geom_id%is_assigned()) then
         write(id_buffer, '(I0)') spec%geom_id%get_value()
         geometry = GeometryCharacteristic(geom_id=trim(id_buffer), regridder_param=spec%regrid_param, compared=compared)
      else
         geometry = GeometryCharacteristic(regridder_param=spec%regrid_param, compared=compared)
         if (.not. compared) then
            call geometry%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
         else if (spec%itemType == MAPL_STATEITEM_EXPRESSION .or. spec%mirror_geom) then
            call geometry%set_status(CHARACTERISTIC_STATUS_MIRRORED)
         else
            call geometry%set_status(CHARACTERISTIC_STATUS_DEFERRED)
         end if
      end if

      call descriptor%set_characteristic(GEOMETRY_CHARACTERISTIC_KIND, geometry, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_geometry_characteristic

   ! SPECIFIED with GEOM unless the item declares NONE. HORIZONTAL_DIMS_MIRROR
   ! is MIRRORED (see check_intent_rules for where that is allowed). Only Field
   ! and FieldBundle payloads have one: an EXPRESSION or SERVICE has no
   ! horizontal layout of its own.
   subroutine make_horizontal_dims_spec_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(has_horizontal_dims_spec(spec%itemType))

      call descriptor%set_characteristic(HORIZONTAL_DIMS_SPEC_CHARACTERISTIC_KIND, &
           HorizontalDimsSpecCharacteristic(spec%horizontal_dims_spec), _RC)

      _RETURN(_SUCCESS)
   end subroutine make_horizontal_dims_spec_characteristic

   ! A vertical grid declared on the item is SPECIFIED. mirror_vgrid, or an
   ! EXPRESSION with no grid, is MIRRORED. Otherwise the component supplies
   ! it at GEOM_B (DEFERRED); where it has none the entry is dropped, so a
   ! 2D item in a component without a vertical grid stays valid. An explicit
   ! vertical_stagger NONE means no vertical dimension and gets no entry.
   ! The VERTICALGRID composite has no grid object, only member names.
   subroutine make_vertical_grid_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(VerticalGridCharacteristic) :: vertical_grid_char

      if (allocated(spec%state_item_variant)) then
         if (spec%state_item_variant == MAPL_STATEITEM_VERTICALGRID) then
            vertical_grid_char = VerticalGridCharacteristic(dimensions=spec%get_member_names())
            call descriptor%set_characteristic(VERTICAL_GRID_CHARACTERISTIC_KIND, vertical_grid_char, _RC)
            _RETURN(_SUCCESS)
         end if
      end if

      _RETURN_UNLESS(carries_field_description(spec%itemType))
      if (allocated(spec%vertical_stagger)) then
         _RETURN_IF(spec%vertical_stagger%is_none())
      end if

      if (allocated(spec%vertical_grid)) then
         vertical_grid_char = VerticalGridCharacteristic(vgrid=spec%vertical_grid)
      else if (spec%itemType == MAPL_STATEITEM_EXPRESSION .or. spec%mirror_vgrid) then
         call vertical_grid_char%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         call vertical_grid_char%set_status(CHARACTERISTIC_STATUS_DEFERRED)
      end if

      call descriptor%set_characteristic(VERTICAL_GRID_CHARACTERISTIC_KIND, vertical_grid_char, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_vertical_grid_characteristic

   ! A declared standard_name is SPECIFIED. The MAPL_STRING_MIRROR request is
   ! MIRRORED (see check_intent_rules for where that is allowed). Undeclared
   ! (or UNKNOWN_STANDARD_NAME) is UNCHECKED: it matches anything. Only a
   ! FIELD or a BRACKET carries one (a BRACKET -> FIELD pair compares names, as
   ! legacy does); the other shapes either enforce their own names (VECTOR) or
   ! are exempt.
   subroutine make_standard_name_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(StandardNameCharacteristic) :: standard_name
      logical :: declared

      _RETURN_UNLESS(any(spec%itemType == [MAPL_STATEITEM_FIELD, MAPL_STATEITEM_BRACKET]))

      if (spec%standard_name_mirror) then
         call standard_name%set_status(CHARACTERISTIC_STATUS_MIRRORED)
         call descriptor%set_characteristic(STANDARD_NAME_CHARACTERISTIC_KIND, standard_name, _RC)
         _RETURN(_SUCCESS)
      end if

      declared = allocated(spec%standard_name)
      if (declared) declared = spec%standard_name /= UNKNOWN_STANDARD_NAME
      if (declared) then
         standard_name = StandardNameCharacteristic(spec%standard_name)
      else
         call standard_name%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
      end if

      call descriptor%set_characteristic(STANDARD_NAME_CHARACTERISTIC_KIND, standard_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_standard_name_characteristic

   ! Always SPECIFIED, even when empty: an import that requires attributes
   ! must mismatch an export that declares none. Same shape set as ungridded
   ! dims, and includes EXPRESSION.
   subroutine make_attributes_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(has_field_metadata(spec%itemType))

      call descriptor%set_characteristic(ATTRIBUTES_CHARACTERISTIC_KIND, AttributesCharacteristic(spec%attributes), _RC)

      _RETURN(_SUCCESS)
   end subroutine make_attributes_characteristic

   ! A real stagger (CENTER, EDGE, or an explicit NONE) is SPECIFIED.
   ! VERTICAL_STAGGER_MIRROR is MIRRORED (see check_intent_rules for where
   ! that is allowed). Undeclared (unallocated, or the
   ! INVALID that the YAML parser passes for an omitted vertical_dim_spec) is
   ! MIRRORED for an EXPRESSION or with mirror_vgrid, else DEFERRED: there is
   ! no built-in default, the component or a realized field supplies it. The
   ! VERTICALGRID composite has no stagger.
   subroutine make_vertical_stagger_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(VerticalStaggerCharacteristic) :: stagger_char
      logical :: declared, real_stagger

      if (allocated(spec%state_item_variant)) then
         _RETURN_IF(spec%state_item_variant == MAPL_STATEITEM_VERTICALGRID)
      end if
      _RETURN_UNLESS(carries_field_description(spec%itemType))

      declared = allocated(spec%vertical_stagger)
      real_stagger = .false.
      if (declared) then
         declared = spec%vertical_stagger%to_string() /= VERTICAL_STAGGER_INVALID%to_string()
         real_stagger = declared .and. .not. spec%vertical_stagger%is_mirror()
      end if

      if (real_stagger) then
         stagger_char = VerticalStaggerCharacteristic(spec%vertical_stagger)
      else if (declared .or. spec%itemType == MAPL_STATEITEM_EXPRESSION .or. spec%mirror_vgrid) then
         call stagger_char%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         call stagger_char%set_status(CHARACTERISTIC_STATUS_DEFERRED)
      end if

      call descriptor%set_characteristic(VERTICAL_STAGGER_CHARACTERISTIC_KIND, stagger_char, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_vertical_stagger_characteristic

   ! vertical_alignment 'upward' or 'downward' is SPECIFIED. 'mirror' is
   ! MIRRORED (see check_intent_rules for where that is allowed). Absent or
   ! 'with_grid' follows the grid: SPECIFIED with the item grid's direction,
   ! else MIRRORED for an EXPRESSION or with mirror_vgrid, else DEFERRED until
   ! the component grid is known. A DEFERRED entry whose component has no
   ! vertical grid is dropped at GEOM_B. An explicit vertical_stagger NONE
   ! means no vertical dimension, so no entry.
   subroutine make_vertical_orientation_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(VerticalOrientationCharacteristic) :: orientation_char
      character(:), allocatable :: requested
      type(VerticalAlignment) :: alignment
      logical :: is_mirror

      if (allocated(spec%state_item_variant)) then
         _RETURN_IF(spec%state_item_variant == MAPL_STATEITEM_VERTICALGRID)
      end if
      _RETURN_UNLESS(carries_field_description(spec%itemType))
      if (allocated(spec%vertical_stagger)) then
         _RETURN_IF(spec%vertical_stagger%is_none())
      end if

      is_mirror = .false.
      alignment = VALIGN_WITH_GRID
      if (allocated(spec%vertical_alignment)) then
         requested = ESMF_UtilStringLowerCase(trim(spec%vertical_alignment))
         is_mirror = (requested == 'mirror')
         if (.not. is_mirror) alignment = VerticalAlignment(requested)
         _ASSERT(alignment /= VALIGN_INVALID, 'unknown vertical_alignment: ' // spec%vertical_alignment)
      end if

      if (is_mirror) then
         call orientation_char%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else if (alignment == VALIGN_UP) then
         orientation_char = VerticalOrientationCharacteristic(VCOORD_DIRECTION_UP)
      else if (alignment == VALIGN_DOWN) then
         orientation_char = VerticalOrientationCharacteristic(VCOORD_DIRECTION_DOWN)
      else if (allocated(spec%vertical_grid)) then
         orientation_char = VerticalOrientationCharacteristic(spec%vertical_grid%get_coordinate_direction())
      else if (spec%itemType == MAPL_STATEITEM_EXPRESSION .or. spec%mirror_vgrid) then
         call orientation_char%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         call orientation_char%set_status(CHARACTERISTIC_STATUS_DEFERRED)
      end if

      call descriptor%set_characteristic(VERTICAL_ORIENTATION_CHARACTERISTIC_KIND, orientation_char, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_vertical_orientation_characteristic

   ! Every shape carries its item class, SPECIFIED, with the declared variant
   ! if any.
   subroutine make_item_shape_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(ItemShapeCharacteristic) :: item_shape

      if (allocated(spec%state_item_variant)) then
         item_shape = ItemShapeCharacteristic(spec%itemType, &
              spec%state_item_variant)
      else
         item_shape = ItemShapeCharacteristic(spec%itemType)
      end if
      call descriptor%set_characteristic(ITEM_SHAPE_CHARACTERISTIC_KIND, &
           item_shape, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_item_shape_characteristic

   ! Only FIELD and EXPRESSION carry a long_name (legacy passes it to no
   ! other class aspect). A MAPL_STRING_MIRROR request is MIRRORED (see
   ! check_intent_rules for where that is allowed). A declared long_name, even an
   ! empty one, is SPECIFIED; the field dictionary has already filled it in
   ! make_VariableSpec. Any intent but IMPORT (export, internal) that declares
   ! none takes its standard_name value; legacy has no such fallback. Anything
   ! else is UNCHECKED and writes nothing.
   subroutine make_long_name_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(LongNameCharacteristic) :: long_name
      logical :: carries, from_standard_name

      carries = spec%itemType == MAPL_STATEITEM_FIELD .or. &
           spec%itemType == MAPL_STATEITEM_EXPRESSION
      _RETURN_UNLESS(carries)

      ! A mirrored standard_name is left unallocated by make_VariableSpec.
      from_standard_name = .false.
      if (spec%state_intent /= ESMF_STATEINTENT_IMPORT .and. &
           allocated(spec%standard_name)) then
         from_standard_name = spec%standard_name /= UNKNOWN_STANDARD_NAME
      end if

      if (spec%long_name_mirror) then
         call long_name%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else if (allocated(spec%long_name)) then
         long_name = LongNameCharacteristic(spec%long_name)
      else if (from_standard_name) then
         long_name = LongNameCharacteristic(spec%standard_name)
      else
         call long_name%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
      end if

      call descriptor%set_characteristic(LONG_NAME_CHARACTERISTIC_KIND, &
           long_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_long_name_characteristic

   ! FIELD carries a restart mode (legacy passes restart_mode to
   ! FieldClassAspect only); EXPRESSION carries one too. An
   ! undeclared mode is SPECIFIED RESTART_REQUIRED and always written; the
   ! exporter's mode is never adopted.
   subroutine make_restart_mode_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_carrier
      type(RestartModeCharacteristic) :: restart_mode

      is_carrier = spec%itemType == MAPL_STATEITEM_FIELD .or. &
           spec%itemType == MAPL_STATEITEM_EXPRESSION
      _RETURN_UNLESS(is_carrier)

      if (allocated(spec%restart_mode)) then
         restart_mode = RestartModeCharacteristic(spec%restart_mode)
      else
         restart_mode = RestartModeCharacteristic(RESTART_REQUIRED)
      end if

      call descriptor%set_characteristic(RESTART_MODE_CHARACTERISTIC_KIND, &
           restart_mode, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_restart_mode_characteristic

   ! The declared quantity type is SPECIFIED; a declared QUANTITY_UNKNOWN
   ! counts as undeclared. An undeclared quantity type is a real gap: on
   ! EXPORT or INTERNAL the component may still set one on its realized item
   ! (DEFERRED, resolved at GEOM_B or, if not required, left to the
   ! connect-phase fallback); on IMPORT there is no such second chance, so
   ! it starts UNCHECKED. Never MIRRORED. Same shape set as ungridded dims.
   subroutine make_quantity_type_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(QuantityTypeCharacteristic) :: quantity_type

      _RETURN_UNLESS(has_field_metadata(spec%itemType))

      if (spec%quantity_type == QUANTITY_UNKNOWN) then
         if (spec%state_intent == ESMF_STATEINTENT_IMPORT) then
            call quantity_type%set_status(CHARACTERISTIC_STATUS_UNCHECKED)
         else
            call quantity_type%set_status(CHARACTERISTIC_STATUS_DEFERRED)
         end if
      else
         quantity_type = &
              QuantityTypeCharacteristic(quantity_type=spec%quantity_type)
      end if

      call descriptor%set_characteristic(QUANTITY_TYPE_CHARACTERISTIC_KIND, &
           quantity_type, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_quantity_type_characteristic

   ! A mirror ConservationMetadata is MIRRORED (see check_intent_rules for
   ! where that is allowed); anything else, including the
   ! CONSERVE_NONE default, is SPECIFIED with the declared value.
   subroutine make_conservation_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConservationCharacteristic) :: conservation

      _RETURN_UNLESS(has_field_metadata(spec%itemType))

      if (spec%conservation%is_mirror()) then
         call conservation%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         conservation = ConservationCharacteristic(spec%conservation)
      end if

      call descriptor%set_characteristic(CONSERVATION_CHARACTERISTIC_KIND, &
           conservation, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_conservation_characteristic

   ! A mirror NormalizationMetadata is MIRRORED (see check_intent_rules for
   ! where that is allowed); anything else, including the NORMALIZE_NONE
   ! default, is SPECIFIED with the declared value.
   subroutine make_normalization_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(NormalizationCharacteristic) :: normalization

      _RETURN_UNLESS(has_field_metadata(spec%itemType))

      if (spec%normalization%is_mirror()) then
         call normalization%set_status(CHARACTERISTIC_STATUS_MIRRORED)
      else
         normalization = NormalizationCharacteristic(spec%normalization)
      end if

      call descriptor%set_characteristic(NORMALIZATION_CHARACTERISTIC_KIND, &
           normalization, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_normalization_characteristic

   ! An EXPRESSION's expression string is SPECIFIED, always, so there is no
   ! UNCHECKED or DEFERRED case. The parser does not require an `expression:`
   ! key, so an unallocated string is stored as ''. Only EXPRESSION carries
   ! this kind.
   subroutine make_expression_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: expression

      _RETURN_UNLESS(spec%itemType == MAPL_STATEITEM_EXPRESSION)

      expression = ''
      if (allocated(spec%expression)) expression = spec%expression

      call descriptor%set_characteristic(EXPRESSION_CHARACTERISTIC_KIND, &
           ExpressionCharacteristic(expression), _RC)

      _RETURN(_SUCCESS)
   end subroutine make_expression_characteristic

   ! Shapes that carry the per-field metadata kinds (ungridded dims,
   ! attributes, quantity type, conservation, normalization): every
   ! field-carrying shape, EXPRESSION included (it declares these itself, like
   ! a FIELD). Kept as a named wrapper over carries_field_description.
   logical function has_field_metadata(item_type) result(has)
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      has = carries_field_description(item_type)
   end function has_field_metadata

   ! Shapes that carry a horizontal dims spec: field-carrying, but not
   ! EXPRESSION.
   logical function has_horizontal_dims_spec(item_type) result(has)
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      has = carries_field_description(item_type) .and. item_type /= MAPL_STATEITEM_EXPRESSION
   end function has_horizontal_dims_spec

   ! Shapes whose payload is a Field or FieldBundle (or an expression that
   ! evaluates to one) and so carry the field-level kinds.
   logical function carries_field_description(item_type) result(carries)
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      carries = item_type == MAPL_STATEITEM_FIELD .or. item_type == MAPL_STATEITEM_BRACKET .or. &
           item_type == MAPL_STATEITEM_VECTOR .or. item_type == MAPL_STATEITEM_VECTORBRACKET .or. &
           item_type == MAPL_STATEITEM_FIELDBUNDLE .or. item_type == MAPL_STATEITEM_EXPRESSION
   end function carries_field_description

end submodule make_characteristics_smod
