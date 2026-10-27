#include "MAPL.h"

submodule (mapl_VariableSpec_mod) make_characteristics_smod
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
        TYPE_KIND_CHARACTERISTIC_KIND, UNGRIDDED_DIMS_CHARACTERISTIC_KIND, GEOMETRY_CHARACTERISTIC_KIND, &
        VERTICAL_GRID_CHARACTERISTIC_KIND, STANDARD_NAME_CHARACTERISTIC_KIND, ATTRIBUTES_CHARACTERISTIC_KIND, &
        VERTICAL_STAGGER_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_UNCHECKED, CHARACTERISTIC_STATUS_MIRRORED, &
        CHARACTERISTIC_STATUS_DEFERRED
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_VERTICALGRID, operator(==)
   use mapl_AttributesCharacteristic_mod, only: AttributesCharacteristic
   use mapl_StandardNameCharacteristic_mod, only: StandardNameCharacteristic, UNKNOWN_STANDARD_NAME
   use mapl_GeometryCharacteristic_mod, only: GeometryCharacteristic
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic, UNKNOWN_UNITS
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic
   use mapl_typekind_mod, only: MAPL_TYPEKIND_MIRROR
   use mapl_UngriddedDimsCharacteristic_mod, only: UngriddedDimsCharacteristic
   use mapl_VerticalGridCharacteristic_mod, only: VerticalGridCharacteristic
   use mapl_VerticalStaggerCharacteristic_mod, only: VerticalStaggerCharacteristic
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_INVALID
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
      type(Factory) :: factories(8)

      factories(1)%make => make_units_characteristic
      factories(2)%make => make_typekind_characteristic
      factories(3)%make => make_ungridded_dims_characteristic
      factories(4)%make => make_geometry_characteristic
      factories(5)%make => make_vertical_grid_characteristic
      factories(6)%make => make_standard_name_characteristic
      factories(7)%make => make_attributes_characteristic
      factories(8)%make => make_vertical_stagger_characteristic

      do i = 1, size(factories)
         call factories(i)%make(this, descriptor, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine make_characteristics

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
   ! SPECIFIED; it is never DEFERRED. An EXPRESSION has no ungridded dims of
   ! its own, so only shapes whose payload is a Field or FieldBundle get an
   ! entry.
   subroutine make_ungridded_dims_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(UngriddedDimsCharacteristic) :: ungridded_dims

      _RETURN_UNLESS(carries_field_description(spec%itemType) .and. spec%itemType /= MAPL_STATEITEM_EXPRESSION)

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
   ! geom is SPECIFIED by identity alone. Every shape that
   ! carries a field, plus SERVICE, has the entry; STATE and WILDCARD do not.
   subroutine make_geometry_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeometryCharacteristic) :: geometry
      character(20) :: id_buffer

      _RETURN_UNLESS(carries_field_description(spec%itemType) .or. spec%itemType == MAPL_STATEITEM_SERVICE)

      if (allocated(spec%geom)) then
         if (spec%geom_id%is_assigned()) then
            write(id_buffer, '(I0)') spec%geom_id%get_value()
            geometry = GeometryCharacteristic(geom_id=trim(id_buffer), geom=spec%geom, &
                 horizontal_dims_spec=spec%horizontal_dims_spec, regridder_param=spec%regrid_param)
         else
            geometry = GeometryCharacteristic(geom=spec%geom, &
                 horizontal_dims_spec=spec%horizontal_dims_spec, regridder_param=spec%regrid_param)
         end if
      else if (spec%geom_id%is_assigned()) then
         write(id_buffer, '(I0)') spec%geom_id%get_value()
         geometry = GeometryCharacteristic(geom_id=trim(id_buffer), &
              horizontal_dims_spec=spec%horizontal_dims_spec, regridder_param=spec%regrid_param)
      else
         geometry = GeometryCharacteristic(horizontal_dims_spec=spec%horizontal_dims_spec, &
              regridder_param=spec%regrid_param)
         if (spec%itemType == MAPL_STATEITEM_EXPRESSION .or. spec%mirror_geom) then
            call geometry%set_status(CHARACTERISTIC_STATUS_MIRRORED)
         else
            call geometry%set_status(CHARACTERISTIC_STATUS_DEFERRED)
         end if
      end if

      call descriptor%set_characteristic(GEOMETRY_CHARACTERISTIC_KIND, geometry, _RC)

      _RETURN(_SUCCESS)
   end subroutine make_geometry_characteristic

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

   ! A declared standard_name is SPECIFIED. Undeclared (or UNKNOWN_STANDARD_NAME)
   ! is UNCHECKED: it matches anything. Only a FIELD carries one: the other
   ! shapes either enforce their own names (VECTOR), are exempt, or adopt the
   ! field-side value after the shape step.
   subroutine make_standard_name_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status
      type(StandardNameCharacteristic) :: standard_name
      logical :: declared

      _RETURN_UNLESS(spec%itemType == MAPL_STATEITEM_FIELD)

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
   ! dims: an EXPRESSION carries no attributes of its own.
   subroutine make_attributes_characteristic(spec, descriptor, rc)
      class(VariableSpec), intent(in) :: spec
      type(StateItemDescriptor), intent(inout) :: descriptor
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(carries_field_description(spec%itemType) .and. spec%itemType /= MAPL_STATEITEM_EXPRESSION)

      call descriptor%set_characteristic(ATTRIBUTES_CHARACTERISTIC_KIND, AttributesCharacteristic(spec%attributes), _RC)

      _RETURN(_SUCCESS)
   end subroutine make_attributes_characteristic

   ! A real stagger (CENTER, EDGE, or an explicit NONE) is SPECIFIED.
   ! VERTICAL_STAGGER_MIRROR is MIRRORED; it is an error on an export other
   ! than an expression. Undeclared (unallocated, or the
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

      if (declared .and. .not. real_stagger .and. spec%state_intent == ESMF_STATEINTENT_EXPORT) then
         _ASSERT(spec%itemType == MAPL_STATEITEM_EXPRESSION, 'VERTICAL_STAGGER_MIRROR is only valid on an import or an expression: ' // spec%short_name)
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

   ! Shapes whose payload is a Field or FieldBundle (or an expression that
   ! evaluates to one) and so carry the field-level kinds.
   logical function carries_field_description(item_type) result(carries)
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      carries = item_type == MAPL_STATEITEM_FIELD .or. item_type == MAPL_STATEITEM_BRACKET .or. &
           item_type == MAPL_STATEITEM_VECTOR .or. item_type == MAPL_STATEITEM_VECTORBRACKET .or. &
           item_type == MAPL_STATEITEM_FIELDBUNDLE .or. item_type == MAPL_STATEITEM_EXPRESSION
   end function carries_field_description

end submodule make_characteristics_smod
