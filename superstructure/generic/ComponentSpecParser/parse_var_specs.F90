#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_var_specs_smod
   use mapl_VerticalGrid_mod
   use mapl_HorizontalDimsSpec_mod
   use mapl_QuantityType_mod, only: QuantityType, QUANTITY_UNKNOWN, operator(/=)
   use mapl_CallbackInterfaceRegistry_mod, only: get_callback_interface
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   ! Renamed on import: MAPL_STATEITEM_FIELD below would otherwise clash
   ! with the ESMF_StateItem_Flag constant of the same name already in
   ! scope from mapl_StateItem_mod (see StateItemFlag.F90's own header).
   use mapl_StateItemFlag_mod, only: CALLBACK_ARGUMENT_FIELD_KIND => MAPL_STATEITEM_FIELD, operator(==)
   use gFTL2_StringStringMap, only: StringStringMap
   implicit none(type,external)

   character(*), parameter :: KEY_DIMS = 'dims'
   character(*), parameter :: KEY_QUANTITY_TYPE = 'quantity_type'
   character(*), parameter :: KEY_NON_NEGATIVE = 'non_negative'
   character(*), parameter :: KEY_NON_NEGATIVE_FILL = 'fill'
   character(*), parameter :: KEY_CALLBACK_INTERFACE = 'callback_interface'
   character(*), parameter :: KEY_CALLBACK_MEMBER_ALIAS = 'alias'
   character(*), parameter :: KEY_CALLBACK_MEMBER_UNITS = 'units'
   character(*), parameter :: KEY_CALLBACK_MEMBERS = 'members'
   character(*), parameter :: KEY_CALLBACK_PROVIDES = 'provides'
   character(*), parameter :: KEY_CALLBACK_RECEIVES = 'receives'

contains

   ! A component is not required to have var_specs.   E.g, in theory GCM gridcomp will not
   ! have var specs in MAPL3, as it does not really have a preferred geom on which to declare
   ! imports and exports.
   module subroutine parse_var_specs(hconfig, registry, &
        component_name, var_specs, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(StateRegistry), target, intent(in) :: registry
      character(*), intent(in) :: component_name
      type(VariableSpecVector), intent(out) :: var_specs
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_states_section
      type(ESMF_HConfig) :: subcfg

      has_states_section = ESMF_HConfigIsDefined(hconfig,keyString=COMPONENT_STATES_SECTION, _RC)
      _RETURN_UNLESS(has_states_section)

      subcfg = ESMF_HConfigCreateAt(hconfig,keyString=COMPONENT_STATES_SECTION, _RC)

      call parse_state_specs(var_specs, subcfg, COMPONENT_INTERNAL_STATE_SECTION, component_name, _RC)
      call parse_state_specs(var_specs, subcfg, COMPONENT_EXPORT_STATE_SECTION, component_name, _RC)
      call parse_state_specs(var_specs, subcfg, COMPONENT_IMPORT_STATE_SECTION, component_name, _RC)

      call ESMF_HConfigDestroy(subcfg, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine parse_state_specs(var_specs, hconfig, state_intent, component_name, rc)
         type(VariableSpecVector), intent(inout) :: var_specs
         type(ESMF_HConfig), target, intent(in) :: hconfig
         character(*), intent(in) :: state_intent
         character(*), intent(in) :: component_name
         integer, optional, intent(out) :: rc

         type(VariableSpec) :: var_spec
         type(ESMF_HConfigIter) :: iter,e,b
         character(:), allocatable :: name
         character(:), allocatable :: short_name
         type(ESMF_HConfig) :: attributes
         type(ESMF_TypeKind_Flag) :: typekind
         real, allocatable :: fill_value
         type(VerticalStaggerLoc) :: vertical_stagger
         character(:), allocatable :: vertical_alignment
         logical :: has_vertical_alignment
         type(UngriddedDims) :: ungridded_dims
         character(:), allocatable :: standard_name
         character(:), allocatable :: long_name
         character(:), allocatable :: units
         character(:), allocatable :: expression
         character(:), allocatable :: non_negative
         character(:), allocatable :: callback_interface
         logical :: has_callback_interface
         character(:), allocatable :: callback_unit_placeholder
         logical :: has_callback_member_alias
         logical :: has_callback_members
         logical :: is_callback_state_export
         logical :: has_callback_provides
         logical :: has_callback_receives
         logical :: is_callback_import
         type(ESMF_StateItem_Flag) :: itemtype
         type(ESMF_StateIntent_Flag) :: esmf_state_intent

         type(StringVector) :: service_items
         integer :: status
         logical :: has_state
         logical :: has_standard_name
         logical :: has_long_name
         logical :: has_units
         logical :: has_expression
         logical :: has_dims
         type(QuantityType) :: quantity_type
         logical :: mirror_geom, has_mirror_geom
         logical :: mirror_vgrid, has_mirror_vgrid
         character(:), allocatable :: dims_str
         type(HorizontalDimsSpec) :: horizontal_dims_spec
         character(:), allocatable :: msg
         type(ESMF_HConfig) :: subcfg
         type(StringVector) :: dependencies

         type(GeometrySpec) :: geometry_spec
         type(mapl_GeomId) :: geom_id
         type(mapl_MaplGeom), pointer :: mapl_geom
         type(mapl_GeomManager), pointer :: geom_mgr
         type(ESMF_Geom), allocatable :: geom
         class(VerticalGrid), allocatable :: vertical_grid

         has_state = ESMF_HConfigIsDefined(hconfig,keyString=state_intent, _RC)
         _RETURN_UNLESS(has_state)

         subcfg = ESMF_HConfigCreateAt(hconfig,keyString=state_intent, _RC)

         b = ESMF_HConfigIterBegin(subcfg, _RC)
         e = ESMF_HConfigIterEnd(subcfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter,b,e))
            name = ESMF_HConfigAsStringMapKey(iter, _RC)
            attributes = ESMF_HConfigCreateAtMapVal(iter,_RC)

            short_name = name

            geom_id = mapl_new_GeomId()
            if (allocated(geom)) deallocate(geom)
            if (allocated(vertical_grid)) deallocate(vertical_grid)

            typekind = to_typekind(attributes, _RC)
            call val_to_float(fill_value, attributes, KEY_FILL_VALUE, _RC)
            vertical_stagger = to_VerticalStaggerLoc(attributes,_RC)
            ungridded_dims = to_UngriddedDims(attributes, _RC)

            horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
            has_dims = ESMF_HConfigIsDefined(attributes, keyString=KEY_DIMS, _RC)
            if (has_dims) then
               dims_str = ESMF_HConfigAsString(attributes, keyString=KEY_DIMS, _RC)
               select case (ESMF_UtilStringLowerCase(dims_str))
               case ('z')
                  horizontal_dims_spec = HORIZONTAL_DIMS_NONE
               case ('mirror')
                  horizontal_dims_spec = HORIZONTAL_DIMS_MIRROR
               end select
            end if

            call to_quantity_type(quantity_type, attributes, _RC)

            mirror_geom = .false.
            has_mirror_geom = ESMF_HConfigIsDefined(attributes, keyString=KEY_GEOMETRY_MIRROR, _RC)
            if (has_mirror_geom) then
               mirror_geom = ESMF_HConfigAsLogical(attributes, keyString=KEY_GEOMETRY_MIRROR, _RC)
            end if

            if (allocated(vertical_alignment)) deallocate(vertical_alignment)
            has_vertical_alignment = ESMF_HConfigIsDefined(attributes, keyString=KEY_VERTICAL_ALIGNMENT, _RC)
            if (has_vertical_alignment) then
               vertical_alignment = ESMF_HConfigAsString(attributes, keyString=KEY_VERTICAL_ALIGNMENT, _RC)
            end if

            mirror_vgrid = .false.
            has_mirror_vgrid = ESMF_HConfigIsDefined(attributes, keyString=KEY_VERTICAL_GRID_MIRROR, _RC)
            if (has_mirror_vgrid) then
               mirror_vgrid = ESMF_HConfigAsLogical(attributes, keyString=KEY_VERTICAL_GRID_MIRROR, _RC)
            end if

            has_standard_name = ESMF_HConfigIsDefined(attributes,keyString='standard_name', _RC)
            if (has_standard_name) then
               standard_name = ESMF_HConfigAsString(attributes,keyString='standard_name', _RC)
            end if

            has_long_name = ESMF_HConfigIsDefined(attributes,keyString='long_name', _RC)
            if (has_long_name) then
               long_name = ESMF_HConfigAsString(attributes,keyString='long_name', _RC)
            end if

            if (allocated(callback_unit_placeholder)) deallocate(callback_unit_placeholder)
            call to_units_or_placeholder(units, callback_unit_placeholder, attributes, has_units, _RC)
            msg = 'a unit-placeholder binding ({Placeholder: ConcreteUnit}) is only valid inside a "' // &
                 KEY_CALLBACK_MEMBERS // '" entry, not a top-level state item: ' // short_name
            _ASSERT(.not. allocated(callback_unit_placeholder), msg)

            has_callback_member_alias = ESMF_HConfigIsDefined(attributes, keyString=KEY_CALLBACK_MEMBER_ALIAS, _RC)
            msg = '"' // KEY_CALLBACK_MEMBER_ALIAS // '" is only valid inside a "' // KEY_CALLBACK_MEMBERS // &
                 '" entry, not a top-level state item: ' // short_name
            _ASSERT(.not. has_callback_member_alias, msg)

            has_expression = ESMF_HConfigIsDefined(attributes,keyString='expression', _RC)
            if (has_expression) then
               expression = ESMF_HConfigAsString(attributes,keyString='expression', _RC)
            end if

            call to_non_negative(non_negative, attributes, _RC)

            has_callback_interface = ESMF_HConfigIsDefined(attributes, keyString=KEY_CALLBACK_INTERFACE, _RC)
            if (has_callback_interface) then
               callback_interface = ESMF_HConfigAsString(attributes, keyString=KEY_CALLBACK_INTERFACE, _RC)
            end if

            has_callback_members = ESMF_HConfigIsDefined(attributes, keyString=KEY_CALLBACK_MEMBERS, _RC)
            is_callback_state_export = has_callback_interface .and. (state_intent == COMPONENT_EXPORT_STATE_SECTION)
            msg = '"' // KEY_CALLBACK_MEMBERS // '" is only valid on a Handler callback-State EXPORT ' // &
                 'declaring "' // KEY_CALLBACK_INTERFACE // '": ' // short_name
            _ASSERT(.not. has_callback_members .or. is_callback_state_export, msg)

            ! `provides:`/`receives:` (Invoker callback IMPORT only, gated
            ! here; argument-name validity against the declared interface is
            ! checked once var_spec%callback_interface_id is resolved, in
            ! parse_callback_provider_map).
            has_callback_provides = ESMF_HConfigIsDefined(attributes, keyString=KEY_CALLBACK_PROVIDES, _RC)
            has_callback_receives = ESMF_HConfigIsDefined(attributes, keyString=KEY_CALLBACK_RECEIVES, _RC)
            is_callback_import = has_callback_interface .and. (state_intent == COMPONENT_IMPORT_STATE_SECTION)
            msg = '"' // KEY_CALLBACK_PROVIDES // '" is only valid on an Invoker callback IMPORT ' // &
                 'declaring "' // KEY_CALLBACK_INTERFACE // '": ' // short_name
            _ASSERT(.not. has_callback_provides .or. is_callback_import, msg)
            msg = '"' // KEY_CALLBACK_RECEIVES // '" is only valid on an Invoker callback IMPORT ' // &
                 'declaring "' // KEY_CALLBACK_INTERFACE // '": ' // short_name
            _ASSERT(.not. has_callback_receives .or. is_callback_import, msg)

            itemtype = to_itemtype(attributes, _RC)
            call to_service_items(service_items, attributes, _RC)

            dependencies = to_dependencies(attributes, _RC)

             geometry_spec = parse_geometry_spec(attributes, registry, component_name//"::"//short_name, _RC)
             geom_id = geometry_spec%geom_id
             if (allocated(geometry_spec%geom_spec)) then
                geom_mgr => mapl_get_geom_manager()
                mapl_geom => geom_mgr%get_mapl_geom(geometry_spec%geom_spec, _RC)
               geom = mapl_geom%get_geom()
            end if
            if (allocated(geometry_spec%vertical_grid)) then
               vertical_grid = geometry_spec%vertical_grid
            end if


            esmf_state_intent = to_esmf_state_intent(state_intent)
            var_spec = make_VariableSpec(esmf_state_intent, short_name=short_name, &
                 units=units, &
                 itemtype=itemtype, &
                 typekind=typekind, &
                 vertical_stagger=vertical_stagger, &
                 vertical_alignment=vertical_alignment, &
                 ungridded_dims=ungridded_dims, &
                 fill_value=fill_value, &
                 service_items=service_items, &
                 standard_name=standard_name, &
                 long_name=long_name, &
                 dependencies=dependencies, &
                  expression=expression, &
                  geom_id=geom_id, &
                  geom=geom, &
                 vertical_grid=vertical_grid, &
                 horizontal_dims_spec=horizontal_dims_spec, &
                 quantity_type=quantity_type, &
                 mirror_geom=mirror_geom, &
                 mirror_vgrid=mirror_vgrid, &
                 non_negative=non_negative, &
                 callback_interface=callback_interface, _RC)

            if (has_callback_members) call parse_callback_members(var_spec, attributes, _RC)
            if (has_callback_provides) then
               call parse_callback_provider_map(var_spec, attributes, KEY_CALLBACK_PROVIDES, callback_interface, var_spec%callback_provides, _RC)
            end if
            if (has_callback_receives) then
               call parse_callback_provider_map(var_spec, attributes, KEY_CALLBACK_RECEIVES, callback_interface, var_spec%callback_receives, _RC)
            end if

            if (allocated(units)) deallocate(units)
            if (allocated(standard_name)) deallocate(standard_name)
            if (allocated(long_name)) deallocate(long_name)
            if (allocated(callback_interface)) deallocate(callback_interface)
            call var_specs%push_back(var_spec)

            call ESMF_HConfigDestroy(attributes, _RC)

         end do

         call ESMF_HConfigDestroy(subcfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine parse_state_specs

      subroutine val_to_float(x, attributes, key, rc)
         real, allocatable, intent(out) :: x
         type(ESMF_HConfig), intent(in) :: attributes
         character(*), intent(in) :: key
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_fill_value

         has_fill_value = ESMF_HConfigIsDefined(attributes, keyString=key, _RC)
         _RETURN_UNLESS(has_fill_value)

         allocate(x)
         x = ESMF_HConfigAsR4(attributes, keyString=KEY_FILL_VALUE, _RC)

         _RETURN(_SUCCESS)
      end subroutine val_to_float

      ! units: a plain scalar string is an ordinary concrete unit
      ! (unchanged behavior, placeholder stays unallocated). A one-entry
      ! map ({Placeholder: ConcreteUnit}, e.g. {U: 'kg kg-1'}) binds the
      ! owning CallbackInterface's named unit placeholder to a concrete
      ! value for this Handler member: units gets the concrete value and
      ! placeholder gets the bare placeholder name (no leading '$').
      ! has_units reports whether the key was present at all, either shape.
      subroutine to_units_or_placeholder(units, placeholder, attributes, has_units, rc)
         character(:), allocatable, intent(out) :: units
         character(:), allocatable, intent(out) :: placeholder
         type(ESMF_HConfig), intent(in) :: attributes
         logical, intent(out) :: has_units
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: subcfg
         type(ESMF_HConfigIter) :: iter, b, e
         logical :: looped, is_map
         integer :: num_keys

         has_units = ESMF_HConfigIsDefined(attributes, keyString='units', _RC)
         _RETURN_UNLESS(has_units)

         subcfg = ESMF_HConfigCreateAt(attributes, keyString='units', _RC)
         is_map = ESMF_HConfigIsMap(subcfg, _RC)
         if (is_map) then
            num_keys = ESMF_HConfigGetSize(subcfg, _RC)
            _ASSERT(num_keys == 1, 'units map must have exactly one entry, e.g. {U: "kg kg-1"}')
            b = ESMF_HConfigIterBegin(subcfg, _RC)
            e = ESMF_HConfigIterEnd(subcfg, _RC)
            iter = b
            looped = ESMF_HConfigIterLoop(iter, b, e, _RC)
            _ASSERT(looped, 'units map must have exactly one entry, e.g. {U: "kg kg-1"}')
            placeholder = ESMF_HConfigAsStringMapKey(iter, _RC)
            units = ESMF_HConfigAsStringMapVal(iter, _RC)
         else
            units = ESMF_HConfigAsString(subcfg, _RC)
         end if
         call ESMF_HConfigDestroy(subcfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine to_units_or_placeholder

      ! `members:` (Handler callback-State EXPORT only, gated by the caller):
      ! a map of member name -> optional map of alias:, units:, dims:,
      ! vertical_dim_spec:, vertical_grid:. alias/units are the two keys
      ! VariableSpec%declare_member's own member argument takes; dims/
      ! vertical_dim_spec/vertical_grid are parsed with the same helpers a
      ! top-level item uses. Any other key is rejected immediately rather
      ! than failing later with a confusing downstream error. A member is
      ! realized directly from its own characteristics, never through the
      ! IMPORT/EXPORT connection-resolution graph, so there is no partner
      ! for it to mirror from ("mirror" is rejected on both dims and
      ! vertical_dim_spec) and no realized field a harvest pass could ever
      ! read a DEFERRED stagger back from: an entry with no vertical_dim_spec
      ! defaults explicitly to NONE, or to CENTER when it names its own
      ! vertical_grid, rather than falling through to DEFERRED. A member
      ! name that matches a declared interface argument must answer a
      ! FIELD-kind argument. One declare_member call per entry.
      subroutine parse_callback_members(var_spec, attributes, rc)
         type(VariableSpec), intent(inout) :: var_spec
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: members_cfg, member_cfg
         type(ESMF_HConfig) :: member_vertical_grid_cfg
         type(ESMF_HConfigIter) :: iter, b, e
         type(ESMF_HConfigIter) :: key_iter, key_b, key_e
         character(:), allocatable :: member_name
         character(:), allocatable :: member_alias
         character(:), allocatable :: member_units
         character(:), allocatable :: member_placeholder
         character(:), allocatable :: key_name
         character(:), allocatable :: dims_str
         logical :: has_alias, has_units, is_known_key, is_map
         logical :: has_dims, has_vertical_dim_spec, has_member_vertical_grid
         character(:), allocatable :: msg
         type(CallbackInterface) :: iface
         type(CallbackArgumentSpec) :: arg_spec
         type(VariableSpec) :: member_var_spec
         type(VerticalStaggerLoc) :: member_vertical_stagger
         type(HorizontalDimsSpec) :: member_horizontal_dims_spec
         class(VerticalGrid), allocatable :: member_vertical_grid

         iface = get_callback_interface(var_spec%callback_interface_id, _RC)

         members_cfg = ESMF_HConfigCreateAt(attributes, keyString=KEY_CALLBACK_MEMBERS, _RC)
         b = ESMF_HConfigIterBegin(members_cfg, _RC)
         e = ESMF_HConfigIterEnd(members_cfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            member_name = ESMF_HConfigAsStringMapKey(iter, _RC)
            member_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)

            is_map = ESMF_HConfigIsMap(member_cfg, _RC)
            msg = '"' // KEY_CALLBACK_MEMBERS // '" entry "' // member_name // &
                 '" must be a map (use "{}" for a plain member with no "' // KEY_CALLBACK_MEMBER_ALIAS // &
                 '"/"' // KEY_CALLBACK_MEMBER_UNITS // '"), not a bare key'
            _ASSERT(is_map, msg)

            key_b = ESMF_HConfigIterBegin(member_cfg, _RC)
            key_e = ESMF_HConfigIterEnd(member_cfg, _RC)
            key_iter = key_b
            do while (ESMF_HConfigIterLoop(key_iter, key_b, key_e))
               key_name = ESMF_HConfigAsStringMapKey(key_iter, _RC)
               is_known_key = (key_name == KEY_CALLBACK_MEMBER_ALIAS) .or. (key_name == KEY_CALLBACK_MEMBER_UNITS) &
                    .or. (key_name == KEY_DIMS) .or. (key_name == KEY_VERTICAL_STAGGER) &
                    .or. (key_name == COMPONENT_VERTICAL_GRID_SECTION)
               msg = '"' // KEY_CALLBACK_MEMBERS // '" entry "' // member_name // '" has unsupported key "' // &
                    key_name // '": a YAML member supports only "' // KEY_CALLBACK_MEMBER_ALIAS // '", "' // &
                    KEY_CALLBACK_MEMBER_UNITS // '", "' // KEY_DIMS // '", "' // KEY_VERTICAL_STAGGER // &
                    '", and "' // COMPONENT_VERTICAL_GRID_SECTION // '"'
               _ASSERT(is_known_key, msg)
            end do

            if (iface%is_argument(member_name)) then
               arg_spec = iface%get_argument(member_name, _RC)
               msg = '"' // KEY_CALLBACK_MEMBERS // '" entry "' // member_name // '" answers a non-FIELD-kind ' // &
                    'interface argument; a YAML member can only answer a FIELD-kind argument: ' // var_spec%short_name
               _ASSERT(arg_spec%get_expected_kind() == CALLBACK_ARGUMENT_FIELD_KIND, msg)
            end if

            if (allocated(member_alias)) deallocate(member_alias)
            has_alias = ESMF_HConfigIsDefined(member_cfg, keyString=KEY_CALLBACK_MEMBER_ALIAS, _RC)
            if (has_alias) then
               member_alias = ESMF_HConfigAsString(member_cfg, keyString=KEY_CALLBACK_MEMBER_ALIAS, _RC)
            end if

            if (allocated(member_units)) deallocate(member_units)
            if (allocated(member_placeholder)) deallocate(member_placeholder)
            call to_units_or_placeholder(member_units, member_placeholder, member_cfg, has_units, _RC)

            member_horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
            has_dims = ESMF_HConfigIsDefined(member_cfg, keyString=KEY_DIMS, _RC)
            if (has_dims) then
               dims_str = ESMF_HConfigAsString(member_cfg, keyString=KEY_DIMS, _RC)
               dims_str = ESMF_UtilStringLowerCase(dims_str)
               msg = '"' // KEY_CALLBACK_MEMBERS // '" entry "' // member_name // '" cannot use "' // KEY_DIMS // &
                    ': mirror": a member is never part of the IMPORT/EXPORT connection-resolution graph, ' // &
                    'so there is no partner to mirror from'
               _ASSERT(dims_str /= 'mirror', msg)
               select case (dims_str)
               case ('z')
                  member_horizontal_dims_spec = HORIZONTAL_DIMS_NONE
               end select
            end if

            has_vertical_dim_spec = ESMF_HConfigIsDefined(member_cfg, keyString=KEY_VERTICAL_STAGGER, _RC)
            member_vertical_stagger = to_VerticalStaggerLoc(member_cfg, _RC)
            msg = '"' // KEY_CALLBACK_MEMBERS // '" entry "' // member_name // '" cannot use "' // &
                 KEY_VERTICAL_STAGGER // ': mirror": a member is never part of the IMPORT/EXPORT ' // &
                 'connection-resolution graph, so there is no partner to mirror from'
            _ASSERT(.not. member_vertical_stagger%is_mirror(), msg)

            if (allocated(member_vertical_grid)) deallocate(member_vertical_grid)
            has_member_vertical_grid = ESMF_HConfigIsDefined(member_cfg, keyString=COMPONENT_VERTICAL_GRID_SECTION, _RC)
            if (has_member_vertical_grid) then
               member_vertical_grid_cfg = ESMF_HConfigCreateAt(member_cfg, keyString=COMPONENT_VERTICAL_GRID_SECTION, _RC)
               call parse_vertical_grid(member_vertical_grid_cfg, registry, member_vertical_grid, _RC)
               call ESMF_HConfigDestroy(member_vertical_grid_cfg, _RC)
            end if

            ! A 2D field (NONE) carrying a vertical_grid is inconsistent:
            ! the realized field stays 2D (the vertical characteristic's
            ! own builder drops a vertical_grid whenever the stagger is
            ! NONE) but the conformance-check dims helper would still
            ! report 'xyz' for it, since it checks vertical_grid before
            ! stagger.
            msg = '"' // KEY_CALLBACK_MEMBERS // '" entry "' // member_name // '" cannot combine "' // &
                 KEY_VERTICAL_STAGGER // ': none" with "' // COMPONENT_VERTICAL_GRID_SECTION // &
                 '": a 2D field cannot also carry a vertical grid'
            _ASSERT(.not. (has_vertical_dim_spec .and. member_vertical_stagger%is_none() .and. has_member_vertical_grid), msg)

            ! An omitted vertical_dim_spec on a top-level item defers to a
            ! realized field a harvest pass reads back from; a member has
            ! no such field, so it cannot use DEFERRED and instead defaults
            ! explicitly here: NONE with no vertical_grid, or CENTER when
            ! it names its own vertical_grid.
            if (.not. has_vertical_dim_spec) then
               if (has_member_vertical_grid) then
                  member_vertical_stagger = VERTICAL_STAGGER_CENTER
               else
                  member_vertical_stagger = VERTICAL_STAGGER_NONE
               end if
            end if

            member_var_spec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name=member_name, &
                 itemtype=MAPL_STATEITEM_FIELD, vertical_stagger=member_vertical_stagger, &
                 vertical_grid=member_vertical_grid, horizontal_dims_spec=member_horizontal_dims_spec, &
                 units=member_units, _RC)
            if (has_alias) member_var_spec%callback_member_alias = member_alias
            if (allocated(member_placeholder)) member_var_spec%callback_unit_placeholder = member_placeholder

            call var_spec%declare_member(member_name, member_var_spec, _RC)

            call ESMF_HConfigDestroy(member_cfg, _RC)
         end do

         call ESMF_HConfigDestroy(members_cfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine parse_callback_members

      ! `provides:`/`receives:` (Invoker callback IMPORT only, gated by the
      ! caller): a map of interface argument name -> this Invoker's own
      ! item short name. Each key must name an argument the interface
      ! actually declares; the item name itself is resolved later, against
      ! the Invoker's own items, by GraphBuilder (any name is accepted
      ! here, the same "parser checks what it can see, GraphBuilder checks
      ! the rest" split as callback_interface's own registry lookup).
      subroutine parse_callback_provider_map(var_spec, attributes, key, callback_interface_name, target_map, rc)
         type(VariableSpec), intent(in) :: var_spec
         type(ESMF_HConfig), intent(in) :: attributes
         character(*), intent(in) :: key
         character(*), intent(in) :: callback_interface_name
         type(StringStringMap), intent(out) :: target_map
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: is_map
         type(ESMF_HConfig) :: subcfg
         type(ESMF_HConfigIter) :: iter, b, e
         character(:), allocatable :: argument_name
         character(:), allocatable :: item_name
         character(:), allocatable :: msg
         type(CallbackInterface) :: iface

         subcfg = ESMF_HConfigCreateAt(attributes, keyString=key, _RC)
         is_map = ESMF_HConfigIsMap(subcfg, _RC)
         msg = '"' // key // '" must be a map of argument name -> item short name: ' // var_spec%short_name
         _ASSERT(is_map, msg)

         iface = get_callback_interface(var_spec%callback_interface_id, _RC)

         b = ESMF_HConfigIterBegin(subcfg, _RC)
         e = ESMF_HConfigIterEnd(subcfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            argument_name = ESMF_HConfigAsStringMapKey(iter, _RC)
            item_name = ESMF_HConfigAsStringMapVal(iter, _RC)

            msg = '"' // key // '" entry names argument "' // argument_name // &
                 '" not declared on interface "' // callback_interface_name // '": ' // var_spec%short_name
            _ASSERT(iface%is_argument(argument_name), msg)

            call target_map%insert(argument_name, item_name)
         end do

         call ESMF_HConfigDestroy(subcfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine parse_callback_provider_map

      ! non_negative: {fill: column}. Unallocated when the key is absent. The
      ! map must hold exactly one key, fill, with a scalar value.
      subroutine to_non_negative(non_negative, attributes, rc)
         character(:), allocatable, intent(out) :: non_negative
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_key, valid
         integer :: num_keys
         type(ESMF_HConfig) :: subcfg

         has_key = ESMF_HConfigIsDefined(attributes, keyString=KEY_NON_NEGATIVE, _RC)
         _RETURN_UNLESS(has_key)

         subcfg = ESMF_HConfigCreateAt(attributes, keyString=KEY_NON_NEGATIVE, _RC)
         valid = ESMF_HConfigIsMap(subcfg, _RC)
         if (valid) then
            num_keys = ESMF_HConfigGetSize(subcfg, _RC)
            valid = num_keys == 1
         end if
         if (valid) then
            valid = ESMF_HConfigIsDefined(subcfg, keyString=KEY_NON_NEGATIVE_FILL, _RC)
         end if
         if (valid) then
            valid = ESMF_HConfigIsScalar(subcfg, keyString=KEY_NON_NEGATIVE_FILL, _RC)
         end if
         if (valid) then
            non_negative = ESMF_HConfigAsString(subcfg, keyString=KEY_NON_NEGATIVE_FILL, _RC)
         end if
         call ESMF_HConfigDestroy(subcfg, _RC)
         _ASSERT(valid, 'non_negative must be a map with only a scalar fill key, e.g. {fill: column}')

         _RETURN(_SUCCESS)
      end subroutine to_non_negative

      ! The value is a full QuantityType name, case-folded. QuantityType(name)
      ! maps an unknown string to QUANTITY_UNKNOWN, so the check is here.
      subroutine to_quantity_type(quantity_type, attributes, rc)
         type(QuantityType), intent(out) :: quantity_type
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_quantity_type
         character(:), allocatable :: written, name, msg

         quantity_type = QUANTITY_UNKNOWN
         has_quantity_type = ESMF_HConfigIsDefined(attributes, keyString=KEY_QUANTITY_TYPE, _RC)
         _RETURN_UNLESS(has_quantity_type)

         written = ESMF_HConfigAsString(attributes, keyString=KEY_QUANTITY_TYPE, _RC)
         name = ESMF_UtilStringUpperCase(written)
         quantity_type = QuantityType(name)
         msg = 'Unsupported quantity_type: <' // written // '>'
         _ASSERT(quantity_type /= QUANTITY_UNKNOWN .or. name == 'QUANTITY_UNKNOWN', msg)

         _RETURN(_SUCCESS)
      end subroutine to_quantity_type

      function to_typekind(attributes, rc) result(typekind)
         use :: mapl_typekind_mod, only: MAPL_TYPEKIND_MIRROR
         type(ESMF_TypeKind_Flag) :: typekind
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: typekind_is_specified
         character(:), allocatable :: typekind_str

         typekind = ESMF_TYPEKIND_R4 ! GEOS defaults

         typekind_is_specified = ESMF_HConfigIsDefined(attributes, keyString='typekind', _RC)
         _RETURN_UNLESS(typekind_is_specified)

         typekind_str= ESMF_HConfigAsString(attributes,keyString='typekind',_RC)
         select case (ESMF_UtilStringLowerCase(typekind_str))
         case ('r4')
            typekind = ESMF_TYPEKIND_R4
         case ('r8')
            typekind = ESMF_TYPEKIND_R8
         case ('i4')
            typekind = ESMF_TYPEKIND_I4
         case ('i8')
            typekind = ESMF_TYPEKIND_I8
         case ('mirror')
            typekind = MAPL_TYPEKIND_MIRROR
         case default
            _FAIL('Unsupported typekind: <'//typekind_str//'>')
         end select

         _RETURN(_SUCCESS)
      end function to_typekind

      function to_VerticalStaggerLoc(attributes, rc) result(vertical_stagger)
         type(VerticalStaggerLoc) :: vertical_stagger
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: vertical_str
         logical :: has_vertical_stagger

         vertical_stagger = VERTICAL_STAGGER_INVALID
         has_vertical_stagger = ESMF_HConfigIsDefined(attributes,keyString=KEY_VERTICAL_STAGGER, _RC)
         _RETURN_UNLESS(has_vertical_stagger)

         vertical_str = ESMF_HConfigAsString(attributes,keyString=KEY_VERTICAL_STAGGER,_RC)

         select case (ESMF_UtilStringLowerCase(vertical_str))
         case ('vertical_dim_none', 'n', 'none')
            vertical_stagger = VERTICAL_STAGGER_NONE
         case ('vertical_dim_center', 'c', 'center')
            vertical_stagger = VERTICAL_STAGGER_CENTER
         case ('vertical_dim_edge', 'e', 'edge')
            vertical_stagger = VERTICAL_STAGGER_EDGE
         case ('vertical_dim_mirror', 'm', 'mirror')
            vertical_stagger = VERTICAL_STAGGER_MIRROR
         case default
            _FAIL('Unsupported vertical_stagger')
         end select

         _RETURN(_SUCCESS)
      end function to_VerticalStaggerLoc

      function to_UngriddedDims(attributes,rc) result(ungridded_dims)
         type(UngriddedDims) :: ungridded_dims
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: dim_specs, dim_spec
         character(len=:), allocatable :: dim_name, dim_units
         real, allocatable :: coordinates(:)
         integer :: dim_size,i
         type(UngriddedDim) :: temp_dim

         logical :: has_ungridded_dims, has_name, has_units, has_extent, has_coordinates
         integer :: n_specs

         has_ungridded_dims = ESMF_HConfigIsDefined(attributes, keyString=KEY_UNGRIDDED_DIMS, _RC)
         _RETURN_UNLESS(has_ungridded_dims)

         dim_specs = ESMF_HConfigCreateAt(attributes, keyString=KEY_UNGRIDDED_DIMS, _RC)

         n_specs = ESMF_HConfigGetSize(dim_specs, _RC)
         do i = 1, n_specs
            dim_spec = ESMF_HConfigCreateAt(dim_specs, index=i, _RC)
            has_name = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_NAME)
            has_units = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_UNITS)
            has_extent = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_EXTENT)
            has_coordinates = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_COORDINATES)
            _ASSERT(.not.(has_extent .and. has_coordinates), "Both extent and coordinates specified")
            if (has_name) then
               dim_name = ESMF_HConfigAsString(dim_spec, keyString=KEY_UNGRIDDED_DIM_NAME, _RC)
            end if
            if (has_units) then
               dim_units = ESMF_HConfigAsString(dim_spec, keyString=KEY_UNGRIDDED_DIM_UNITS, _RC)
            end if
            if (has_extent) then
               dim_size = ESMF_HConfigAsI4(dim_spec, keyString=KEY_UNGRIDDED_DIM_EXTENT, _RC)
               temp_dim = UngriddedDim(dim_size, name=dim_name, units=dim_units)
            end if
            if (has_coordinates) then
               coordinates = ESMF_HConfigAsR4Seq(dim_spec, keyString=KEY_UNGRIDDED_DIM_COORDINATES, _RC)
               temp_dim = UngriddedDim(coordinates, name=dim_name, units=dim_units)
            end if
            call ungridded_dims%add_dim(temp_dim, _RC)
            call ESMF_HConfigDestroy(dim_spec, _RC)
         end do

         call ESMF_HConfigDestroy(dim_specs, _RC)

         _RETURN(_SUCCESS)
      end function to_UngriddedDims


     subroutine to_service_items(service_items, attributes, rc)
         type(StringVector), intent(out) :: service_items
         type(ESMF_HConfig), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: seq
         integer :: num_items, i
         character(:), allocatable :: item_name
         logical :: has_service_items

         has_service_items = ESMF_HConfigIsDefined(attributes,keyString='items',_RC)
         _RETURN_UNLESS(has_service_items)

         seq = ESMF_HConfigCreateAt(attributes,keyString='items',_RC)
         _ASSERT(ESMF_HConfigIsSequence(seq),"items must be a sequence")
         num_items = ESMF_HConfigGetSize(seq,_RC)
         do i = 1,num_items
            item_name = ESMF_HConfigAsString(seq,index = i, _RC)
            call service_items%push_back(item_name)
         end do

         _RETURN(_SUCCESS)
      end subroutine to_service_items

      function to_dependencies(attributes, rc) result(dependencies)
         type(StringVector) :: dependencies
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_dependencies
         type(ESMF_HConfig) :: dependencies_hconfig
         integer :: i, n_dependencies
         character(:), allocatable :: name

         dependencies = StringVector()
         has_dependencies = ESMF_HConfigIsDefined(attributes, keyString='dependencies', _RC)
         _RETURN_UNLESS(has_dependencies)

         dependencies_hconfig = ESMF_HConfigCreateAt(attributes, keyString='dependencies', _RC)
         _ASSERT(ESMF_HConfigIsSequence(dependencies_hconfig), 'expected sequence for attribute <dependencies>')
         n_dependencies = ESMF_HConfigGetSize(dependencies_hconfig, _RC)

         do i = 1, n_dependencies
            name = ESMF_HConfigAsString(dependencies_hconfig, index=i, _RC)
            call dependencies%push_back(name)
         end do

         _RETURN(_SUCCESS)
      end function to_dependencies

   end subroutine parse_var_specs

end submodule parse_var_specs_smod
