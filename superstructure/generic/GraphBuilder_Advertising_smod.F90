#include "MAPL.h"

! Advertising: one StateItemNode per declared import/export/internal item.

submodule (mapl_GraphBuilder_mod) GraphBuilder_Advertising_smod
   use esmf, only: operator(==), operator(/=)
   use mapl_CompositeStateMaterialization_mod, only: materialize_composite
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: &
        QUANTITY_TYPE_CHARACTERISTIC_KIND, &
        CONSERVATION_CHARACTERISTIC_KIND, NORMALIZATION_CHARACTERISTIC_KIND, &
        ITEM_SHAPE_CHARACTERISTIC_KIND
   use mapl_StateItemFlag_mod, only: operator(==)
   use mapl_QuantityTypeCharacteristic_mod, only: QuantityTypeCharacteristic
   use mapl_ConservationCharacteristic_mod, only: ConservationCharacteristic
   use mapl_NormalizationCharacteristic_mod, only: NormalizationCharacteristic
   use mapl_ItemShapeCharacteristic_mod, only: ItemShapeCharacteristic
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! REQ-GB-003: one StateItemNode per advertised import/export/internal
   ! item. Imports and exports are also recorded as public ports (any
   ! sibling's connect_all can reach them by name); internal items are
   ! component-private.
   module subroutine graphbuilder_advertise(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      comp_spec => this%get_component_spec()
      graph => this%get_component_graph()

      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            call advertise_one(graph, var_spec, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_advertise

   module subroutine advertise_one(graph, var_spec, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: key
      type(NodeId), pointer :: existing
      type(NodeId) :: id
      type(StateItemNode) :: node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id
      type(StringVector) :: member_names
      class(GraphNode), pointer :: registered_node

      key = item_key(var_spec%state_intent, var_spec%short_name)

      ! Idempotent: an already-indexed identity already has its node.
      existing => graph%get_resource_index(key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      ! A VariableSpec with declared members is a composite: build its
      ! node tree instead of the flat node below. Identity key, resource
      ! index and port registration apply uniformly to both cases.
      member_names = var_spec%get_member_names()
      if (member_names%size() > 0) then
         id = materialize_composite(graph, var_spec, _RC)
      else
         id = graph%next_node_id(_RC)

         ! payload/revision stay default-initialized: the item is
         ! advertised, not yet realized. Identity fields are set once
         ! here so later resolution steps read them off the node.
         node = StateItemNode(id, payload, revision, short_name=var_spec%short_name, &
              state_intent=var_spec%state_intent, is_inout_borrower=var_spec%is_inout_borrower, &
              callback_interface_id=var_spec%callback_interface_id)
         call graph%register_node(node, _RC)
      end if
      call graph%add_resource_index(key, id, _RC)

      ! Populate the node's characteristics once at advertise time, for
      ! both branches above (a composite VERTICALGRID item needs its
      ! vertical-coordinate entry too, and materialize_composite cannot
      ! derive it without the var_spec). Also (re)sets the identity
      ! fields: materialize_composite builds its top-level node without
      ! them, so a composite import/export would otherwise be invisible
      ! to for_each_matching_import. Harmless repeat for the flat branch.
      registered_node => graph%get_node(id)
      _ASSERT(associated(registered_node), 'GraphBuilder: advertise_one - newly-registered node not found')
      select type (registered_node)
      class is (StateItemNode)
         call registered_node%set_short_name(var_spec%short_name)
         call registered_node%set_state_intent(var_spec%state_intent)
         call registered_node%set_is_inout_borrower(var_spec%is_inout_borrower)
         call registered_node%set_callback_interface_id(var_spec%callback_interface_id)
         call populate_characteristics(registered_node, var_spec, _RC)
      class default
         _FAIL('GraphBuilder: advertise_one - newly-registered node is not a StateItemNode')
      end select

      if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_import_port(port_id, id, _RC)
      else if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_export_port(port_id, id, _RC)
      end if
      ! INTERNAL items get no port: not visible outside the component.

      _RETURN(_SUCCESS)
   end subroutine advertise_one

   ! Maps the var_spec's declared fields into the node's stored
   ! StateItemDescriptor.
   subroutine populate_characteristics(node, var_spec, rc)
      class(StateItemNode), target, intent(inout) :: node
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: descriptor

      ! Mutate the stored descriptor in place through a pointer, not a copy:
      ! copying a populated characteristics map can corrupt nondeterministically
      ! under gfortran -fcheck=all.
      descriptor => node%get_descriptor()

      call var_spec%make_characteristics(descriptor, _RC)

      ! QuantityType/Conservation/Normalization: unconditional; each
      ! default is a valid comparable value.
      call descriptor%set_characteristic(QUANTITY_TYPE_CHARACTERISTIC_KIND, &
           QuantityTypeCharacteristic(quantity_type=var_spec%quantity_type), _RC)
      call descriptor%set_characteristic(CONSERVATION_CHARACTERISTIC_KIND, &
           ConservationCharacteristic(var_spec%conservation), _RC)
      call descriptor%set_characteristic(NORMALIZATION_CHARACTERISTIC_KIND, &
           NormalizationCharacteristic(var_spec%normalization), _RC)

      ! Capture the item type so extension materialization needs no
      ! VariableSpec. The component-wide geom and vertical grid are not
      ! captured: GEOM_B runs after ADVERTISE, so they arrive through
      ! DEFERRED resolution.
      call descriptor%set_declared_item_type(var_spec%itemType)
      ! ItemShapeCharacteristic deliberately duplicates the item type just
      ! captured above. Inserted only for shape-comparable types
      ! (FIELD/BRACKET/VECTOR/VECTORBRACKET): other types (EXPRESSION,
      ! WILDCARD, SERVICE, STATE, FIELDBUNDLE, ...) resolve through
      ! separate mechanisms, and inserting it for them would report a
      ! spurious shape mismatch (e.g. EXPRESSION export to FIELD import)
      ! that build_transform fails.
      if (var_spec%itemType == MAPL_STATEITEM_FIELD .or. var_spec%itemType == MAPL_STATEITEM_BRACKET &
           .or. var_spec%itemType == MAPL_STATEITEM_VECTOR .or. var_spec%itemType == MAPL_STATEITEM_VECTORBRACKET) then
         call descriptor%set_characteristic(ITEM_SHAPE_CHARACTERISTIC_KIND, &
              ItemShapeCharacteristic(var_spec%itemType), _RC)
      end if

      ! Captured at advertise time; populate_one reads it back when it
      ! creates the MAPL_NamedAlias.
      if (allocated(var_spec%restart_mode)) call descriptor%set_declared_restart_mode(var_spec%restart_mode)

      ! Captured at advertise time; graphbuilder_run_expression_hook reads
      ! it back.
      if (var_spec%itemType == MAPL_STATEITEM_EXPRESSION .and. allocated(var_spec%expression)) then
         call descriptor%set_declared_expression(var_spec%expression)
      end if

      _RETURN(_SUCCESS)
   end subroutine populate_characteristics

end submodule GraphBuilder_Advertising_smod
