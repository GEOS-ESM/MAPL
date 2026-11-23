#include "MAPL.h"

!------------------------------------------------------------------------------
! CompositeStateMaterialization: recursive walk that turns a VariableSpec
! with declared members (specs/VariableSpec.F90) into graph structure: one
! StateItemNode per tree node (leaves and nested-state levels alike), with
! each nested level's GraphStateItem state_members map
! populated with its declared children's NodeIds. A separate module that
! GraphBuilder calls into, like ExtensionResolution/ExtensionMaterialization.
!
! A "leaf" is a VariableSpec with no declared members; it gets the same
! unallocated "advertised, not yet realized" GraphStateItem payload that
! advertise_one gives flat items. A "nested" member (has declared members)
! gets a real, structurally empty ESMF_State, enough to satisfy
! GraphStateItem's esmf_kind()==STATE gate on state_members; it does not
! realize the state's ESMF membership. The role is read from
! get_member_names(); declare_member already enforces
! itemType==MAPL_STATEITEM_STATE at declaration time.
!------------------------------------------------------------------------------
module mapl_CompositeStateMaterialization_mod
   use ESMF, only: ESMF_State, ESMF_StateCreate
   use gFTL2_StringVector, only: StringVector
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap, &
        CallbackArgumentSpecMapIterator, operator(/=)
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceRegistry_mod, only: get_callback_interface
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
   use mapl_StateItemKeyPrefix_mod, only: INTERNAL_ITEM_KEY_PREFIX
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_VariableSpec_mod, only: VariableSpec
   use mapl_state_api, only: MAPL_StateSet
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic
   use mapl_CharacteristicStatus_mod, only: operator(==), CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: materialize_composite
   public :: materialize_member

contains

   ! Builds a StateItemNode for var_spec's top-level identity, recursing into
   ! every declared member first so the node's GraphStateItem is fully populated
   ! (state_members complete) when registered, with no fetch-and-mutate after
   ! registration.
   recursive function materialize_composite(graph, var_spec, rc) result(node_id)
      class(ComponentGraph), target, intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      type(ESMF_State) :: state
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(StateItemNode) :: node
      type(StringVector) :: names
      integer :: i
      character(:), allocatable :: name
      type(VariableSpec) :: member
      type(NodeId) :: child_id

      state = ESMF_StateCreate(_RC)
      ! A composite tagged via VariableSpec%state_item_variant (e.g.
      ! MAPL_STATEITEM_VERTICALGRID) gets that variant attached to its
      ! ESMF_State before wrapping; untagged composites keep reporting the plain
      ! STATE variant.
      if (allocated(var_spec%state_item_variant)) then
         call MAPL_StateSet(state, state_item_variant=var_spec%state_item_variant, _RC)
      end if
      call payload%set(state, _RC)

      names = var_spec%get_member_names()
      do i = 1, names%size()
         name = names%of(i)
         member = var_spec%get_member(name, _RC)
         child_id = materialize_member(graph, member, _RC)
         call payload%add_state_member(name, child_id, _RC)
      end do

      node_id = graph%next_node_id(_RC)
      node = StateItemNode(node_id, payload, revision)
      call graph%register_node(node, _RC)

      ! Only the top-level Handler composite declares a valid
      ! callback_interface_id (a nested sub-composite's own id stays
      ! unset), so no extra gating is needed for the recursive call
      ! above.
      if (var_spec%callback_interface_id%is_valid()) then
         call check_unit_placeholders(graph, node_id, var_spec, _RC)
      end if

      _RETURN(_SUCCESS)
   end function materialize_composite

   ! Confirms a Handler composite's declared members satisfy every unit
   ! its CallbackInterface declares. A placeholder argument
   ! (CallbackArgumentSpec's leading '$' convention) requires each
   ! non-alias member bound to it to declare a matching
   ! callback_unit_placeholder, and two arguments sharing one
   ! placeholder name must agree on the bound concrete unit. A fixed
   ! (non-placeholder) argument unit is bound onto the member's own
   ! PHYSICAL_UNITS_CHARACTERISTIC_KIND characteristic
   ! (bind_concrete_units). An alias member is skipped in both cases -
   ! its unit comes from the aliased INTERNAL item, no binding needed. A
   ! missing member is left to export_conforms_to_interface
   ! (GraphBuilder_CallbackWiring_smod.F90), which already reports a
   ! missing-argument violation at match time; this check is
   ! declaration-only and independent of whether any Invoker ever
   ! matches this Handler.
   subroutine check_unit_placeholders(graph, node_id, var_spec, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(CallbackInterface) :: iface
      type(CallbackArgumentSpecMap), target :: arguments
      type(CallbackArgumentSpecMapIterator) :: iter
      character(:), allocatable :: argument_name
      type(CallbackArgumentSpec) :: arg_spec
      character(:), allocatable :: unit_text
      character(:), allocatable :: placeholder_name
      type(VariableSpec) :: member
      character(:), allocatable :: concrete_unit
      type(StringVector) :: seen_placeholders, seen_units
      integer :: found_at, i
      character(:), allocatable :: msg

      iface = get_callback_interface(var_spec%callback_interface_id, _RC)
      arguments = iface%get_arguments()

      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         if (allocated(argument_name)) deallocate(argument_name)
         allocate(argument_name, source=iter%first())
         arg_spec = iter%second()

         if (.not. arg_spec%has_units()) cycle
         unit_text = arg_spec%get_units()
         if (len(unit_text) == 0) cycle

         if (.not. var_spec%has_member(argument_name)) cycle
         member = var_spec%get_member(argument_name, _RC)
         if (allocated(member%callback_member_alias)) cycle

         if (unit_text(1:1) /= '$') then
            call bind_concrete_units(graph, node_id, var_spec, argument_name, unit_text, _RC)
            cycle
         end if
         placeholder_name = unit_text(2:)

         msg = 'CompositeStateMaterialization: Handler "' // var_spec%short_name // &
              '" argument "' // argument_name // '" expects unit placeholder "' // &
              placeholder_name // '" bound on its member, but the member binds none'
         _ASSERT(allocated(member%callback_unit_placeholder), msg)

         msg = 'CompositeStateMaterialization: Handler "' // var_spec%short_name // &
              '" argument "' // argument_name // '" binds placeholder "' // &
              member%callback_unit_placeholder // '", interface expects "' // &
              placeholder_name // '"'
         _ASSERT(member%callback_unit_placeholder == placeholder_name, msg)

         concrete_unit = ''
         if (allocated(member%units)) concrete_unit = member%units

         found_at = 0
         do i = 1, seen_placeholders%size()
            if (seen_placeholders%of(i) == placeholder_name) then
               found_at = i
               exit
            end if
         end do
         if (found_at > 0) then
            msg = 'CompositeStateMaterialization: Handler "' // var_spec%short_name // &
                 '" placeholder "' // placeholder_name // '" bound to "' // &
                 seen_units%of(found_at) // '" and "' // concrete_unit // &
                 '" by different arguments'
            _ASSERT(seen_units%of(found_at) == concrete_unit, msg)
         else
            call seen_placeholders%push_back(placeholder_name)
            call seen_units%push_back(concrete_unit)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine check_unit_placeholders

   ! A Handler argument whose CallbackInterface declares a fixed
   ! (non-placeholder) unit: fills the bound member's own
   ! PHYSICAL_UNITS_CHARACTERISTIC_KIND characteristic with that unit
   ! when the member left its own units unspecified, or confirms the
   ! member's own declared unit already matches when it did not.
   subroutine bind_concrete_units(graph, node_id, var_spec, argument_name, concrete_unit, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      type(VariableSpec), intent(in) :: var_spec
      character(*), intent(in) :: argument_name
      character(*), intent(in) :: concrete_unit
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: composite_node, member_node
      type(GraphStateItem), pointer :: composite_payload
      type(StateItemMemberMap), pointer :: members
      type(NodeId), pointer :: member_id
      type(StateItemDescriptor), pointer :: descriptor
      class(StateItemCharacteristic), allocatable :: existing
      character(:), allocatable :: msg

      composite_node => graph%get_node(node_id)
      _ASSERT(associated(composite_node), 'CompositeStateMaterialization: bind_concrete_units - composite NodeId not found')
      select type (composite_node)
      class is (StateItemNode)
         composite_payload => composite_node%get_payload_ptr()
      class default
         _FAIL('CompositeStateMaterialization: bind_concrete_units - composite node is not a StateItemNode')
      end select
      members => composite_payload%state_members_ptr()

      member_id => members%at(argument_name)
      msg = 'CompositeStateMaterialization: bind_concrete_units - member not found: ' // argument_name
      _ASSERT(associated(member_id), msg)

      member_node => graph%get_node(member_id)
      msg = 'CompositeStateMaterialization: bind_concrete_units - member NodeId not found: ' // argument_name
      _ASSERT(associated(member_node), msg)
      select type (member_node)
      class is (StateItemNode)
         descriptor => member_node%get_descriptor()
      class default
         _FAIL('CompositeStateMaterialization: bind_concrete_units - member node is not a StateItemNode: ' // argument_name)
      end select

      if (descriptor%has_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND)) then
         call descriptor%get_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, existing, _RC)
      end if

      if (allocated(existing)) then
         if (existing%get_status() == CHARACTERISTIC_STATUS_SPECIFIED) then
            select type (existing)
            class is (PhysicalUnitsCharacteristic)
               msg = 'CompositeStateMaterialization: Handler "' // var_spec%short_name // &
                    '" argument "' // argument_name // '" expects unit "' // concrete_unit // &
                    '", member declares "' // existing%get_units() // '"'
               _ASSERT(existing%get_units() == concrete_unit, msg)
            class default
               _FAIL('CompositeStateMaterialization: bind_concrete_units - existing characteristic is not PhysicalUnitsCharacteristic: ' // argument_name)
            end select
            _RETURN(_SUCCESS)
         end if
      end if

      call descriptor%set_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, PhysicalUnitsCharacteristic(concrete_unit), _RC)

      _RETURN(_SUCCESS)
   end subroutine bind_concrete_units

   ! One declared member: a leaf (no members) or a nested composite, decided
   ! by var_spec%get_member_names().
   recursive function materialize_member(graph, var_spec, rc) result(node_id)
      class(ComponentGraph), target, intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(StateItemNode) :: node
      type(StringVector) :: names
      class(GraphNode), pointer :: registered_node
      type(StateItemDescriptor), pointer :: descriptor

      names = var_spec%get_member_names()
      if (allocated(var_spec%callback_member_alias)) then
         ! Alias member: a zero-copy view of this same Handler's own
         ! INTERNAL item, not a new leaf. Resolve the INTERNAL item's
         ! own, already-registered NodeId and share it rather than
         ! building a second node for the same storage.
         ! INTERNAL_ITEM_KEY_PREFIX (mapl_StateItemKeyPrefix_mod) keeps
         ! this key format in sync with item_key() (GraphBuilder.F90)
         ! without this graph-neutral module depending on
         ! mapl_GraphBuilder_mod.
         block
            type(NodeId), pointer :: aliased_id
            character(:), allocatable :: msg
            aliased_id => graph%get_resource_index(INTERNAL_ITEM_KEY_PREFIX // var_spec%callback_member_alias)
            msg = 'CompositeStateMaterialization: materialize_member - aliased INTERNAL item not found: ' &
                 // var_spec%callback_member_alias
            _ASSERT(associated(aliased_id), msg)
            node_id = aliased_id
         end block
      else if (names%size() > 0) then
         node_id = materialize_composite(graph, var_spec, _RC)
      else
         ! Unallocated payload/default revision: "advertised, not yet realized",
         ! as advertise_one does for a flat item. Realization is out of scope here.
         node_id = graph%next_node_id(_RC)
         node = StateItemNode(node_id, payload, revision)
         call graph%register_node(node, _RC)

         ! A leaf member's own VariableSpec never reaches advertise_one (only
         ! the composite's top-level var_spec does), so this is the only
         ! place its characteristics (geometry, units, ...) get attached to
         ! its node; wire_callback_argument and any other per-member
         ! comparison depend on this. Mutate the stored descriptor in place
         ! through a pointer, not a copy, as advertise_one does.
         registered_node => graph%get_node(node_id)
         _ASSERT(associated(registered_node), 'CompositeStateMaterialization: materialize_member - newly-registered node not found')
         select type (registered_node)
         class is (StateItemNode)
            descriptor => registered_node%get_descriptor()
            call var_spec%make_characteristics(descriptor, _RC)
         class default
            _FAIL('CompositeStateMaterialization: materialize_member - newly-registered node is not a StateItemNode')
         end select
      end if

      _RETURN(_SUCCESS)
   end function materialize_member

end module mapl_CompositeStateMaterialization_mod
