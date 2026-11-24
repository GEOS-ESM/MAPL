#include "MAPL.h"

! Callback state realization: gives each Handler's top-level callback
! composite (itemType STATE, var_spec%callback_interface_id valid or not -
! any STATE-itemType EXPORT is handled uniformly) a real ESMF_State
! holding its declared members, at REALIZE_ACCEPTED. A new (non-alias)
! member gets its Field/FieldBundle created and completed here (deferred
! from ADVERTISE, which only attaches its descriptor); an alias member's
! Field is already complete by this point, shared zero-copy with its own
! INTERNAL item (graphbuilder_allocate_internals runs just before this).
! A nested composite member recurses the same fill-then-insert shape.

submodule (mapl_GraphBuilder_mod) GraphBuilder_CallbackRealization_smod
   use esmf, only: ESMF_StateAddReplace, ESMF_StateAdd, ESMF_StateRemove
   use esmf, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_STATE, ESMF_STATEITEM_NOTFOUND
   use mapl_CallbackAttributeAccess_mod, only: record_callback_interface_name
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMapIterator, operator(/=)
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Walks this component's own EXPORT var_specs for a STATE itemType
   ! (a Handler's declared callback composite, or any other STATE-kind
   ! export) and realizes each one into export_state. A no-op unless the
   ! graph populates the real user states; a legacy-populated callback
   ! composite is out of scope (legacy has no such concept).
   module subroutine graphbuilder_realize_callback_states(this, export_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State), intent(inout) :: export_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: id_ptr
      class(GraphNode), pointer :: generic_node
      type(GraphStateItem), pointer :: payload
      type(ESMF_StateItem_Flag) :: kind_flag

      _RETURN_UNLESS(graph_populates_user_states())

      graph => this%get_component_graph()
      spec => this%get_component_spec()
      associate (e => spec%var_specs%ftn_end())
         iter = spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle
            if (.not. (var_spec%itemType == MAPL_STATEITEM_STATE)) cycle
            id_ptr => graph%get_resource_index( &
                 item_key(var_spec%state_intent, var_spec%short_name))
            if (.not. associated(id_ptr)) cycle
            generic_node => graph%get_node(id_ptr)
            if (.not. associated(generic_node)) cycle
            select type (generic_node)
            class is (StateItemNode)
               payload => generic_node%get_payload_ptr()
            class default
               cycle
            end select
            ! A zero-member STATE var_spec is registered via advertise_one's
            ! flat-item branch with a completely unallocated payload, not
            ! through CompositeStateMaterialization; skip it here.
            kind_flag = payload%itemType(_RC)
            if (.not. (kind_flag == ESMF_STATEITEM_STATE)) cycle
            call realize_callback_state(graph, id_ptr, var_spec%short_name, export_state, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_realize_callback_states

   ! Fills state_node_id's own real ESMF_State with every declared
   ! member (realize_callback_member), records its interface name when
   ! it carries one, then inserts a MAPL_NamedAlias of itself under
   ! short_name into target_state (the component's real export State for
   ! a top-level call, or an enclosing composite's own ESMF_State for a
   ! nested one). Add-or-replace by name: idempotent across a second
   ! REALIZE_ACCEPTED pass.
   recursive subroutine realize_callback_state(graph, state_node_id, short_name, target_state, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: state_node_id
      character(*), intent(in) :: short_name
      type(ESMF_State), intent(inout) :: target_state
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: generic_node
      type(GraphStateItem), pointer :: payload
      type(ESMF_State) :: composite_state, named_state
      type(StateItemMemberMap), pointer :: members
      type(StateItemMemberMapIterator) :: iter
      type(StringVector) :: member_names
      integer :: i
      type(NodeId), pointer :: member_id
      type(CallbackInterfaceId) :: iface_id
      character(:), allocatable :: interface_name
      character(:), allocatable :: msg

      generic_node => graph%get_node(state_node_id)
      _ASSERT(associated(generic_node), 'GraphBuilder: realize_callback_state - NodeId not found: ' // short_name)
      select type (generic_node)
      class is (StateItemNode)
         payload => generic_node%get_payload_ptr()
         iface_id = generic_node%get_callback_interface_id()
      class default
         _FAIL('GraphBuilder: realize_callback_state - node is not a StateItemNode: ' // short_name)
      end select

      composite_state = payload%get_state(_RC)
      members => payload%state_members_ptr()

      ! Collect names first: realize_callback_member may recurse into
      ! realize_callback_state for a nested composite member, and no map
      ! iterator should be live across that.
      member_names = StringVector()
      iter = members%ftn_begin()
      do while (iter /= members%ftn_end())
         call iter%next()
         call member_names%push_back(iter%first())
      end do

      do i = 1, member_names%size()
         member_id => members%at(member_names%of(i))
         msg = 'GraphBuilder: realize_callback_state - member not found: ' // member_names%of(i)
         _ASSERT(associated(member_id), msg)
         call realize_callback_member(graph, member_id, member_names%of(i), composite_state, _RC)
      end do

      if (iface_id%is_valid()) then
         interface_name = get_callback_interface_name(iface_id, _RC)
         call record_callback_interface_name(composite_state, interface_name, _RC)
      end if

      named_state = MAPL_NamedAlias(composite_state, name=short_name, _RC)
      call ESMF_StateAddReplace(target_state, [named_state], _RC)

      _RETURN(_SUCCESS)
   end subroutine realize_callback_state

   ! Realizes one composite member and inserts it into composite_state
   ! under member_name. A member with a forward-network predecessor (its
   ! argument is get-side/IN-or-INOUT in some bound method) is aliased to
   ! that predecessor's already-materialized Field/FieldBundle, so a
   ! units-converting extension chain's output actually reaches the member
   ! the user code sees; otherwise a new Field/FieldBundle is created here
   ! (deferred from ADVERTISE), or, for an alias member, reused from the
   ! already-materialized INTERNAL item it aliases. A nested composite
   ! member (payload kind STATE) recurses rather than being treated as a
   ! leaf.
   recursive subroutine realize_callback_member(graph, member_id, member_name, composite_state, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: member_id
      character(*), intent(in) :: member_name
      type(ESMF_State), intent(inout) :: composite_state
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: generic_node
      type(GraphStateItem), pointer :: payload
      type(StateItemDescriptor), pointer :: descriptor
      type(StateItemCharacteristicKind), allocatable :: incomplete(:)
      logical :: is_alias_member
      type(ESMF_StateItem_Flag) :: kind_flag
      type(ESMF_Field) :: field, field_alias
      type(ESMF_FieldBundle) :: bundle, bundle_alias
      character(:), allocatable :: msg
      type(DependencyNetwork), pointer :: net
      type(DependencyNetworkId) :: forward_net_id
      type(NodeId) :: predecessor_id
      logical :: has_predecessor

      generic_node => graph%get_node(member_id)
      msg = 'GraphBuilder: realize_callback_member - NodeId not found for member: ' // member_name
      _ASSERT(associated(generic_node), msg)
      select type (generic_node)
      class is (StateItemNode)
         payload => generic_node%get_payload_ptr()
         descriptor => generic_node%get_descriptor()
         is_alias_member = generic_node%has_short_name()
      class default
         _FAIL('GraphBuilder: realize_callback_member - node is not a StateItemNode: ' // member_name)
      end select

      kind_flag = payload%itemType(_RC)

      if (kind_flag == ESMF_STATEITEM_STATE) then
         call realize_callback_state(graph, member_id, member_name, composite_state, _RC)
         _RETURN(_SUCCESS)
      end if

      msg = 'GraphBuilder: realize_callback_member - member "' // member_name // &
           '" aliases an INTERNAL item not yet materialized; INTERNAL items must be ' // &
           'materialized before the composite that aliases them'
      _ASSERT(.not. (kind_flag == ESMF_STATEITEM_NOTFOUND .and. is_alias_member), msg)

      incomplete = descriptor%get_incomplete_kinds()
      msg = 'GraphBuilder: realize_callback_member - member "' // member_name // &
           '" has an incomplete characteristic at REALIZE_ACCEPTED'
      _ASSERT(size(incomplete) == 0, msg)

      forward_net_id = graph%get_forward_network_id(_RC)
      net => graph%get_network(forward_net_id)
      has_predecessor = .false.
      if (.not. is_alias_member) then
         predecessor_id = find_materialized_predecessor(graph, net, member_id, has_predecessor)
      end if

      if (has_predecessor) then
         call get_item_if_materialized(graph, predecessor_id, field, bundle, kind_flag, has_predecessor, _RC)
      end if
      if (.not. has_predecessor) then
         call materialize_field_extension(graph, member_id, name=member_name, with_members=.true., _RC)
         kind_flag = payload%itemType(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELD) field = payload%get_field(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) bundle = payload%get_field_bundle(_RC)
      end if

      if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle_alias = MAPL_NamedAlias(bundle, name=member_name, _RC)
         call ESMF_StateAddReplace(composite_state, [bundle_alias], _RC)
      else if (kind_flag == ESMF_STATEITEM_FIELD) then
         field_alias = MAPL_NamedAlias(field, name=member_name, _RC)
         call ESMF_StateAddReplace(composite_state, [field_alias], _RC)
      else
         msg = 'GraphBuilder: realize_callback_member - member "' // member_name // &
              '" is neither Field, FieldBundle nor State after materialization'
         _FAIL(msg)
      end if

      _RETURN(_SUCCESS)
   end subroutine realize_callback_member

   ! Walks the IMPORT var_specs owned by this component for one declaring
   ! an expected callback interface (resolve_one_pair uses the same gate;
   ! such an import keeps the default FIELD declared_item_type - "members:"
   ! is Handler-EXPORT-only, so there is no itemType to filter on here) and
   ! fills the matched collection (resolve_callback_destination populates a
   ! "CALLBACK_COLLECTION:<import NodeId>" reuse entry) into import_state.
   ! An import with no reuse entry (no connection ever resolved it - an
   ! unmet import, reported elsewhere) is left alone.
   module subroutine graphbuilder_fill_callback_collections(this, import_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State), intent(inout) :: import_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: id_ptr
      character(:), allocatable :: collection_key
      type(NodeId), pointer :: collection_id

      _RETURN_UNLESS(graph_populates_user_states())

      graph => this%get_component_graph()
      spec => this%get_component_spec()
      associate (e => spec%var_specs%ftn_end())
         iter = spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT)) cycle
            if (.not. var_spec%callback_interface_id%is_valid()) cycle
            id_ptr => graph%get_resource_index( &
                 item_key(var_spec%state_intent, var_spec%short_name))
            if (.not. associated(id_ptr)) cycle
            collection_key = 'CALLBACK_COLLECTION:' // id_ptr%to_string()
            collection_id => graph%get_reuse_entry(collection_key, _RC)
            if (.not. associated(collection_id)) cycle
            call fill_callback_collection(graph, collection_id, var_spec%short_name, import_state, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_fill_callback_collections

   ! ESMF_StateAddReplace keeps an existing record's item kind, so swapping
   ! a callback import's own created FIELD for the collection STATE needs
   ! a remove then an add.
   subroutine fill_callback_collection(graph, collection_id, short_name, target_state, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: collection_id
      character(*), intent(in) :: short_name
      type(ESMF_State), intent(inout) :: target_state
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: generic_node
      type(GraphStateItem), pointer :: payload
      type(ESMF_State) :: collection_state, member_state, named_member, named_collection
      type(StateItemMemberMap), pointer :: members
      type(StateItemMemberMapIterator) :: iter
      type(StringVector) :: member_names
      integer :: i
      type(NodeId), pointer :: member_id
      class(GraphNode), pointer :: member_node
      type(GraphStateItem), pointer :: member_payload
      character(:), allocatable :: msg

      generic_node => graph%get_node(collection_id)
      msg = 'GraphBuilder: fill_callback_collection - collection NodeId not found: ' // short_name
      _ASSERT(associated(generic_node), msg)
      select type (generic_node)
      class is (StateItemNode)
         payload => generic_node%get_payload_ptr()
      class default
         _FAIL('GraphBuilder: fill_callback_collection - collection node is not a StateItemNode: ' // short_name)
      end select

      collection_state = payload%get_state(_RC)
      members => payload%state_members_ptr()

      ! Collect names first: the loop below only reads, but matches the
      ! established idiom already used by realize_callback_state for
      ! walking this map, rather than holding a live iterator across
      ! intervening error-handling calls.
      member_names = StringVector()
      iter = members%ftn_begin()
      do while (iter /= members%ftn_end())
         call iter%next()
         call member_names%push_back(iter%first())
      end do

      do i = 1, member_names%size()
         member_id => members%at(member_names%of(i))
         msg = 'GraphBuilder: fill_callback_collection - matched member not found: ' // member_names%of(i)
         _ASSERT(associated(member_id), msg)
         member_node => graph%get_node(member_id)
         msg = 'GraphBuilder: fill_callback_collection - matched member NodeId not found: ' // member_names%of(i)
         _ASSERT(associated(member_node), msg)
         select type (member_node)
         class is (StateItemNode)
            member_payload => member_node%get_payload_ptr()
         class default
            _FAIL('GraphBuilder: fill_callback_collection - matched member is not a StateItemNode: ' // member_names%of(i))
         end select
         member_state = member_payload%get_state(_RC)
         named_member = MAPL_NamedAlias(member_state, name=member_names%of(i), _RC)
         call ESMF_StateAddReplace(collection_state, [named_member], _RC)
      end do

      named_collection = MAPL_NamedAlias(collection_state, name=short_name, _RC)
      call ESMF_StateRemove(target_state, [short_name], relaxedFlag=.true., _RC)
      call ESMF_StateAdd(target_state, [named_collection], _RC)

      _RETURN(_SUCCESS)
   end subroutine fill_callback_collection

end submodule GraphBuilder_CallbackRealization_smod
