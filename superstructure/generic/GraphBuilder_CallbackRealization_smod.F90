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
   use esmf, only: ESMF_StateAddReplace
   use esmf, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_STATE, ESMF_STATEITEM_NOTFOUND
   use mapl_CallbackAttributeAccess_mod, only: record_callback_interface_name
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMapIterator, operator(/=)
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
   ! under member_name. A member whose payload is still NOTFOUND (a new,
   ! non-alias leaf: ADVERTISE only attached its descriptor) gets its
   ! Field/FieldBundle created and completed now, by the same routine
   ! advertise_one's flat items and ordinary exports use
   ! (materialize_field_extension), reading the descriptor CompositeState
   ! Materialization and GEOM_A/B already completed. An alias member (its
   ! node IS the aliased INTERNAL item's own node) is already a complete
   ! Field/FieldBundle by now (graphbuilder_allocate_internals runs just
   ! before this). A nested composite member (payload kind STATE)
   ! recurses rather than being treated as a leaf.
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

      call materialize_field_extension(graph, member_id, name=member_name, with_members=.true., _RC)
      kind_flag = payload%itemType(_RC)

      if (kind_flag == ESMF_STATEITEM_FIELDBUNDLE) then
         bundle = payload%get_field_bundle(_RC)
         bundle_alias = MAPL_NamedAlias(bundle, name=member_name, _RC)
         call ESMF_StateAddReplace(composite_state, [bundle_alias], _RC)
      else if (kind_flag == ESMF_STATEITEM_FIELD) then
         field = payload%get_field(_RC)
         field_alias = MAPL_NamedAlias(field, name=member_name, _RC)
         call ESMF_StateAddReplace(composite_state, [field_alias], _RC)
      else
         msg = 'GraphBuilder: realize_callback_member - member "' // member_name // &
              '" is neither Field, FieldBundle nor State after materialization'
         _FAIL(msg)
      end if

      _RETURN(_SUCCESS)
   end subroutine realize_callback_member

end submodule GraphBuilder_CallbackRealization_smod
