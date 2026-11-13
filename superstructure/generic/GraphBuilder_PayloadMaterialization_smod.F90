#include "MAPL.h"

! Export payload materialization: creates a MAPL_NamedAlias field or field
! bundle for an aliased export, drives extension-chain field/bundle
! materialization, and tears down a created payload when materialization
! fails or is superseded.
!
! A direct sibling of GraphBuilder_ConnectionResolution_smod.F90 (both
! descend from mapl_GraphBuilder_mod directly): shared helpers it calls
! (push_warnings, find_export_node, mutable_descriptor, ...) are module
! procedures declared in mapl_GraphBuilder_mod's own interface block, not
! reached through submodule nesting.
submodule (mapl_GraphBuilder_mod) GraphBuilder_PayloadMaterialization_smod
   use esmf, only: ESMF_State, ESMF_StateCreate, ESMF_StateGet, &
        ESMF_FieldDestroy, &
        ESMF_STATEITEM_NOTFOUND, ESMF_STATEITEM_FIELD, &
        ESMF_STATEITEM_FIELDBUNDLE, ESMF_STATEITEM_STATE, &
        ESMF_STATEINTENT_INTERNAL, operator(==), operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator, operator(/=)
   use mapl_ExpressionTransform_mod, only: ExpressionTransform
   use mapl_ExtensionResolution_mod, only: find_or_build_extension_chain, materialize_extensions_enabled
   use mapl_StateItemDescriptor_mod, only: resolve_descriptors, &
        resolve_characteristics
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_MIRRORED, &
        CHARACTERISTIC_STATUS_DEFERRED, CHARACTERISTIC_STATUS_SPECIFIED, &
        operator(==)
   use mapl_StateItemCharacteristic_mod, only: &
        StateItemCharacteristicMapIterator, operator(/=)
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_ExtensionMaterialization_mod, only: field_extension_is_created
   use mapl_FieldCreate_mod, only: FieldsAreAliased
   use mapl_field_bundle_api, only: MAPL_FieldBundleDestroy
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap, PortIdNodeIdMapIterator, operator(/=)
   use mapl_PortId_mod, only: PortId
   use mapl_StateArithmeticParser_mod, only: parser_variables_in_expression
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use gFTL2_StringVector, only: StringVectorIterator, operator(/=)
   use pflogger, only: Logger
   use mapl_ReexportConnection_mod, only: ReexportConnection
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Materializes node_id's payload as a MAPL_NamedAlias of the export
   ! component's real, already-realized field (or field bundle, for a VECTOR,
   ! SERVICE or BRACKET item), read by name from that component's export
   ! ESMF_State (no StateRegistry). Tried by ensure_export_payload BEFORE
   ! try_materialize_field_extension: if found, the FieldCreate path is
   ! skipped. If the field is not yet available by name (two-pass timing),
   ! returns success with found = .false. and the caller falls back to
   ! FieldCreate. An item that is neither a Field nor a FieldBundle (e.g. a
   ! nested State) also gives found = .false.
   !
   ! export_port_name is the export-port key (find_export_node_id's key). A
   ! propagated port is `<child>/<leaf>`, passed up unchanged through every
   ! ancestor, and legacy keeps it out of those ancestors' user states; the
   ! Field is in the user export state of the component find_alias_owner
   ! walks down to, under <leaf>. A component's own export whose name has a
   ! slash (`<path>/<leaf>`) is a Field under <leaf> in the sub-state
   ! <path> of its export state, as FieldClassAspect%add_to_state puts it.
   !
   ! With graph_populates_user_states() there is nothing to alias: the state
   ! holds the node's own created Field, so found = .false. and the caller
   ! completes that Field in place. With it off, the created Field is
   ! replaced by the alias and destroyed (nothing else holds it).
   module subroutine try_alias_export_field(this, export_comp_name, export_port_name, &
        graph, node_id, found, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      character(*), intent(in) :: export_port_name
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status, slash
      character(:), allocatable :: leaf, sub_path
      logical :: has_substate
      type(OuterMetaComponent), pointer :: export_meta
      type(GriddedComponentDriver), pointer :: export_driver
      type(MultiState) :: export_states
      type(ESMF_State) :: export_state, holder
      type(ESMF_StateItem_Flag) :: item_kind
      type(ESMF_Field) :: field
      type(ESMF_Field) :: alias
      type(ESMF_FieldBundle) :: bundle, bundle_alias
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload

      found = .false.
      _RETURN_IF(graph_populates_user_states())

      slash = index(export_port_name, '/', back=.true.)
      leaf = export_port_name(slash + 1:)

      export_meta => resolve_qualified_component(this, export_comp_name, _RC)
      export_meta => find_alias_owner(export_meta, export_port_name, &
           sub_path, _RC)
      ! No component owns the port by that name: nothing to alias.
      _RETURN_UNLESS(associated(export_meta))
      export_driver => export_meta%get_user_gc_driver()
      export_states = export_driver%get_states()
      call export_states%get_state(export_state, ESMF_STATEINTENT_EXPORT, _RC)

      ! Read only: unlike get_substate, a missing sub-state is not created
      ! (the field may not exist yet).
      call find_substate(export_state, sub_path, holder, has_substate, _RC)
      _RETURN_UNLESS(has_substate)

      ! Plain by-name read; an absent item is not a failure (the field may not
      ! exist yet). Checked via itemType, as FieldClassAspect%add_to_state
      ! does, rather than trusting ESMF_StateGet's rc.
      call ESMF_StateGet(holder, itemName=leaf, itemType=item_kind, _RC)
      if (item_kind == ESMF_STATEITEM_NOTFOUND) then
         _RETURN(_SUCCESS)
      end if

      ! Only a Field or a FieldBundle (VECTOR, SERVICE, BRACKET) can be
      ! aliased; any other kind (a nested State) is left to the caller.
      if (item_kind /= ESMF_STATEITEM_FIELD .and. &
           item_kind /= ESMF_STATEITEM_FIELDBUNDLE) then
         _RETURN(_SUCCESS)
      end if

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: try_alias_export_field - NodeId not found in graph')
      select type (node)
      class is (StateItemNode)
         ! Mutate the stored handle in place through a pointer rather than a
         ! get_payload()/set_payload() round trip (gfortran copy hazard).
         payload => node%get_payload_ptr()
         if (item_kind == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(holder, itemName=leaf, field=field, _RC)
            ! MAPL_NamedAlias (not a bare ESMF_NamedAlias) carries MAPL's
            ! per-alias metadata (standard_name/long_name/restart_mode), as
            ! populate_one does.
            alias = MAPL_NamedAlias(field, name=leaf, _RC)
            call destroy_created_payload(payload, graph, node_id, kept=field, _RC)
            call payload%set(alias, _RC)
         else
            ! A created payload is replaced and destroyed (nothing else holds
            ! it): the placeholder Field of a node declared FIELD whose legacy
            ! item is a bundle, or a created VECTOR or BRACKET bundle with its
            ! members.
            call destroy_created_payload(payload, graph, node_id, _RC)
            call ESMF_StateGet(holder, itemName=leaf, fieldBundle=bundle, _RC)
            bundle_alias = MAPL_NamedAlias(bundle, name=leaf, _RC)
            call payload%set(bundle_alias, _RC)
         end if
      class default
         _FAIL('GraphBuilder: try_alias_export_field - NodeId does not refer to a StateItemNode')
      end select

      found = .true.

      _RETURN(_SUCCESS)
   end subroutine try_alias_export_field

   ! The component whose user export state holds the Field behind export port
   ! `port_name` of `meta`, or null if no descendant matches; `sub_path` is
   ! the path of nested States inside that export state down to the Field
   ! (empty when it is at the top level). A name without '/' is meta's own
   ! export or re-export. A qualified `<child>/<leaf>` is either passed up
   ! unchanged from a deeper level (a child carries the same port name:
   ! recurse into it) or that child's own export (the child is named
   ! <child> and has a port named <leaf>). Otherwise a name with a slash that
   ! meta itself exports is meta's own export, held in the sub-state
   ! <path> (all before the last '/') as FieldClassAspect%add_to_state does.
   recursive function find_alias_owner(meta, port_name, sub_path, rc) &
        result(owner)
      type(OuterMetaComponent), pointer :: owner
      class(OuterMetaComponent), target, intent(inout) :: meta
      character(*), intent(in) :: port_name
      character(:), allocatable, intent(out) :: sub_path
      integer, optional, intent(out) :: rc

      integer :: status, slash, i
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta

      owner => null()
      sub_path = ''
      slash = index(port_name, '/', back=.true.)
      if (slash == 0) then
         owner => meta
         _RETURN(_SUCCESS)
      end if

      do i = 1, meta%get_num_children()
         child_name = meta%get_child_name(i, _RC)
         child_meta => meta%get_child_outer_meta(child_name, _RC)
         if (associated(find_export_node(child_meta, port_name))) then
            owner => find_alias_owner(child_meta, port_name, sub_path, _RC)
            _RETURN(_SUCCESS)
         end if
         if (child_name == port_name(1:slash - 1)) then
            if (associated(find_export_node(child_meta, port_name(slash + 1:)))) then
               owner => child_meta
               _RETURN(_SUCCESS)
            end if
         end if
      end do

      if (associated(find_export_node(meta, port_name))) then
         owner => meta
         sub_path = port_name(1:slash - 1)
      end if

      _RETURN(_SUCCESS)
   end function find_alias_owner

   ! The State nested under `state` along the '/'-separated `path`, without
   ! creating any level; found is false if a level is absent or is not a
   ! State. An empty path is `state` itself.
   subroutine find_substate(state, path, substate, found, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: path
      type(ESMF_State), intent(out) :: substate
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status, idx
      character(:), allocatable :: rest, level
      type(ESMF_State) :: inner
      type(ESMF_StateItem_Flag) :: item_kind

      found = .false.
      substate = state
      rest = path
      do while (len(rest) > 0)
         idx = index(rest, '/')
         if (idx == 0) then
            level = rest
            rest = ''
         else
            level = rest(1:idx - 1)
            rest = rest(idx + 1:)
         end if
         call ESMF_StateGet(substate, itemName=level, itemType=item_kind, _RC)
         _RETURN_UNLESS(item_kind == ESMF_STATEITEM_STATE)
         call ESMF_StateGet(substate, itemName=level, nestedState=inner, _RC)
         substate = inner
      end do
      found = .true.

      _RETURN(_SUCCESS)
   end subroutine find_substate

   ! Destroys node_id's payload if the graph created it, before it is
   ! replaced by an alias. A created Field is kept when it shares its host
   ! with `kept`; without `kept` (a bundle item) it is always destroyed.
   subroutine destroy_created_payload(payload, graph, node_id, kept, rc)
      type(GraphStateItem), intent(in) :: payload
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_Field), optional, intent(in) :: kept
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: payload_kind
      type(ESMF_Field) :: created
      type(ESMF_FieldBundle) :: created_bundle

      payload_kind = payload%itemType(_RC)
      _RETURN_UNLESS(field_extension_is_created(graph, node_id))
      if (payload_kind == ESMF_STATEITEM_FIELD) then
         created = payload%get_field(_RC)
         if (present(kept)) then
            call destroy_if_not_aliased(created, kept, _RC)
         else
            call ESMF_FieldDestroy(created, noGarbage=.true., _RC)
         end if
      else if (payload_kind == ESMF_STATEITEM_FIELDBUNDLE) then
         created_bundle = payload%get_field_bundle(_RC)
         call MAPL_FieldBundleDestroy(created_bundle, destroy_contents=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine destroy_created_payload

   ! Destroys `old` unless it shares its host with `kept` (an alias of the
   ! same Field must not be destroyed).
   subroutine destroy_if_not_aliased(old, kept, rc)
      type(ESMF_Field), intent(inout) :: old
      type(ESMF_Field), intent(in) :: kept
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: aliased

      aliased = FieldsAreAliased(old, kept, rc=status)
      _VERIFY(status)
      if (.not. aliased) then
         call ESMF_FieldDestroy(old, noGarbage=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine destroy_if_not_aliased

   ! Gives node_id (an export) a real ESMF_Field, or for a VECTOR or BRACKET a
   ! FieldBundle with its members, built from its own descriptor. Otherwise
   ! sets failure_reason (empty on success): an item class that cannot be
   ! created on either side, or a descriptor still missing a value creation
   ! needs.
   ! Optional `name` is passed through to materialize_field_extension so
   ! graph-native state population can find the field by name once it is
   ! inserted into an ESMF_State.
   subroutine try_materialize_field_extension(export_item, import_item, &
        graph, node_id, failure_reason, name, rc)
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(:), allocatable, intent(out) :: failure_reason
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      call check_materializable(export_item, import_item, export_item, &
           failure_reason)
      _RETURN_IF(failure_reason /= '')
      call materialize_field_extension(graph, node_id, name=name, &
           with_members=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine try_materialize_field_extension

   ! Gives export node node_id a real payload outside GRAPH_MODE_REGISTRY:
   ! an alias of the export component's field (alias_name) if try_alias and
   ! one exists, else one created from export_item/import_item. Creating
   ! after an alias would overwrite it. failure is empty unless creation was
   ! refused. With chain_input, a class that cannot be created is accepted
   ! when chain_input already holds a bundle payload.
   module subroutine ensure_export_payload(this, alias_comp, alias_name, graph, node_id, export_item, import_item, &
        materialize_name, try_alias, failure, chain_input, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: alias_comp
      character(*), intent(in) :: alias_name
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      character(*), intent(in) :: materialize_name
      logical, intent(in) :: try_alias
      character(:), allocatable, intent(out) :: failure
      class(StateItemNode), target, optional, intent(in) :: chain_input
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: alias_found
      type(GraphStateItem), pointer :: payload
      type(ESMF_StateItem_Flag) :: payload_type

      failure = ''
      _RETURN_IF(graph_mode() == GRAPH_MODE_REGISTRY)

      alias_found = .false.
      if (try_alias) then
         call try_alias_export_field(this, alias_comp, alias_name, graph, node_id, alias_found, _RC)
      end if
      _RETURN_IF(alias_found)

      if (present(chain_input) .and. .not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_VECTOR &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET)) then
         payload => chain_input%get_payload_ptr()
         payload_type = payload%itemType(_RC)
         if (.not. (payload_type == ESMF_STATEITEM_FIELDBUNDLE)) then
            failure = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         end if
      else
         call try_materialize_field_extension(export_item, import_item, graph, node_id, failure, &
              name=materialize_name, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine ensure_export_payload

   ! failure_reason is empty when a payload can be created for `item`. The
   ! export side admits FIELD, VECTOR and BRACKET (the graph creates a bundle
   ! for the latter two); the import side admits FIELD, VECTOR and BRACKET
   ! (a bracket only as the export passed as its own import, for a chain
   ! input); and item lacks no creation value. SERVICE is never admitted.
   subroutine check_materializable(export_item, import_item, item, &
        failure_reason)
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(StateItemDescriptor), intent(in) :: item
      character(:), allocatable, intent(out) :: failure_reason

      type(StateItemCharacteristicKind), allocatable :: incomplete(:)

      failure_reason = ''
      if (.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_VECTOR)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         return
      end if
      if (.not. (import_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
           .or. import_item%get_declared_item_type() == MAPL_STATEITEM_VECTOR &
           .or. import_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(import_item%get_declared_item_type())
         return
      end if
      incomplete = item%get_incomplete_kinds()
      if (size(incomplete) > 0) then
         failure_reason = 'incomplete_characteristic:' // &
              incomplete(1)%to_string()
      end if
   end subroutine check_materializable

   ! Materializes every entry of intermediate_node_ids (in order), each from
   ! that extension node's own descriptor (the tip after its step). Stops
   ! at the first link that cannot be materialized, returning
   ! check_materializable's failure_reason, or unsupported_item_class when
   ! the link itself is declared neither Field, Vector nor Bracket. `name`
   ! names the links, for consumers that look their inputs up by name.
   module subroutine materialize_extension_chain(export_item, import_item, &
        intermediate_node_ids, graph, failure_reason, rc, name)
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(NodeId), intent(in) :: intermediate_node_ids(:)
      type(ComponentGraph), target, intent(inout) :: graph
      character(:), allocatable, intent(out) :: failure_reason
      integer, optional, intent(out) :: rc
      character(*), optional, intent(in) :: name

      integer :: status
      integer :: i
      class(GraphNode), pointer :: link_node
      type(ESMF_StateItem_Flag) :: link_type
      type(StateItemDescriptor), pointer :: link_descriptor

      failure_reason = ''
      do i = 1, size(intermediate_node_ids)
         link_node => graph%get_node(intermediate_node_ids(i))
         _ASSERT(associated(link_node), 'GraphBuilder: materialize_extension_chain - link NodeId not found in graph')
         select type (link_node)
         class is (StateItemNode)
            call check_materializable(export_item, import_item, &
                 link_node%get_descriptor(), failure_reason)
            ! A link is built as a Field, or as a bundle without members
            ! for a vector or bracket; no other item class can be.
            link_descriptor => link_node%get_descriptor()
            link_type = link_descriptor%get_declared_item_type()
            if (failure_reason == '' .and. &
                 .not. (link_type == MAPL_STATEITEM_FIELD .or. &
                 link_type == MAPL_STATEITEM_VECTOR .or. &
                 link_type == MAPL_STATEITEM_BRACKET)) then
               failure_reason = 'unsupported_item_class:' // itemtype_name(link_type)
            end if
         class default
            _FAIL('GraphBuilder: materialize_extension_chain - link NodeId is not a StateItemNode')
         end select
         _RETURN_IF(failure_reason /= '')
         call materialize_field_extension(graph, intermediate_node_ids(i), &
              name=name, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine materialize_extension_chain

end submodule GraphBuilder_PayloadMaterialization_smod
