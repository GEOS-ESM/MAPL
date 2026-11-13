#include "MAPL.h"

! Destination-kind resolution for connections already matched to an export:
! ordinary inout-borrower aliasing, callback-declared destinations, and
! wildcard-import destinations.
!
! A direct sibling of GraphBuilder_ConnectionResolution_smod.F90 (both
! descend from mapl_GraphBuilder_mod directly): shared helpers it calls
! (push_warnings, find_export_node, mutable_descriptor, ...) are module
! procedures declared in mapl_GraphBuilder_mod's own interface block, not
! reached through submodule nesting.
submodule (mapl_GraphBuilder_mod) GraphBuilder_ConnectionDestinations_smod
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

   ! Resolves a destination declared an ordinary inout borrower (direct-alias
   ! case only). Reuses find_export_node_id/resolve_descriptors
   ! as resolve_one's ordinary path does for the exact-match check. On an
   ! exact match, adds the forward edge (owner -> borrower) in the forward
   ! network and a return edge (borrower -> owner) in a fresh per-pairing
   ! network, so no network ever has two producers of one NodeId. Shapes
   ! reserved for a future design (mismatched payload, no identifiable
   ! owner, chained borrowing) are rejected via `unsupported`, never
   ! delegated to extension-chain construction. Only graph structure is
   ! built here; the runtime return-edge propagation trigger does not exist
   ! yet.
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, dst_node, effective_src_name, unsupported, &
        conflicts, warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
      character(*), intent(in) :: effective_src_name
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      type(GraphResolutionEntryVector), intent(inout) :: conflicts
      type(GraphResolutionEntryVector), intent(inout) :: warnings
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(DependencyNetworkId) :: net_id, return_net_id
      type(NodeId) :: import_node_id, owner_node_id
      logical :: has_owner
      class(StateItemNode), pointer :: owner_node
      type(ComponentGraph), pointer :: owner_graph
      type(OuterMetaComponent), pointer :: owner_meta
      type(PortIdNodeIdMap), target :: owner_import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: candidate_id
      class(GraphNode), pointer :: candidate_node
      logical :: owner_is_chained
      type(StateItemDescriptor), pointer :: owner_item, borrower_item
      character(:), allocatable :: status_conflict
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      type(StringVector) :: match_warnings

      graph => this%get_component_graph()
      net_id = graph%get_forward_network_id(_RC)

      import_node_id = resolve_local_node_id(this, dst_pt%component_name, &
           ESMF_STATEINTENT_IMPORT, dst_node%get_short_name(), _RC)

      ! Source lookup key is the effective source name (see resolve_one).
      has_owner = find_export_node_id(this, src_pt%component_name, effective_src_name, &
           owner_node_id, owner_node, _RC)

      if (.not. has_owner) then
         ! No identifiable owner: reported, never silently treated as an ordinary
         ! import.
         call push_entry(unsupported, dst_pt, dst_node, 'inout borrower has no identifiable owner')
         _RETURN(_SUCCESS)
      end if

      ! Chained borrowing: owner_node is the source's EXPORT-intent node, and
      ! is_inout_borrower() is only true on IMPORT-intent nodes, so checking it
      ! directly would never detect chaining. Instead scan the owner
      ! component's import ports for an inout-borrower IMPORT with the owner's
      ! short name (i.e. the value being borrowed is itself borrowed). The name
      ! compared is the OWNER's (owner_node), not dst_node's, which may differ
      ! for a SimpleConnection.
      owner_meta => resolve_qualified_component(this, src_pt%component_name, _RC)
      owner_graph => owner_meta%get_component_graph()
      owner_import_ports = owner_graph%get_import_ports()
      owner_is_chained = .false.
      associate (e => owner_import_ports%ftn_end())
         iter = owner_import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_id = iter%second()
            candidate_node => owner_graph%get_node(candidate_id)
            _ASSERT(associated(candidate_node), 'GraphBuilder: resolve_inout_destination - import port NodeId not found')
            select type (candidate_node)
            class is (StateItemNode)
               if (candidate_node%get_short_name() == owner_node%get_short_name() .and. &
                    candidate_node%is_inout_borrower()) then
                  owner_is_chained = .true.
                  exit
               end if
            class default
               _FAIL('GraphBuilder: resolve_inout_destination - import port NodeId is not a StateItemNode')
            end select
         end do
      end associate
      if (owner_is_chained) then
         call push_entry(unsupported, dst_pt, dst_node, &
              'inout borrower owner is itself a declared inout borrower (chained borrowing)')
         _RETURN(_SUCCESS)
      end if

      owner_item => owner_node%get_descriptor()
      borrower_item => mutable_descriptor(graph, import_node_id, _RC)
      call resolve_descriptors(owner_item, borrower_item, mismatched, status_conflict, warnings=match_warnings, _RC)
      if (status_conflict /= '') then
         call push_entry(unsupported, dst_pt, dst_node, status_conflict)
         _RETURN(_SUCCESS)
      end if

      if (size(mismatched) /= 0) then
         ! A mismatched pairing is rejected rather than chained: the
         ! non-direct-alias case is reserved, so extension-chain construction must
         ! not be used.
         call push_entry(unsupported, dst_pt, dst_node, &
              'inout borrower payload does not exactly match its owner (non-direct-alias case not supported)')
         _RETURN(_SUCCESS)
      end if

      ! Direct-alias case: edge in the forward network (as in the ordinary
      ! exact-match case) plus a return edge in a fresh per-pairing
      ! write-back network. No TransformGraphNode for either.
      ! A different owner already feeds this borrower: record it and skip the
      ! pairing (no forward edge, no return network).
      if (record_second_producer(graph, owner_node_id, import_node_id, dst_pt, dst_node, conflicts)) then
         _RETURN(_SUCCESS)
      end if
      call graph%add_dependency(net_id, owner_node_id, import_node_id, _RC)
      call push_warnings(warnings, dst_pt%component_name, dst_node%get_short_name(), match_warnings)

      return_net_id = graph%create_network(_RC)
      call graph%add_dependency(return_net_id, import_node_id, owner_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_inout_destination

   ! Orchestrates one callback-declaring destination's full resolution:
   ! wildcard match + interface conformance (resolve_callback_import),
   ! materialization (materialize_callback_collection), wiring to the
   ! advertised destination import via an ordinary add_dependency edge, and
   ! per-match CallbackStateBinding construction
   ! (build_callback_state_binding). Idempotent: a hierarchy-wide reuse
   ! key derived from the import's NodeId is checked first (as
   ! find_or_build_extension_chain's chain_key is), so a repeat
   ! resolve_connections() call, from any declaring component, does not
   ! duplicate collection members.
   module subroutine resolve_callback_destination(this, src_pt, dst_pt, dst_short_name, expected_interface_id, rejected, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      character(*), intent(in) :: dst_short_name
      type(CallbackInterfaceId), intent(in) :: expected_interface_id
      type(StringVector), optional, intent(out) :: rejected
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeId) :: import_id
      character(:), allocatable :: collection_key
      type(NodeId), pointer :: existing_collection
      type(QualifiedExportEntry), allocatable :: matches(:)
      type(NodeId) :: collection_id
      type(DependencyNetworkId) :: net_id

      graph => this%get_component_graph()

      import_id = resolve_local_node_id(this, dst_pt%component_name, ESMF_STATEINTENT_IMPORT, dst_short_name, _RC)

      collection_key = 'CALLBACK_COLLECTION:' // import_id%to_string()
      existing_collection => graph%get_reuse_entry(collection_key, _RC)
      if (associated(existing_collection)) then
         if (present(rejected)) rejected = StringVector()
         _RETURN(_SUCCESS)
      end if

      matches = resolve_callback_import(this, src_pt, expected_interface_id, rejected, _RC)

      collection_id = materialize_callback_collection(this, src_pt, matches, _RC)
      call graph%add_reuse_entry(collection_key, collection_id, _RC)

      net_id = graph%get_forward_network_id(_RC)
      call graph%add_dependency(net_id, collection_id, import_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_callback_destination

   ! Resolves one WILDCARD-typed destination import by re-running its own
   ! declared pattern against the connection's source export namespace
   ! (build_qualified_export_namespace) and fanning the result out into one
   ! composite member per match, bound in place onto dst_node's own NodeId
   ! rather than a freshly-synthesized collection node. Every match is bound
   ! via add_state_member (pure reference binding), never add_dependency: a
   ! WILDCARD import gains no DependencyNetwork edge through this path.
   module subroutine resolve_wildcard_destination(this, src_pt, dst_pt, dst_node, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      character(:), allocatable :: collection_key
      type(NodeId), pointer :: existing_collection
      class(OuterMetaComponent), pointer :: root_meta
      type(QualifiedExportEntry), allocatable :: namespace(:)
      type(QualifiedExportEntry), allocatable :: matches(:)
      type(QualifiedExportEntry), allocatable :: grown_matches(:)
      type(VirtualConnectionPt) :: wildcard_v_pt
      type(VirtualConnectionPt) :: candidate_v_pt
      type(GraphStateItem), pointer :: payload
      type(ESMF_State) :: state
      type(NodeId) :: member_id
      type(NodeId) :: dst_node_id
      class(Logger), pointer :: lgr
      character(:), allocatable :: msg
      integer :: i, n, n_skipped

      graph => this%get_component_graph()
      dst_node_id = dst_node%get_node_id()

      ! Idempotency, keyed off the import's NodeId (as
      ! resolve_callback_destination does).
      collection_key = 'WILDCARD_COLLECTION:' // dst_node_id%to_string()
      existing_collection => graph%get_reuse_entry(collection_key, _RC)
      if (associated(existing_collection)) then
         _RETURN(_SUCCESS)
      end if

      ! Same source-side scan resolve_callback_import drives, with the
      ! EXPORT-side candidate. The regex-bearing operand must be the wildcard
      ! import's own pattern (dst_node%get_short_name(), rebuilt with EXPORT
      ! intent), NOT dst_pt%v_pt: for an all_unsatisfied-style connection that
      ! is the connection's IMPORT-intent destination pattern, and
      ! VirtualConnectionPt%matches() compares state_intent first, so it would
      ! never match an EXPORT candidate.
      allocate(matches(0))
      n_skipped = 0
      lgr => this%get_logger()

      wildcard_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, dst_node%get_short_name())

      root_meta => resolve_qualified_component(this, src_pt%component_name, _RC)
      namespace = build_qualified_export_namespace(root_meta, _RC)

      do i = 1, size(namespace)
         candidate_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, namespace(i)%qualified_name)
         if (.not. wildcard_v_pt%matches(candidate_v_pt)) cycle

         ! Only FIELD exports are members; any other shape is skipped (not a
         ! connection conflict).
         if (.not. is_wildcard_member_shape(namespace(i))) then
            n_skipped = n_skipped + 1
            ! One pre-concatenated string: pflogger %a substitution is
            ! unreliable under gfortran.
            msg = 'GraphBuilder: wildcard import ' // &
                 dst_pt%component_name // ':' // dst_node%get_short_name() // &
                 ' skips non-FIELD export ' // namespace(i)%qualified_name // &
                 ' (' // itemtype_name(namespace(i)%var_spec%itemType) // ')'
            call lgr%debug(msg)
            cycle
         end if

         ! Grow via a fresh temporary + move_alloc: a self-referencing array
         ! constructor ICEs classic ifort.
         n = size(matches)
         allocate(grown_matches(n + 1))
         grown_matches(1:n) = matches
         grown_matches(n + 1) = namespace(i)
         call move_alloc(grown_matches, matches)
      end do

      if (size(matches) == 0) then
         ! Zero matches: reported as unresolved. No reuse entry, so a
         ! later connection that supplies a match is not blocked by a stale guard.
         if (n_skipped > 0) then
            msg = 'GraphBuilder: wildcard import ' // dst_pt%component_name // &
                 ':' // dst_node%get_short_name() // ' is unresolved: all ' // &
                 'matching exports are non-FIELD and are not Wildcard members'
            call lgr%warning(msg)
         end if
         call push_entry(unresolved, dst_pt, dst_node)
         _RETURN(_SUCCESS)
      end if

      ! Materialize in place on dst_node's payload, then one add_state_member
      ! per match (as materialize_callback_collection's per-match loop does).
      payload => dst_node%get_payload_ptr()
      state = ESMF_StateCreate(_RC)
      call payload%set(state, _RC)

      do i = 1, size(matches)
         member_id = resolve_qualified_export_node_id(this, src_pt%component_name, matches(i), _RC)
         call payload%add_state_member(matches(i)%qualified_name, member_id, _RC)
      end do

      call graph%add_reuse_entry(collection_key, dst_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_wildcard_destination

end submodule GraphBuilder_ConnectionDestinations_smod
