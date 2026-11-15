#include "MAPL.h"

! Match-connection resolution: drives wildcard/regex and exact-name import
! matching for MatchConnection destinations, applies ordinary
! extension-chain resolution on a match, and reports/collects imports that
! remain unresolved after a connect pass.
!
! A direct sibling of GraphBuilder_ConnectionResolution_smod.F90 (both
! descend from mapl_GraphBuilder_mod directly): shared helpers it calls
! (push_warnings, find_export_node, mutable_descriptor, ...) are module
! procedures declared in mapl_GraphBuilder_mod's own interface block, not
! reached through submodule nesting.
submodule (mapl_GraphBuilder_mod) GraphBuilder_ConnectionMatching_smod
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

   ! Read-only counterpart of resolve_match_connection() below: same
   ! destination-import filtering and same-name export existence check,
   ! but never calls resolve_local_node_id() or add_dependency() -
   ! no proxy node or dependency edge is created here.
   !
   ! Accepts any Connection subtype; the only subtype-dependent step (the
   ! source's effective short name) is isolated in effective_source_name.
   module subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: dst_meta, src_meta
      type(ComponentGraph), pointer :: dst_graph

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      ! resolve_qualified_component reaches any descendant, not only "<self>"
      ! or a direct child.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_graph => dst_meta%get_component_graph()
      src_meta => resolve_qualified_component(this, src_pt%component_name, _RC)

      call for_each_matching_import(dst_graph, dst_pt, check_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine check_one(node, rc)
         class(StateItemNode), target, intent(in) :: node
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: satisfied

         ! src_meta was resolved eagerly above: a bad source path fails even
         ! when no destination import matches.
         satisfied = source_satisfies_import(this, conn, node, src_meta=src_meta, _RC)
         if (.not. satisfied) then
            call push_entry(unresolved, dst_pt, node)
         end if

         _RETURN(_SUCCESS)
      end subroutine check_one

   end subroutine check_match_connection_unsatisfied

   ! Mirrors MatchConnection%activate() (connection/MatchConnection.F90):
   ! filter the destination's imports against the declared pattern, then for
   ! each match require an EXACT-name export on the source side (both
   ! comparisons are case-sensitive, like VirtualConnectionPt%matches).
   ! Computed against the destination ComponentGraph's import-port table
   ! rather than StateRegistry's subregistry machinery.
   ! VirtualConnectionPt%matches() and the ConnectionPt/VirtualConnectionPt
   ! accessors are plain queries, used as-is.
   !
   ! Accepts any Connection subtype: a SimpleConnection is the exact-match
   ! case of MatchConnection's pattern expansion, with no separate code path.
   module subroutine resolve_match_connection(this, conn, unresolved, unsupported, conflicts, warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      type(GraphResolutionEntryVector), intent(inout) :: conflicts
      type(GraphResolutionEntryVector), intent(inout) :: warnings
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: dst_meta
      type(ComponentGraph), pointer :: dst_graph
      type(ComponentGraph), pointer :: this_graph
      type(DependencyNetworkId) :: net_id

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      ! See check_match_connection_unsatisfied: resolve_qualified_component
      ! reaches any descendant.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_graph => dst_meta%get_component_graph()

      this_graph => this%get_component_graph()
      net_id = this_graph%get_forward_network_id(_RC)

      call for_each_matching_import(dst_graph, dst_pt, resolve_one, _RC)

      _RETURN(_SUCCESS)
   contains

      ! An import is settled once its pair has a final outcome; one whose pair
      ! still has a DEFERRED side is tried again on the next connect pass.
      subroutine resolve_one(dst_node, rc)
         class(StateItemNode), target, intent(in) :: dst_node
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: waiting

         _RETURN_IF(this%is_import_settled(dst_node%get_node_id()))

         waiting = .false.
         call resolve_one_pair(dst_node, waiting, rc=status)
         _VERIFY(status)
         if (waiting) then
            call this%note_connection_waiting(dst_node%get_node_id())
         else
            call this%note_import_settled(dst_node%get_node_id())
         end if

         _RETURN(_SUCCESS)
      end subroutine resolve_one

      subroutine resolve_one_pair(dst_node, waiting, rc)
         class(StateItemNode), target, intent(in) :: dst_node
         logical, intent(inout) :: waiting
         integer, optional, intent(out) :: rc

         integer :: status
         type(NodeId) :: import_node_id, export_node_id, final_node_id
         type(NodeId), allocatable :: intermediate_node_ids(:)
         logical :: has_export
         class(StateItemNode), pointer :: export_node
         type(StateItemDescriptor), pointer :: export_item, import_item
         type(StateItemCharacteristicKind), allocatable :: mismatched(:)
         character(:), allocatable :: unsupported_characteristic
         character(:), allocatable :: incompatible_pair
         character(:), allocatable :: materialization_failure
         type(StringVector) :: callback_rejected
         character(:), pointer :: rejected_item
         integer :: j
         type(CallbackInterfaceId) :: callback_interface_id
         type(StateItemDescriptor), pointer :: dst_descriptor
         character(:), allocatable :: status_conflict
         type(StringVector) :: match_warnings
         logical :: is_evaluation, evaluated, expression_strict
         character(:), allocatable :: expression_problem

         ! A callback interface and inout borrower intent are mutually exclusive
         ! resolution paths; reject the combination before either branch so
         ! neither silently wins.
         callback_interface_id = dst_node%get_callback_interface_id()
         if (callback_interface_id%is_valid() .and. dst_node%is_inout_borrower()) then
            call push_entry(unsupported, dst_pt, dst_node, &
                 'declares both an expected callback interface and inout borrower intent (mutually exclusive)')
            _RETURN(_SUCCESS)
         end if

         ! A destination declaring an expected callback interface is resolved
         ! against the flattened namespace and wired via an ordinary add_dependency
         ! edge; the exact-name/extension-chain logic below never runs for it.
          if (callback_interface_id%is_valid()) then
             call resolve_callback_destination(this, src_pt, dst_pt, dst_node%get_short_name(), &
                  callback_interface_id, callback_rejected, _RC)
             do j = 1, callback_rejected%size()
                rejected_item => callback_rejected%of(j)
                call push_entry(unsupported, dst_pt, dst_node, rejected_item)
             end do
             _RETURN(_SUCCESS)
          end if

          ! An inout borrower is resolved by resolve_inout_destination instead of
          ! the exact-name/extension-chain logic below. It takes the precomputed
          ! effective source name because a SimpleConnection's source name may
          ! differ from dst_node's.
          if (dst_node%is_inout_borrower()) then
             call resolve_inout_destination(this, src_pt, dst_pt, dst_node, effective_source_name(conn, dst_node), &
                  unsupported, conflicts, warnings, _RC)
             _RETURN(_SUCCESS)
          end if

          ! A WILDCARD destination (its short name is a regex, not one export's
          ! name) is resolved by fanning the regex matches against the source export
          ! namespace out into composite members of this import's own node. This
          ! branch adds no add_dependency edges.
          dst_descriptor => dst_node%get_descriptor()
          if (dst_descriptor%get_declared_item_type() == MAPL_STATEITEM_WILDCARD) then
             call resolve_wildcard_destination(this, src_pt, dst_pt, dst_node, unresolved, _RC)
             _RETURN(_SUCCESS)
          end if

          ! dst_node is already the exact match found by walking dst_graph's
          ! import-port table; use its NodeId directly. A resource-index lookup
          ! would fail for a MIRRORED import, which is deliberately not given a
          ! resource-index entry.
          import_node_id = dst_node%get_node_id()

          ! Source lookup key is the effective source name: a SimpleConnection's
          ! declared name may differ from dst_node's. For a MatchConnection it is
          ! dst_node's name (same-name wildcard rule).
          has_export = find_export_node_id(this, src_pt%component_name, effective_source_name(conn, dst_node), &
               export_node_id, export_node, _RC)

         if (.not. has_export) then
            ! Reported as unresolved, not silently dropped.
            call push_entry(unresolved, dst_pt, dst_node)
            _RETURN(_SUCCESS)
         end if

         export_item => export_node%get_descriptor()
         import_item => mutable_descriptor(dst_graph, import_node_id, _RC)
         ! An EXPRESSION export is a placeholder: the consumer is connected to
         ! an evaluation of it, then resolved as an ordinary pair.
         is_evaluation = &
              export_item%get_declared_item_type() == MAPL_STATEITEM_EXPRESSION
         if (is_evaluation) then
            call build_expression_evaluation(this, this_graph, net_id, &
                 src_pt%component_name, export_node, import_item, &
                 export_node_id, evaluated, expression_problem, &
                 incompatible_pair, waiting, expression_strict, _RC)
            if (incompatible_pair /= '') then
               call push_entry(conflicts, dst_pt, dst_node, incompatible_pair)
               _RETURN(_SUCCESS)
            end if
            if (expression_problem /= '') then
               call push_entry(unsupported, dst_pt, dst_node, expression_problem)
               if (expression_strict) then
                  call this%note_unsupported_pair(dst_pt%component_name // &
                       ':' // dst_node%get_short_name() // ':' // &
                       expression_problem)
               end if
               _RETURN(_SUCCESS)
            end if
            _RETURN_UNLESS(evaluated)
            export_item => mutable_descriptor(this_graph, export_node_id, _RC)
         end if
         call resolve_descriptors(export_item, import_item, mismatched, status_conflict, rc=status, waiting=waiting, &
              warnings=match_warnings)
         _VERIFY(status)
         _RETURN_IF(waiting)
         if (status_conflict /= '') then
            call push_entry(unsupported, dst_pt, dst_node, status_conflict)
            _RETURN(_SUCCESS)
         end if
         call push_warnings(warnings, dst_pt%component_name, dst_node%get_short_name(), match_warnings)

         if (size(mismatched) == 0) then
            ! Exact match: wire directly, no extension.
            if (record_second_producer(this_graph, export_node_id, import_node_id, dst_pt, dst_node, conflicts)) then
               _RETURN(_SUCCESS)
            end if
            call this_graph%add_dependency(net_id, export_node_id, import_node_id, _RC)

            ! Give the export node a real, allocated field using the same geom/vgrid
            ! resolution as the mismatch branch. Gated on graph mode, not on
            ! materialize_extensions_enabled() (an orthogonal gate).
            call ensure_export_payload(this, src_pt%component_name, effective_source_name(conn, dst_node), &
                 this_graph, export_node_id, export_item, import_item, export_node%get_short_name(), &
                 .not. is_evaluation, materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! The edge above still exists; only payload materialization failed.
               call push_entry(unsupported, dst_pt, dst_node, materialization_failure)
            end if

            _RETURN(_SUCCESS)
         end if

         ! Mismatch: delegate to extension-chain construction
         ! (mapl_ExtensionResolution_mod) rather than wiring the pair directly.
         call find_or_build_extension_chain(this_graph, net_id, export_node_id, &
              export_item%get_characteristics(), import_item%get_characteristics(), mismatched, &
              final_node_id, intermediate_node_ids, unsupported_characteristic, conflict=incompatible_pair, _RC)

         if (incompatible_pair /= '') then
            ! Reported apart from "no registered extension provider": this pair
            ! can never connect. The component's other connections still resolve.
            call push_entry(conflicts, dst_pt, dst_node, incompatible_pair)
            _RETURN(_SUCCESS)
         end if

         if (unsupported_characteristic /= '') then
            ! Distinguishable from "import has no matching export".
            call push_entry(unsupported, dst_pt, dst_node, unsupported_characteristic)
            ! Kept for the freeze check, which fails on these in strict mode.
            call this%note_unsupported_pair(dst_pt%component_name // ':' // dst_node%get_short_name() // ':' // &
                 unsupported_characteristic)
            _RETURN(_SUCCESS)
         end if

         ! The chain's input node (export_node_id) must be materialized too: the
         ! chain's compute() fails on an unallocated input. export_item is passed
         ! as BOTH export and import so the export's typekind/units are used here
         ! (the chain's final node, below, keeps the import's). A class the graph
         ! cannot create is reported now, not when the chain's Transform fails at
         ! compute; only a test that sets a bundle payload by hand gets past that.
         call ensure_export_payload(this, src_pt%component_name, effective_source_name(conn, dst_node), &
              this_graph, export_node_id, export_item, export_item, export_node%get_short_name(), &
              .not. is_evaluation, materialization_failure, chain_input=export_node, _RC)
         if (materialization_failure /= '') then
            call push_entry(unsupported, dst_pt, dst_node, materialization_failure)
            _RETURN(_SUCCESS)
         end if

         ! Chain *structure* above is unconditional; only the FieldCreate-based
         ! payload materialization is gated. When off (the default), extension
         ! items keep their unallocated placeholder payload.
         !
         ! materialize_extension_chain materializes every link in
         ! intermediate_node_ids from that link's own descriptor.
         if (materialize_extensions_enabled()) then
            call materialize_extension_chain(export_item, import_item, &
                 intermediate_node_ids, this_graph, &
                 materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! Distinguishable from "no matching export" and "unregistered
               ! characteristic" above.
               call push_entry(unsupported, dst_pt, dst_node, materialization_failure)
               _RETURN(_SUCCESS)
            end if
         end if

         if (record_second_producer(this_graph, final_node_id, import_node_id, dst_pt, dst_node, conflicts)) then
            _RETURN(_SUCCESS)
         end if
         call this_graph%add_dependency(net_id, final_node_id, import_node_id, _RC)

         _RETURN(_SUCCESS)
      end subroutine resolve_one_pair

   end subroutine resolve_match_connection

   ! GENERIC_INIT_ADVERTISE-time analog of legacy Connection%activate()
   ! (GraphBuilder.F90 module header, "Two-phase timing"): for each
   ! Match/SimpleConnection, determine which destination imports have a
   ! matching source export, WITHOUT creating any graph structure (edges are
   ! graphbuilder_resolve_connections' job, at connect() time). Read-only;
   ! purely populates the unresolved-imports report, which drives legacy's
   ! decision to bubble an unsatisfied import to the parent.
   module subroutine graphbuilder_check_unsatisfied_imports(this, unusable, unresolved_imports, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphResolutionEntryVector) :: unresolved

      call for_each_connection_pair(this, check_pair, _RC)

      if (present(unresolved_imports)) unresolved_imports = unresolved

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

      subroutine check_pair(conn, done, rc)
         class(Connection), intent(in) :: conn
         logical, intent(out) :: done
         integer, optional, intent(out) :: rc

         integer :: status

         done = .false.
         call check_match_connection_unsatisfied(this, conn, unresolved, _RC)

         _RETURN(_SUCCESS)
      end subroutine check_pair

   end subroutine graphbuilder_check_unsatisfied_imports

   ! Returns child_meta's imports that no connection resolves yet, which
   ! must be mirrored up into this_parent. "Resolved" cannot be read off
   ! dependency edges (none exist at the ADVERTISE-time point this runs;
   ! edges are added at ACCEPT_TRANSFER) nor off
   ! graphbuilder_check_unsatisfied_imports(child_meta) alone (it sees only
   ! connections child_meta itself declares, missing the most common shape: a
   ! connection on the direct parent naming child_meta). So an import counts
   ! as unresolved iff neither (a) a connection declared on child_meta whose
   ! destination resolves to child_meta, nor (b) one declared on this_parent
   ! whose destination resolves to child_meta, finds a source export for it
   ! (connection_resolves_import, below). Wrongly reporting a resolved import
   ! as unresolved would risk two producers of one NodeId once it is mirrored
   ! and later resolved again by an ancestor.
   ! child_meta MUST be a direct child of this_parent. Read-only.
   module function graphbuilder_unresolved_import_node_ids(this_parent, child_meta, rc) result(unresolved)
      class(OuterMetaComponent), target, intent(inout) :: this_parent
      class(OuterMetaComponent), target, intent(inout) :: child_meta
      integer, optional, intent(out) :: rc
      type(UnresolvedImportEntry), allocatable :: unresolved(:)

      integer :: status
      type(ComponentGraph), pointer :: child_graph
      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: port_iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node
      logical :: resolved
      type(UnresolvedImportEntry) :: entry
      type(UnresolvedImportEntry), allocatable :: grown(:)
      integer :: n

      allocate(unresolved(0))
      child_graph => child_meta%get_component_graph()
      import_ports = child_graph%get_import_ports()

      associate (e => import_ports%ftn_end())
         port_iter = import_ports%ftn_begin()
         do while (port_iter /= e)
            call port_iter%next()
            node_id = port_iter%second()
            generic_node => child_graph%get_node(node_id)
            _ASSERT(associated(generic_node), 'GraphBuilder: unresolved_import_node_ids - import port NodeId not found')
            select type (generic_node)
            class is (StateItemNode)
               if (.not. (generic_node%get_state_intent() == ESMF_STATEINTENT_IMPORT)) cycle
               ! An import with no graph-native resolution path (see
               ! is_ordinary_resolvable_import) is never propagated: no ancestor
               ! connection could resolve it.
               if (.not. is_ordinary_resolvable_import(generic_node)) cycle

               ! (a) resolved at child_meta's own level or below.
               resolved = connection_resolves_import(child_meta, child_meta, generic_node, _RC)
               ! (b) resolved by this_parent's own direct connection
               ! naming child_meta (the ordinary connect_all shape).
               if (.not. resolved) then
                  resolved = connection_resolves_import(this_parent, child_meta, generic_node, _RC)
               end if

               if (.not. resolved) then
                  entry%short_name = generic_node%get_short_name()
                  entry%node_id = node_id

                  ! Grow via a fresh temporary + move_alloc: a self-referencing array
                  ! constructor ICEs some compilers (as in collect_qualified_exports).
                  n = size(unresolved)
                  allocate(grown(n + 1))
                  if (n > 0) grown(1:n) = unresolved
                  grown(n + 1) = entry
                  call move_alloc(grown, unresolved)
               end if
            class default
               _FAIL('GraphBuilder: unresolved_import_node_ids - import port NodeId is not a StateItemNode')
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end function graphbuilder_unresolved_import_node_ids

   ! Shared by graphbuilder_unresolved_import_node_ids (cases (a) and (b)):
   ! does some Match/SimpleConnection declared on `owner`, whose destination
   ! resolves to `target_meta`, match `node`'s short name AND find a real
   ! source export? A positive collector: stops at the first resolving
   ! connection, so later connections are not evaluated. Read-only.
   !
   ! A connection naming a destination whose source does not exist must NOT
   ! count as resolved, or mirroring would be wrongly suppressed.
   ! ReexportConnection and other subtypes are never visited, so the import
   ! is still mirrored.
   function connection_resolves_import(owner, target_meta, node, rc) result(resolved)
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(OuterMetaComponent), target, intent(in) :: target_meta
      class(StateItemNode), target, intent(in) :: node
      integer, optional, intent(out) :: rc
      logical :: resolved

      integer :: status

      resolved = .false.
      call for_each_connection_pair(owner, try_pair, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine try_pair(conn, done, rc)
         class(Connection), intent(in) :: conn
         logical, intent(out) :: done
         integer, optional, intent(out) :: rc

         integer :: status
         type(ConnectionPt) :: dst_pt
         type(OuterMetaComponent), pointer :: dst_meta

         done = .false.
         dst_pt = conn%get_destination()

         dst_meta => resolve_qualified_component(owner, dst_pt%component_name, _RC)
         _RETURN_UNLESS(associated(dst_meta, target_meta))
         _RETURN_UNLESS(dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, node%get_short_name())))

         resolved = source_satisfies_import(owner, conn, node, _RC)
         done = resolved

         _RETURN(_SUCCESS)
      end subroutine try_pair

   end function connection_resolves_import

   ! Shared by check_match_connection_unsatisfied() and
   ! resolve_match_connection(): which of dst_graph's import ports does
   ! dst_pt's declared pattern match? Owns the loop and guard clauses; each
   ! caller supplies the per-match step as an internal procedure so it can
   ! host-associate its own context (same style as OuterMetaComponent's
   ! apply_to_children_custom()/I_child_op).
   !
   ! Iterates dst_graph's import-port table; each StateItemNode's
   ! short_name/state_intent feed the dst_pt%v_pt%matches filter. op has
   ! signature `subroutine(node, rc)`.
   module subroutine for_each_matching_import(dst_graph, dst_pt, op, rc)
      type(ComponentGraph), target, intent(in) :: dst_graph
      type(ConnectionPt), intent(in) :: dst_pt
      procedure(I_match_op) :: op
      integer, optional, intent(out) :: rc

      integer :: status
      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node

      import_ports = dst_graph%get_import_ports()

      associate (e => import_ports%ftn_end())
         iter = import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node_id = iter%second()
            generic_node => dst_graph%get_node(node_id)
            _ASSERT(associated(generic_node), 'GraphBuilder: for_each_matching_import - import port NodeId not found')
            select type (generic_node)
            class is (StateItemNode)
               if (.not. (generic_node%get_state_intent() == ESMF_STATEINTENT_IMPORT)) cycle
               if (.not. dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, generic_node%get_short_name()))) cycle
               call op(generic_node, _RC)
            class default
               _FAIL('GraphBuilder: for_each_matching_import - import port NodeId is not a StateItemNode')
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine for_each_matching_import

   ! GENERIC_INIT_ACCEPT_TRANSFER-time analog of legacy Connection%connect()
   ! (GraphBuilder.F90 module header, "Two-phase timing"): the real graph
   ! mutation, i.e. dependency edges. Handles MatchConnection and
   ! SimpleConnection; other subtypes are left untouched.
   !
   ! Idempotent: ESMF Provide/Accept/Realize negotiation can invoke
   ! ACCEPT_TRANSFER more than once per component. Legacy guards with a
   ! per-connection `consumed` flag; here connections_already_resolved()/
   ! mark_connections_resolved() make a repeat call a no-op instead of
   ! minting duplicate graph structure. graph%is_frozen() cannot serve as the
   ! guard: the graph is frozen once, hierarchy-wide, at the later
   ! GRAPH_FREEZE phase, so it is still mutable across both ACCEPT_TRANSFER
   ! passes.
    module subroutine graphbuilder_resolve_connections(this, unusable, unresolved_imports, &
         unsupported_characteristics, connection_conflicts, connection_warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      ! Matched export/import pairs whose mismatch has no registered extension
      ! provider (mapl_ExtensionResolution_mod); distinct from unresolved_imports
      ! ("no export at all").
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported_characteristics
      type(GraphResolutionEntryVector), optional, intent(out) :: connection_conflicts
      type(GraphResolutionEntryVector), optional, intent(out) :: connection_warnings
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphResolutionEntryVector) :: unresolved
      type(GraphResolutionEntryVector) :: unsupported
      type(GraphResolutionEntryVector) :: conflicts
      type(GraphResolutionEntryVector) :: warnings

      if (this%connections_already_resolved()) then
         if (present(unresolved_imports)) unresolved_imports = unresolved
         if (present(unsupported_characteristics)) unsupported_characteristics = unsupported
         if (present(connection_conflicts)) connection_conflicts = conflicts
         if (present(connection_warnings)) connection_warnings = warnings
         _RETURN(_SUCCESS)
      end if

      ! A connection pairs this component's own items with a descendant's
      ! (resolve_qualified_component, find_export_node_id): a side still
      ! DEFERRED there would otherwise wait forever, since nothing else
      ! visits that descendant's own graph from here. Idempotent with the
      ! connect hook's own per-component call (graphbuilder_run_connect_hook):
      ! a descendant already handled through its own hook has nothing left
      ! to fall back.
      call fallback_unsupplied_deferred_tree(this, _RC)

      call this%begin_connection_pass()
      call for_each_connection_pair(this, resolve_pair, _RC)

      call this%end_connection_pass()

      if (present(unresolved_imports)) unresolved_imports = unresolved
      if (present(unsupported_characteristics)) unsupported_characteristics = unsupported
      if (present(connection_conflicts)) connection_conflicts = conflicts
      if (present(connection_warnings)) connection_warnings = warnings

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

      subroutine resolve_pair(conn, done, rc)
         class(Connection), intent(in) :: conn
         logical, intent(out) :: done
         integer, optional, intent(out) :: rc

         integer :: status

         done = .false.
         call resolve_match_connection(this, conn, unresolved, unsupported, conflicts, warnings, _RC)

         _RETURN(_SUCCESS)
      end subroutine resolve_pair

   end subroutine graphbuilder_resolve_connections

   ! Applies graphbuilder_fallback_unsupplied_deferred to meta and every
   ! descendant, so a connection resolved directly (bypassing each
   ! descendant's own connect hook) still sees a non-fatal DEFERRED side
   ! converted before matching.
   subroutine fallback_unsupplied_deferred_tree(meta, rc)
      class(OuterMetaComponent), target, intent(inout) :: meta
      integer, optional, intent(out) :: rc

      integer :: status

      call for_each_descendant(meta, fallback_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine fallback_one(component, descend, rc)
         class(OuterMetaComponent), target, intent(inout) :: component
         logical, intent(out) :: descend
         integer, optional, intent(out) :: rc

         integer :: status, j
         type(GraphResolutionEntryVector) :: fallback
         type(GraphResolutionEntry), pointer :: entry
         class(Logger), pointer :: lgr
         character(:), allocatable :: msg

         descend = .true.
         call graphbuilder_fallback_unsupplied_deferred(component, fallback, _RC)
         lgr => component%get_logger()
         do j = 1, fallback%size()
            entry => fallback%of(j)
            msg = 'GraphBuilder: ' // entry%component_name // ': ' // entry%reason
            call lgr%warning(msg)
         end do

         _RETURN(_SUCCESS)
      end subroutine fallback_one

   end subroutine fallback_unsupplied_deferred_tree

end submodule GraphBuilder_ConnectionMatching_smod
