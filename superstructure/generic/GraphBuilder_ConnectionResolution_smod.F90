#include "MAPL.h"

! Ordinary connection resolution: wires SimpleConnection/MatchConnection
! destinations to export nodes, propagates unresolved imports and exports up
! the hierarchy, and resolves expression, callback, wildcard and inout
! destinations.
!
! Identity (short_name, intent, inout/callback markers) and the mismatch
! characteristics are read off each already-advertised StateItemNode, never
! re-derived from a VariableSpec.
submodule (mapl_GraphBuilder_mod) GraphBuilder_ConnectionResolution_smod
   use esmf, only: ESMF_State, ESMF_StateCreate, ESMF_StateGet, ESMF_STATEITEM_NOTFOUND, operator(==)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator, operator(/=)
   use mapl_ExpressionTransform_mod, only: ExpressionTransform
   use mapl_ExtensionResolution_mod, only: find_or_build_extension_chain, materialize_extensions_enabled
   use mapl_StateItemDescriptor_mod, only: resolve_descriptors
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap, PortIdNodeIdMapIterator, operator(/=)
   use mapl_PortId_mod, only: PortId
   use mapl_StateArithmeticParser_mod, only: parser_variables_in_expression
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use gFTL2_StringVector, only: StringVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! The one place a Connection's concrete subtype matters for resolution.
   ! SimpleConnection: its declared source name, which may differ from the
   ! destination's name. MatchConnection (class default): the destination's own
   ! name (the same-name wildcard semantic of connection/MatchConnection.F90).
   ! Callers stay subtype-agnostic by going through this function.
   function effective_source_name(conn, dst_node) result(name)
      class(Connection), intent(in) :: conn
      class(StateItemNode), intent(in) :: dst_node
      character(:), allocatable :: name

      type(ConnectionPt) :: source_pt

      select type (conn)
      class is (SimpleConnection)
         ! NAG rejects `f()%method()` on a function result; use a local temporary.
         source_pt = conn%get_source()
         name = source_pt%get_esmf_name()
      class default
         name = dst_node%get_short_name()
      end select
   end function effective_source_name

   ! Read-only counterpart of resolve_wildcard_destination's matching loop,
   ! shared by the "already resolved?" checks (check_one,
   ! connection_pt_pair_resolves). The exact-name export lookup cannot work
   ! for a WILDCARD import: its short name is the regex pattern itself. The
   ! pattern is rebuilt here with EXPORT intent because the connection's own
   ! dst_pt%v_pt is IMPORT-intent and so never matches an export.
   function wildcard_import_has_match(owner, src_component_name, node, rc) result(has_match)
      class(OuterMetaComponent), target, intent(inout) :: owner
      character(*), intent(in) :: src_component_name
      class(StateItemNode), intent(in) :: node
      integer, optional, intent(out) :: rc
      logical :: has_match

      integer :: status
      class(OuterMetaComponent), pointer :: root_meta
      type(QualifiedExportEntry), allocatable :: namespace(:)
      type(VirtualConnectionPt) :: wildcard_v_pt
      type(VirtualConnectionPt) :: candidate_v_pt
      integer :: i

      has_match = .false.

      wildcard_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, node%get_short_name())

      root_meta => component_for(owner, src_component_name, _RC)
      namespace = build_qualified_export_namespace(root_meta, _RC)

      do i = 1, size(namespace)
         candidate_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, namespace(i)%qualified_name)
         if (.not. wildcard_v_pt%matches(candidate_v_pt)) cycle
         has_match = .true.
         exit
      end do

      _RETURN(_SUCCESS)
   end function wildcard_import_has_match

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
         logical :: has_export
         logical :: has_match
         type(CallbackInterfaceId) :: callback_interface_id
         type(QualifiedExportEntry), allocatable :: callback_matches(:)
         type(StateItemDescriptor), pointer :: node_descriptor

         ! A destination declaring an expected callback interface is checked
         ! against the flattened namespace, not by exact name. Read-only:
         ! resolve_callback_import never mutates the graph.
         !
         ! NAG rejects `f()%method()` on a function result; use a local temporary.
         callback_interface_id = node%get_callback_interface_id()
         if (callback_interface_id%is_valid()) then
            callback_matches = resolve_callback_import(this, src_pt, callback_interface_id, rc=status)
            _VERIFY(status)
            if (size(callback_matches) == 0) then
               call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, node%get_short_name()))
            end if
            _RETURN(_SUCCESS)
         end if

         ! WILDCARD destinations use wildcard_import_has_match: the exact-name
         ! find_export_node fallback below can never match a regex short name.
         node_descriptor => node%get_descriptor()
         if (node_descriptor%get_declared_item_type() == MAPL_STATEITEM_WILDCARD) then
            has_match = wildcard_import_has_match(this, src_pt%component_name, node, _RC)
            if (.not. has_match) then
               call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, node%get_short_name()))
            end if
            _RETURN(_SUCCESS)
         end if

         has_export = associated(find_export_node(src_meta, effective_source_name(conn, node)))
         if (.not. has_export) then
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, node%get_short_name()))
         end if

         _RETURN(_SUCCESS)
      end subroutine check_one

   end subroutine check_match_connection_unsatisfied

   ! Mirrors MatchConnection%activate() (connection/MatchConnection.F90):
   ! filter the destination's imports against the declared pattern, then for
   ! each match require an EXACT-name export on the source side. Computed
   ! against the destination ComponentGraph's import-port table rather than
   ! StateRegistry's subregistry machinery. VirtualConnectionPt%matches() and
   ! the ConnectionPt/VirtualConnectionPt accessors are plain queries, used
   ! as-is.
   !
   ! Accepts any Connection subtype: a SimpleConnection is the exact-match
   ! case of MatchConnection's pattern expansion, with no separate code path.
   module subroutine resolve_match_connection(this, conn, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
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
      net_id = this_graph%get_default_network_id()

      call for_each_matching_import(dst_graph, dst_pt, resolve_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine resolve_one(dst_node, rc)
         class(StateItemNode), target, intent(in) :: dst_node
         integer, optional, intent(out) :: rc

         integer :: status
         type(NodeId) :: import_node_id, export_node_id, final_node_id
         type(NodeId), allocatable :: intermediate_node_ids(:)
         logical :: has_export
         ! Set by try_alias_export_field. When true the export node already has a
         ! real field, so try_materialize_field_extension is skipped (its
         ! FieldCreate would overwrite the alias).
         logical :: alias_found
         class(StateItemNode), pointer :: export_node
         type(StateItemDescriptor), pointer :: export_item, import_item
         type(StateItemCharacteristicKind), allocatable :: mismatched(:)
         character(:), allocatable :: unsupported_characteristic
         character(:), allocatable :: materialization_failure
         type(StringVector) :: callback_rejected
         character(:), pointer :: rejected_item
         integer :: j
         type(CallbackInterfaceId) :: callback_interface_id
         type(StateItemDescriptor), pointer :: dst_descriptor
         character(:), allocatable :: status_conflict

         ! A callback interface and inout borrower intent are mutually exclusive
         ! resolution paths; reject the combination before either branch so
         ! neither silently wins.
         callback_interface_id = dst_node%get_callback_interface_id()
         if (callback_interface_id%is_valid() .and. dst_node%is_inout_borrower()) then
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 'declares both an expected callback interface and inout borrower intent (mutually exclusive)'))
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
                call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                     rejected_item))
             end do
             _RETURN(_SUCCESS)
          end if

          ! An inout borrower is resolved by resolve_inout_destination instead of
          ! the exact-name/extension-chain logic below. It takes the precomputed
          ! effective source name because a SimpleConnection's source name may
          ! differ from dst_node's.
          if (dst_node%is_inout_borrower()) then
             call resolve_inout_destination(this, src_pt, dst_pt, dst_node, effective_source_name(conn, dst_node), unsupported, _RC)
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
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name()))
            _RETURN(_SUCCESS)
         end if

         export_item => export_node%get_descriptor()
         import_item => mutable_descriptor(dst_graph, import_node_id, _RC)
         call resolve_descriptors(export_item, import_item, mismatched, status_conflict, _RC)
         if (status_conflict /= '') then
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 status_conflict))
            _RETURN(_SUCCESS)
         end if

         if (size(mismatched) == 0) then
            ! Exact match: wire directly, no extension.
            call this_graph%add_dependency(net_id, export_node_id, import_node_id, _RC)

            ! Give the export node a real, allocated field using the same geom/vgrid
            ! resolution as the mismatch branch. Gated on graph mode, not on
            ! materialize_extensions_enabled() (an orthogonal gate); no-op in
            ! GRAPH_MODE_REGISTRY.
            if (graph_mode() /= GRAPH_MODE_REGISTRY) then
               ! Try aliasing the export component's real field first. If found, the
               ! node already has a real field and try_materialize_field_extension must
               ! be skipped: its FieldCreate would overwrite the alias.
               call try_alias_export_field(this, src_pt%component_name, export_node%get_short_name(), &
                    this_graph, export_node_id, alias_found, _RC)
               if (.not. alias_found) then
                  call try_materialize_field_extension(this, src_pt%component_name, export_item, import_item, &
                       this_graph, export_node_id, materialization_failure, name=export_node%get_short_name(), _RC)
                  if (materialization_failure /= '') then
                     ! The edge above still exists; only payload materialization failed.
                     call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                          materialization_failure))
                  end if
               end if
            end if

            _RETURN(_SUCCESS)
         end if

         ! Mismatch: delegate to extension-chain construction
         ! (mapl_ExtensionResolution_mod) rather than wiring the pair directly.
         call find_or_build_extension_chain(this_graph, net_id, export_node_id, &
              export_item%get_characteristics(), import_item%get_characteristics(), mismatched, &
              final_node_id, intermediate_node_ids, unsupported_characteristic, _RC)

         if (unsupported_characteristic /= '') then
            ! Distinguishable from "import has no matching export".
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 unsupported_characteristic))
            _RETURN(_SUCCESS)
         end if

         ! The chain's input node (export_node_id) must be materialized too: the
         ! chain's compute() fails on an unallocated input. export_item is passed
         ! as BOTH export and import so try_materialize_field_extension uses the
         ! export's typekind/units here (the chain's final node, below, keeps the
         ! import's). Gated on graph_mode() like the exact-match branch.
         if (graph_mode() /= GRAPH_MODE_REGISTRY) then
            ! Same alias-first treatment as the exact-match branch.
            call try_alias_export_field(this, src_pt%component_name, export_node%get_short_name(), &
                 this_graph, export_node_id, alias_found, _RC)
            if (.not. alias_found) then
               ! export_item is passed as both export and import, so loosening the
               ! export-side gate would reach the FIELD-only import-side check and
               ! abandon the connection. A non-FIELD (e.g. bracket) input has no
               ! field-shaped version to materialize, so skip it here and let the
               ! chain's own materialization below run.
               if (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD) then
                  call try_materialize_field_extension(this, src_pt%component_name, export_item, export_item, &
                       this_graph, export_node_id, materialization_failure, name=export_node%get_short_name(), _RC)
                  if (materialization_failure /= '') then
                     call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                          materialization_failure))
                     _RETURN(_SUCCESS)
                  end if
               end if
            end if
         end if

         ! Chain *structure* above is unconditional; only the FieldCreate-based
         ! payload materialization is gated. When off (the default), extension
         ! items keep their unallocated placeholder payload.
         !
         ! materialize_extension_chain materializes every link in
         ! intermediate_node_ids with that link's own evolved typekind/units.
         if (materialize_extensions_enabled()) then
            call materialize_extension_chain(this, src_pt%component_name, export_item, import_item, &
                 mismatched, intermediate_node_ids, this_graph, materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! Distinguishable from "no matching export" and "unregistered
               ! characteristic" above.
               call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                    materialization_failure))
               _RETURN(_SUCCESS)
            end if
         end if

         call this_graph%add_dependency(net_id, final_node_id, import_node_id, _RC)

         _RETURN(_SUCCESS)
      end subroutine resolve_one

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
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(GraphResolutionEntryVector) :: unresolved

      comp_spec => this%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call check_match_connection_unsatisfied(this, c, unresolved, _RC)
            class is (SimpleConnection)
               ! Same call as the MatchConnection arm: check_match_connection_unsatisfied
               ! only uses Connection-base accessors.
               call check_match_connection_unsatisfied(this, c, unresolved, _RC)
            class default
               ! Other subtypes (e.g. ReexportConnection) are not resolved here.
            end select
         end do
      end associate

      if (present(unresolved_imports)) unresolved_imports = unresolved

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
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
   ! source export? A positive collector: returns at the first resolving
   ! connection. Read-only.
   !
   ! The per-connection check (connection_pt_pair_resolves) is shared by both
   ! subtypes. A connection naming a destination whose source does not exist
   ! must NOT count as resolved, or mirroring would be wrongly suppressed.
   ! ReexportConnection and other subtypes fall through unresolved, so the
   ! import is still mirrored.
   function connection_resolves_import(owner, target_meta, node, rc) result(resolved)
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(OuterMetaComponent), target, intent(in) :: target_meta
      class(StateItemNode), target, intent(in) :: node
      integer, optional, intent(out) :: rc
      logical :: resolved

      integer :: status
      type(ComponentSpec), pointer :: owner_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      resolved = .false.
      owner_spec => owner%get_component_spec()

      associate (e => owner_spec%connections%ftn_end())
         iter = owner_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               resolved = connection_pt_pair_resolves(owner, target_meta, node, c, _RC)
            class is (SimpleConnection)
               ! Exact (non-regex) destination; same shared check as a MatchConnection
               ! match.
               resolved = connection_pt_pair_resolves(owner, target_meta, node, c, _RC)
            class default
               ! ReexportConnection and others: not resolved here, so still mirrored.
            end select
            if (resolved) exit
         end do
      end associate

      _RETURN(_SUCCESS)
   end function connection_resolves_import

   ! The source-export lookup uses effective_source_name: a
   ! SimpleConnection's source name may differ from node's (destination-side)
   ! short name.
   function connection_pt_pair_resolves(owner, target_meta, node, c, rc) result(resolved)
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(OuterMetaComponent), target, intent(in) :: target_meta
      class(StateItemNode), target, intent(in) :: node
      class(Connection), intent(in) :: c
      integer, optional, intent(out) :: rc
      logical :: resolved

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: dst_meta, src_meta
      type(CallbackInterfaceId) :: callback_interface_id
      type(QualifiedExportEntry), allocatable :: callback_matches(:)
      logical :: has_export
      type(StateItemDescriptor), pointer :: node_descriptor

      resolved = .false.

      src_pt = c%get_source()
      dst_pt = c%get_destination()

      dst_meta => resolve_qualified_component(owner, dst_pt%component_name, _RC)
      if (.not. associated(dst_meta, target_meta)) then
         _RETURN(_SUCCESS)
      end if
      if (.not. dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, node%get_short_name()))) then
         _RETURN(_SUCCESS)
      end if

      callback_interface_id = node%get_callback_interface_id()
      if (callback_interface_id%is_valid()) then
         callback_matches = resolve_callback_import(owner, src_pt, callback_interface_id, rc=status)
         _VERIFY(status)
         resolved = (size(callback_matches) > 0)
         _RETURN(_SUCCESS)
      end if

      ! Same wildcard_import_has_match substitution as check_one.
      node_descriptor => node%get_descriptor()
      if (node_descriptor%get_declared_item_type() == MAPL_STATEITEM_WILDCARD) then
         resolved = wildcard_import_has_match(owner, src_pt%component_name, node, _RC)
         _RETURN(_SUCCESS)
      end if

      src_meta => resolve_qualified_component(owner, src_pt%component_name, _RC)
      has_export = associated(find_export_node(src_meta, effective_source_name(c, node)))
      resolved = has_export

      _RETURN(_SUCCESS)
   end function connection_pt_pair_resolves

   ! For each direct child C of `this`, obtains C's still-unresolved imports
   ! (graphbuilder_unresolved_import_node_ids, via the framework-internal
   ! get_child_component_graph carve-out) and mirrors each into `this`'s
   ! import-port table under C's own NodeId: no new node or proxy, so
   ! for_each_matching_import finds it like any import `this` advertised.
   !
   ! Idempotent: ESMF Provide/Accept/Realize negotiation can invoke
   ! GENERIC_INIT_ADVERTISE (and so this hook) more than once per component;
   ! unguarded, it hangs under gfortran and re-logs warnings. A NodeId already
   ! present as a VALUE in this import-port table is skipped
   ! (already_mirrored); a repeat call re-derives the same set, so that check
   ! is sufficient.
   module subroutine graphbuilder_run_propagate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(ComponentGraph), pointer :: this_graph
      type(UnresolvedImportEntry), allocatable :: unresolved(:)
      type(PortId) :: port_id
      integer :: j

      this_graph => this%get_component_graph()
      num_children = this%get_num_children()

      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'run_propagate_hook:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'run_propagate_hook:get_child_outer_meta', status)
         if (status /= 0) cycle

         unresolved = graphbuilder_unresolved_import_node_ids(this, child_meta, status)
         call report_if_failed(this, 'run_propagate_hook:unresolved_import_node_ids', status)
         if (status /= 0) cycle

         do j = 1, size(unresolved)
            if (already_mirrored(this_graph, unresolved(j)%node_id)) cycle

            port_id = this_graph%next_port_id(status)
            call report_if_failed(this, 'run_propagate_hook:next_port_id', status)
            if (status /= 0) cycle
            call this_graph%add_import_port(port_id, unresolved(j)%node_id, status)
            call report_if_failed(this, 'run_propagate_hook:add_import_port', status)
         end do
      end do
   end subroutine graphbuilder_run_propagate_hook

   ! Idempotency helper for graphbuilder_run_propagate_hook above: does
   ! graph's own import-port table already contain target_id as a
   ! VALUE (not a key - PortId keys are always freshly minted, so a key
   ! check would never match)? A plain linear scan - this table is
   ! bounded by one component's own import count, not hierarchy-wide.
   function already_mirrored(graph, target_id) result(found)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: target_id
      logical :: found

      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: candidate

      found = .false.
      import_ports = graph%get_import_ports()
      associate (e => import_ports%ftn_end())
         iter = import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate = iter%second()
            if (candidate == target_id) then
               found = .true.
               return
            end if
         end do
      end associate
   end function already_mirrored

   ! For each direct child C of `this`, propagates every export-port entry of
   ! C into `this`'s export-port table under C's own NodeId, with the
   ! qualified name from qualify_exported_name. Unconditional (no "already
   ! resolved" gate): an export's usefulness depends on whether some
   ! descendant's import matches it, which is not knowable here.
   module subroutine graphbuilder_run_export_propagate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(PortIdNodeIdMap), target :: child_export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: child_port_id
      type(NodeId) :: export_node_id
      character(:), allocatable :: qualified_name
      type(PortId) :: new_port_id

      this_graph => this%get_component_graph()
      num_children = this%get_num_children()

      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'run_export_propagate_hook:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'run_export_propagate_hook:get_child_outer_meta', status)
         if (status /= 0) cycle

         child_graph => child_meta%get_component_graph()
         child_export_ports = child_graph%get_export_ports()

         associate (e => child_export_ports%ftn_end())
            iter = child_export_ports%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_port_id = iter%first()
               export_node_id = iter%second()

               qualified_name = qualify_exported_name(child_graph, child_port_id, child_name)
               if (.not. allocated(qualified_name)) cycle

               if (already_exported(this_graph, export_node_id, qualified_name)) cycle

               new_port_id = this_graph%next_port_id(status)
               call report_if_failed(this, 'run_export_propagate_hook:next_port_id', status)
               if (status /= 0) cycle
               call this_graph%add_export_port(new_port_id, export_node_id, status, &
                    qualified_name=qualified_name)
               call report_if_failed(this, 'run_export_propagate_hook:add_export_port', status)
            end do
         end associate
      end do
   end subroutine graphbuilder_run_export_propagate_hook

   ! Name a child's export-port entry carries once propagated into the
   ! parent's export-port table. Mirrors legacy propagate_exports_virtual_pt:
   ! a name already qualified (propagated up from a grandchild) passes
   ! through unchanged; a child's own direct export is qualified as
   ! child_name // '/' // short_name.
   function qualify_exported_name(child_graph, child_port_id, child_name) result(qualified_name)
      type(ComponentGraph), target, intent(in) :: child_graph
      type(PortId), intent(in) :: child_port_id
      character(*), intent(in) :: child_name
      character(:), allocatable :: qualified_name

      character(:), allocatable :: own_name

      own_name = child_graph%get_export_port_name(child_port_id)
      if (.not. allocated(own_name)) return

      ! An override-carrying name was set by an earlier propagation level and
      ! always has the form "descendant/short_name". get_export_port_name does
      ! not expose whether an override exists, so '/' in the name is the
      ! signal; child names are plain identifiers with no '/'.
      if (index(own_name, '/') > 0) then
         qualified_name = own_name
      else
         qualified_name = child_name // '/' // own_name
      end if
   end function qualify_exported_name

   ! Idempotency helper for graphbuilder_run_export_propagate_hook, symmetric
   ! to already_mirrored: is (target_id, target_name) already present in
   ! graph's export-port table? Both must be checked: in a diamond-shaped
   ! hierarchy one export NodeId can legitimately appear under several
   ! qualified names; only an identical pair indicates a repeat ADVERTISE
   ! call.
   function already_exported(graph, target_id, target_name) result(found)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: target_id
      character(*), intent(in) :: target_name
      logical :: found

      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: candidate_port_id
      type(NodeId) :: candidate_id
      character(:), allocatable :: candidate_name

      found = .false.
      export_ports = graph%get_export_ports()
      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_port_id = iter%first()
            candidate_id = iter%second()
            if (.not. (candidate_id == target_id)) cycle
            candidate_name = graph%get_export_port_name(candidate_port_id)
            if (.not. allocated(candidate_name)) cycle
            if (candidate_name == target_name) then
               found = .true.
               return
            end if
         end do
      end associate
   end function already_exported

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
         unsupported_characteristics, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      ! Matched export/import pairs whose mismatch has no registered extension
      ! provider (mapl_ExtensionResolution_mod); distinct from unresolved_imports
      ! ("no export at all").
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported_characteristics
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(GraphResolutionEntryVector) :: unresolved
      type(GraphResolutionEntryVector) :: unsupported

      if (this%connections_already_resolved()) then
         if (present(unresolved_imports)) unresolved_imports = unresolved
         if (present(unsupported_characteristics)) unsupported_characteristics = unsupported
         _RETURN(_SUCCESS)
      end if

      comp_spec => this%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call resolve_match_connection(this, c, unresolved, unsupported, _RC)
            class is (SimpleConnection)
               ! Same call as the MatchConnection arm: resolve_match_connection only
               ! uses Connection-base accessors.
               call resolve_match_connection(this, c, unresolved, unsupported, _RC)
            class default
               ! Other subtypes (e.g. ReexportConnection) are not resolved here.
            end select
         end do
      end associate

      call this%mark_connections_resolved()

      if (present(unresolved_imports)) unresolved_imports = unresolved
      if (present(unsupported_characteristics)) unsupported_characteristics = unsupported

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_resolve_connections


   ! Read-only scan of src_meta's ComponentGraph export-port table for a node
   ! whose effective port name matches short_name. Used by
   ! find_export_node_id (which then mutates the graph) and by check_one,
   ! which must NOT create graph structure.
   !
   ! Matches against get_export_port_name (the export-propagation override
   ! when one exists, else the node's short_name), not the node's
   ! short_name: a propagated export's StateItemNode%short_name stays
   ! unqualified, while this graph's port-table entry carries the
   ! ancestor-qualified name (`child_name/short_name`), mirroring legacy
   ! propagate_exports_virtual_pt.
   function find_export_node(src_meta, short_name) result(export_node)
      class(OuterMetaComponent), target, intent(inout) :: src_meta
      character(*), intent(in) :: short_name
      class(StateItemNode), pointer :: export_node

      type(ComponentGraph), pointer :: src_graph
      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: candidate_port_id
      type(NodeId) :: candidate_id
      class(GraphNode), pointer :: candidate_node
      character(:), allocatable :: candidate_name

      export_node => null()

      src_graph => src_meta%get_component_graph()
      export_ports = src_graph%get_export_ports()

      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_port_id = iter%first()
            candidate_id = iter%second()
            candidate_node => src_graph%get_node(candidate_id)
            if (.not. associated(candidate_node)) cycle
            select type (candidate_node)
            class is (StateItemNode)
               candidate_name = src_graph%get_export_port_name(candidate_port_id)
               if (.not. allocated(candidate_name)) cycle
               if (candidate_name == short_name) then
                  export_node => candidate_node
                  return
               end if
            class default
               ! Not a StateItemNode: not an export candidate; skip.
            end select
         end do
      end associate
   end function find_export_node

   ! export_node is the exact match find_export_node found in src_meta's
   ! export-port table; use its NodeId directly. A resource-index lookup
   ! keyed by short_name would fail for a PROPAGATED export: the resource
   ! index only has the unqualified short_name, never the qualified name
   ! find_export_node matches against.
   logical module function find_export_node_id(this, comp_name, short_name, export_node_id, export_node, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      class(StateItemNode), pointer, intent(out) :: export_node
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: src_meta

      src_meta => resolve_qualified_component(this, comp_name, _RC)
      export_node => find_export_node(src_meta, short_name)
      found = associated(export_node)

      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      export_node_id = export_node%get_node_id()

      _RETURN(_SUCCESS)
   end function find_export_node_id

   ! Resolves a destination declared an ordinary inout borrower (direct-alias
   ! case only). Reuses find_export_node_id/resolve_descriptors
   ! as resolve_one's ordinary path does for the exact-match check. On an
   ! exact match, adds the forward edge (owner -> borrower) in the default
   ! network and a return edge (borrower -> owner) in a fresh per-pairing
   ! network, so no network ever has two producers of one NodeId. Shapes
   ! reserved for a future design (mismatched payload, no identifiable
   ! owner, chained borrowing) are rejected via `unsupported`, never
   ! delegated to extension-chain construction. Only graph structure is
   ! built here; the runtime return-edge propagation trigger does not exist
   ! yet.
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, dst_node, effective_src_name, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
      character(*), intent(in) :: effective_src_name
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
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

      graph => this%get_component_graph()
      net_id = graph%get_default_network_id()

      import_node_id = resolve_local_node_id(this, dst_pt%component_name, &
           ESMF_STATEINTENT_IMPORT, dst_node%get_short_name(), _RC)

      ! Source lookup key is the effective source name (see resolve_one).
      has_owner = find_export_node_id(this, src_pt%component_name, effective_src_name, &
           owner_node_id, owner_node, _RC)

      if (.not. has_owner) then
         ! No identifiable owner: reported, never silently treated as an ordinary
         ! import.
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              'inout borrower has no identifiable owner'))
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
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              'inout borrower owner is itself a declared inout borrower (chained borrowing)'))
         _RETURN(_SUCCESS)
      end if

      owner_item => owner_node%get_descriptor()
      borrower_item => mutable_descriptor(graph, import_node_id, _RC)
      call resolve_descriptors(owner_item, borrower_item, mismatched, status_conflict, _RC)
      if (status_conflict /= '') then
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              status_conflict))
         _RETURN(_SUCCESS)
      end if

      if (size(mismatched) /= 0) then
         ! A mismatched pairing is rejected rather than chained: the
         ! non-direct-alias case is reserved, so extension-chain construction must
         ! not be used.
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              'inout borrower payload does not exactly match its owner (non-direct-alias case not supported)'))
         _RETURN(_SUCCESS)
      end if

      ! Direct-alias case: forward edge in the default network (as in the
      ! ordinary exact-match case) plus a return edge in a fresh per-pairing
      ! network. No TransformGraphNode for either.
      call graph%add_dependency(net_id, owner_node_id, import_node_id, _RC)

      return_net_id = graph%create_network(_RC)
      call graph%add_dependency(return_net_id, import_node_id, owner_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_inout_destination

   ! Materializes node_id's payload as a MAPL_NamedAlias of the export
   ! component's real, already-realized field, read by name from that
   ! component's export ESMF_State (no StateRegistry). Tried BEFORE
   ! try_materialize_field_extension at resolve_one's export_node_id call
   ! sites: if found, the caller skips the FieldCreate path. If the field is
   ! not yet available by name (two-pass timing), returns success with
   ! found = .false. and the caller falls back to FieldCreate.
   module subroutine try_alias_export_field(this, export_comp_name, export_short_name, &
        graph, node_id, found, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      character(*), intent(in) :: export_short_name
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      class(OuterMetaComponent), pointer :: export_meta
      type(GriddedComponentDriver), pointer :: export_driver
      type(MultiState) :: export_states
      type(ESMF_State) :: export_state
      type(ESMF_StateItem_Flag) :: item_kind
      type(ESMF_Field) :: field
      type(ESMF_Field) :: alias
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload

      found = .false.

      export_meta => component_for(this, export_comp_name, _RC)
      export_driver => export_meta%get_user_gc_driver()
      export_states = export_driver%get_states()
      call export_states%get_state(export_state, ESMF_STATEINTENT_EXPORT, _RC)

      ! Plain by-name read; an absent item is not a failure (the field may not
      ! exist yet). Checked via itemType, as FieldClassAspect%add_to_state
      ! does, rather than trusting ESMF_StateGet's rc.
      call ESMF_StateGet(export_state, itemName=export_short_name, itemType=item_kind, _RC)
      if (item_kind == ESMF_STATEITEM_NOTFOUND) then
         _RETURN(_SUCCESS)
      end if

      call ESMF_StateGet(export_state, itemName=export_short_name, field=field, _RC)

      ! MAPL_NamedAlias (not a bare ESMF_NamedAlias) carries MAPL's per-alias
      ! metadata (standard_name/long_name/restart_mode), as populate_one does.
      alias = MAPL_NamedAlias(field, name=export_short_name, _RC)

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: try_alias_export_field - NodeId not found in graph')
      select type (node)
      class is (StateItemNode)
         ! Mutate the stored handle in place through a pointer rather than a
         ! get_payload()/set_payload() round trip (gfortran copy hazard).
         payload => node%get_payload_ptr()
         call payload%set(alias, _RC)
      class default
         _FAIL('GraphBuilder: try_alias_export_field - NodeId does not refer to a StateItemNode')
      end select

      found = .true.

      _RETURN(_SUCCESS)
   end subroutine try_alias_export_field

   ! Gives a resolved extension chain's final item a real ESMF_Field (via
   ! FieldCreate) for a Field-typed export/import pair. Otherwise sets
   ! failure_reason (empty on success): a non-Field item class on either
   ! side, or an unresolvable geom.
   ! Optional `name` is passed through to materialize_field_extension so
   ! graph-native state population can find the field by name once it is
   ! inserted into an ESMF_State.
   !
   ! export_item/import_item are the two sides' GraphStateItem payloads; item
   ! type, units, typekind, ungridded_dims and vertical_stagger are read from
   ! them directly, with no VariableSpec lookup.
   subroutine try_materialize_field_extension(this, export_comp_name, export_item, import_item, &
        graph, node_id, failure_reason, name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(:), allocatable, intent(out) :: failure_reason
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(OuterMetaComponent), pointer :: export_meta
      type(ESMF_Geom) :: geom
      logical :: has_geom
      class(VerticalGrid), allocatable :: vgrid
      type(VerticalStaggerLoc) :: vert_staggerloc
      type(VerticalStaggerLoc), allocatable :: declared_vertical_stagger
      type(ESMF_TypeKind_Flag) :: import_typekind
      character(:), allocatable :: import_units

      failure_reason = ''

      ! The export side admits BRACKET alongside FIELD: a bracket export
      ! resolved through ItemShapeCharacteristic's chain into a field import is
      ! materialized via the same FieldCreate path (always a plain Field). The
      ! import side stays FIELD-only: a bracket is never a valid import shape.
      if (.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      if (.not. (import_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(import_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      import_units = get_item_units(import_item, _RC)
      _ASSERT(import_units /= '', 'GraphBuilder: materialization reached with no import units - internal inconsistency')

      export_meta => component_for(this, export_comp_name, _RC)

      call resolve_export_geom(export_item, export_meta, geom, has_geom, _RC)
      if (.not. has_geom) then
         ! Neither the export's declared geom nor the owning OuterMetaComponent has
         ! a concrete geom; distinguishable from "unsupported item class" and
         ! "unregistered characteristic".
         failure_reason = 'unresolved_geom'
         _RETURN(_SUCCESS)
      end if

      ! Vertical grid is optional (a 2D field legitimately has none,
      ! same as FieldCreate's own optional vgrid argument) - unlike geom,
      ! its absence is not a failure. When present, a vert_staggerloc is
      ! required alongside it (FieldCreate's own contract); default to
      ! CENTER when the export's own declared vertical_stagger does not
      ! say otherwise (VariableSpec.F90's own make_VerticalGridAspect
      ! precedent).
      call resolve_export_vgrid(export_item, export_meta, vgrid)
      import_typekind = get_item_typekind(import_item, _RC)
      if (allocated(vgrid)) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         call export_item%get_vertical_stagger(declared_vertical_stagger)
         if (allocated(declared_vertical_stagger)) vert_staggerloc = declared_vertical_stagger

         ! typekind/units are the IMPORT's values: the materialized payload must
         ! match what the import requires, not what the export produced.
         call materialize_field_extension(graph, node_id, geom=geom, typekind=import_typekind, &
              units=import_units, ungridded_dims=export_item%get_ungridded_dims(), &
              vgrid=vgrid, vert_staggerloc=vert_staggerloc, name=name, _RC)
      else
         call materialize_field_extension(graph, node_id, geom=geom, typekind=import_typekind, &
              units=import_units, ungridded_dims=export_item%get_ungridded_dims(), name=name, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine try_materialize_field_extension

   ! Recovers a GraphStateItem's PHYSICAL_UNITS characteristic as a plain
   ! string. Returns '' if never established: an item advertised with no
   ! explicit units never gets this characteristic inserted.
   function get_item_units(item, rc) result(units)
      type(StateItemDescriptor), intent(in) :: item
      integer, optional, intent(out) :: rc
      character(:), allocatable :: units

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      units = ''
      if (.not. item%has_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND)) then
         _RETURN(_SUCCESS)
      end if

      call item%get_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (PhysicalUnitsCharacteristic)
         units = characteristic%get_units()
      class default
         _FAIL('GraphBuilder: get_item_units - characteristic is not a PhysicalUnitsCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end function get_item_units

   ! Same role as get_item_units(), for TYPE_KIND_CHARACTERISTIC_KIND. Always
   ! established (advertise_one inserts it unconditionally).
   function get_item_typekind(item, rc) result(type_kind)
      type(StateItemDescriptor), intent(in) :: item
      integer, optional, intent(out) :: rc
      type(ESMF_TypeKind_Flag) :: type_kind

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      type_kind = ESMF_TYPEKIND_R4
      _ASSERT(item%has_characteristic(TYPE_KIND_CHARACTERISTIC_KIND), 'GraphBuilder: get_item_typekind - no TYPE_KIND_CHARACTERISTIC_KIND established')

      call item%get_characteristic(TYPE_KIND_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (TypeKindCharacteristic)
         type_kind = label_to_typekind(characteristic%get_type_kind())
      class default
         _FAIL('GraphBuilder: get_item_typekind - characteristic is not a TypeKindCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end function get_item_typekind

   ! Materializes every entry of intermediate_node_ids (in order) with that
   ! link's own evolved typekind/units (merged_item_for_link). Resolves
   ! geom/vgrid once from export_item and reuses them for every link. Stops
   ! at the first failing link, returning try_materialize_field_extension's
   ! failure_reason. mismatched_kinds MUST be the array
   ! find_or_build_extension_chain was called with (intermediate_node_ids is
   ! parallel to it).
   subroutine materialize_extension_chain(this, export_comp_name, export_item, import_item, &
        mismatched_kinds, intermediate_node_ids, graph, failure_reason, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      type(NodeId), intent(in) :: intermediate_node_ids(:)
      type(ComponentGraph), target, intent(inout) :: graph
      character(:), allocatable, intent(out) :: failure_reason
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(OuterMetaComponent), pointer :: export_meta
      type(ESMF_Geom) :: geom
      logical :: has_geom
      class(VerticalGrid), allocatable :: vgrid
      type(VerticalStaggerLoc) :: vert_staggerloc
      type(VerticalStaggerLoc), allocatable :: declared_vertical_stagger
      character(:), allocatable :: link_units
      type(ESMF_TypeKind_Flag) :: link_typekind
      character(:), allocatable :: import_units

      failure_reason = ''

      ! Same export-side admission as try_materialize_field_extension; the
      ! import side stays FIELD-only.
      if (.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      if (.not. (import_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(import_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      import_units = get_item_units(import_item, _RC)
      _ASSERT(import_units /= '', 'GraphBuilder: materialize_extension_chain reached with no import units - internal inconsistency')

      export_meta => component_for(this, export_comp_name, _RC)

      call resolve_export_geom(export_item, export_meta, geom, has_geom, _RC)
      if (.not. has_geom) then
          failure_reason = 'unresolved_geom'
         _RETURN(_SUCCESS)
      end if

      call resolve_export_vgrid(export_item, export_meta, vgrid)
      if (allocated(vgrid)) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         call export_item%get_vertical_stagger(declared_vertical_stagger)
         if (allocated(declared_vertical_stagger)) vert_staggerloc = declared_vertical_stagger
      end if

      do i = 1, size(intermediate_node_ids)
         call merged_item_for_link(export_item, import_item, mismatched_kinds, i, link_units, link_typekind, _RC)

         if (allocated(vgrid)) then
            call materialize_field_extension(graph, intermediate_node_ids(i), geom=geom, &
                 typekind=link_typekind, units=link_units, &
                 ungridded_dims=export_item%get_ungridded_dims(), &
                 vgrid=vgrid, vert_staggerloc=vert_staggerloc, _RC)
         else
            call materialize_field_extension(graph, intermediate_node_ids(i), geom=geom, &
                 typekind=link_typekind, units=link_units, &
                 ungridded_dims=export_item%get_ungridded_dims(), _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine materialize_extension_chain

   ! Link i's materialized typekind/units is the IMPORT item's value for each
   ! kind in mismatched_kinds(1:i) (already converted by that point) and the
   ! EXPORT item's otherwise. At i == size(mismatched_kinds) this is the
   ! final-node formula of try_materialize_field_extension. _FAILs for any
   ! kind it cannot translate.
   subroutine merged_item_for_link(export_item, import_item, mismatched_kinds, i, units, type_kind, rc)
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      integer, intent(in) :: i
      character(:), allocatable, intent(out) :: units
      type(ESMF_TypeKind_Flag), intent(out) :: type_kind
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: j
      logical :: units_converted
      logical :: typekind_converted
      character(:), allocatable :: assertion_message

      units = get_item_units(export_item, _RC)
      type_kind = get_item_typekind(export_item, _RC)
      units_converted = .false.
      typekind_converted = .false.

      do j = 1, i
         if (mismatched_kinds(j) == PHYSICAL_UNITS_CHARACTERISTIC_KIND) then
            units_converted = .true.
         else if (mismatched_kinds(j) == TYPE_KIND_CHARACTERISTIC_KIND) then
            typekind_converted = .true.
         else if (mismatched_kinds(j) == VERTICAL_COORDINATE_CHARACTERISTIC_KIND) then
            ! Vertical grid is resolved once by materialize_extension_chain, not per
            ! link; nothing to merge. build_transform for this kind always fails, so
            ! this is unreachable in practice, but it is enumerated so a mismatch
            ! fails with build_chain's message rather than the _FAIL below.
         else if (mismatched_kinds(j) == ITEM_SHAPE_CHARACTERISTIC_KIND) then
            ! Nothing to merge: the materialized node is always a plain Field,
            ! decided by materialize_field_extension. Enumerated so a BRACKET->FIELD
            ! mismatch that does not reach a real Transform fails with build_chain's
            ! unsupported-characteristic message, not the "unrecognized" assertion
            ! below.
         else if (mismatched_kinds(j) == ATTRIBUTES_CHARACTERISTIC_KIND .or. &
                  mismatched_kinds(j) == UNGRIDDED_DIMS_CHARACTERISTIC_KIND .or. &
                  mismatched_kinds(j) == STANDARD_NAME_CHARACTERISTIC_KIND .or. &
                  mismatched_kinds(j) == QUANTITY_TYPE_CHARACTERISTIC_KIND .or. &
                  mismatched_kinds(j) == CONSERVATION_CHARACTERISTIC_KIND .or. &
                  mismatched_kinds(j) == NORMALIZATION_CHARACTERISTIC_KIND) then
            ! None of these axes has a real build_transform (each _FAILs), so
            ! build_chain fails first and none can reach here in practice. Enumerated
            ! so a mismatch on one fails with that message, not the "unrecognized"
            ! assertion below.
         else
            assertion_message = 'GraphBuilder: merged_item_for_link - unrecognized StateItemCharacteristicKind: ' // &
                 mismatched_kinds(j)%to_string()
            _FAIL(assertion_message)
         end if
      end do

      if (units_converted) units = get_item_units(import_item, _RC)
      if (typekind_converted) type_kind = get_item_typekind(import_item, _RC)

      _RETURN(_SUCCESS)
   end subroutine merged_item_for_link
   ! Resolves a (component, intent, short_name) reference to the item's real
   ! NodeId:
   !  - "<self>"/this's own name: this component's graph (the item must
   !    already have been advertised - graphbuilder_advertise() runs first).
   !  - a named descendant (direct child or `/`-separated path): the
   !    descendant's own NodeId, read from its ComponentGraph via
   !    resolve_qualified_component. The walk only goes downward, so a
   !    connection endpoint never resolves upward past `this`.
   !
   ! Node/network storage is shared hierarchy-wide, so a descendant's NodeId
   ! is already a valid edge endpoint for this component's
   ! ComponentGraph%add_dependency; no parent-local stand-in node is created.
   function resolve_local_node_id(this, comp_name, state_intent, short_name, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      character(:), allocatable :: item_id_key
      type(ComponentGraph), pointer :: owner_graph
      type(NodeId), pointer :: existing
      type(OuterMetaComponent), pointer :: owner_meta
      character(:), allocatable :: assertion_message

      item_id_key = item_key(state_intent, short_name)

      owner_meta => resolve_qualified_component(this, comp_name, _RC)
      owner_graph => owner_meta%get_component_graph()
      existing => owner_graph%get_resource_index(item_id_key)
      assertion_message = 'GraphBuilder: connection references an item "' // comp_name // &
           '" never advertised: ' // short_name
      _ASSERT(associated(existing), assertion_message)
      node_id = existing

      _RETURN(_SUCCESS)
   end function resolve_local_node_id
   ! Does this import have a graph-native resolution path? SERVICE does not:
   ! its resolution lives entirely in StateRegistry, never in the
   ! ComponentGraph import-port table.
   ! graphbuilder_unresolved_import_node_ids must exclude such an import
   ! BEFORE treating it as unresolved-and-propagatable: propagating it would
   ! expose it to an ancestor connection (e.g. all_unsatisfied) that can
   ! never resolve it either.
   !
   ! VECTOR/BRACKET are not excluded: a composite's members are resolved
   ! individually as ordinary graph nodes. EXPRESSION and WILDCARD are not
   ! excluded either: graphbuilder_run_expression_hook and
   ! resolve_wildcard_destination resolve them.
   logical function is_ordinary_resolvable_import(node) result(is_ordinary)
      class(StateItemNode), target, intent(in) :: node

      type(StateItemDescriptor), pointer :: descriptor
      type(ESMF_StateItem_Flag) :: declared_type

      descriptor => node%get_descriptor()
      declared_type = descriptor%get_declared_item_type()

      is_ordinary = .not. (declared_type == MAPL_STATEITEM_SERVICE)
   end function is_ordinary_resolvable_import
   ! For each of `this`'s advertised StateItemNodes declared EXPRESSION,
   ! resolves its N named variable references against already-advertised
   ! sibling exports and builds the ExpressionTransform wiring. A dedicated
   ! hook because the resolution is N-ary: resolve_one is pairwise and
   ! build_chain has a fixed 1:1 interface. Idempotent: guarded by an
   ! 'EXPRHOOK:' resource-index entry keyed off each item's NodeId, as
   ! resolve_callback_destination does.
   module subroutine graphbuilder_run_expression_hook(this, unusable, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: node_iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node
      type(GraphResolutionEntryVector) :: local_unresolved
      type(StateItemDescriptor), pointer :: descriptor

      graph => this%get_component_graph()
      node_ids = graph%get_node_ids()

      node_iter = node_ids%ftn_begin()
      do while (node_iter /= node_ids%ftn_end())
         call node_iter%next()
         node_id = node_iter%of()
         generic_node => graph%get_node(node_id)
         if (.not. associated(generic_node)) cycle
         select type (generic_node)
         class is (StateItemNode)
            descriptor => generic_node%get_descriptor()
            if (.not. (descriptor%get_declared_item_type() == MAPL_STATEITEM_EXPRESSION)) cycle
            call resolve_expression_item(this, graph, generic_node, local_unresolved, _RC)
         end select
      end do

      if (present(unresolved)) unresolved = local_unresolved

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_run_expression_hook

   ! Per-item worker for graphbuilder_run_expression_hook. node is one
   ! already-advertised EXPRESSION-typed StateItemNode owned by this's
   ! graph.
   subroutine resolve_expression_item(this, graph, node, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ComponentGraph), target, intent(inout) :: graph
      class(StateItemNode), target, intent(in) :: node
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(NodeId) :: node_id
      character(:), allocatable :: hook_key
      type(NodeId), pointer :: existing
      type(StateItemDescriptor), pointer :: descriptor
      character(:), allocatable :: expression
      type(StringVector) :: referenced_names
      type(StringVectorIterator) :: iter
      character(:), pointer :: name
      type(NodeId), allocatable :: source_ids(:)
      ! gfortran rejects growing a deferred-length CHARACTER array with an array
      ! constructor of differing lengths; accumulate port names in a
      ! StringVector, in source_ids order.
      type(StringVector) :: source_names
      type(StringVectorIterator) :: source_names_iter
      character(:), pointer :: source_name
      type(NodeId) :: export_node_id
      class(StateItemNode), pointer :: export_node
      logical :: found
      logical :: all_resolved
      type(ExpressionTransform) :: transformer
      type(NodeId) :: transform_id
      type(DependencyNetworkId) :: net_id
      integer :: i
      character(:), allocatable :: this_name

      node_id = node%get_node_id()

      ! Idempotency guard.
      hook_key = 'EXPRHOOK:' // node_id%to_string()
      existing => graph%get_resource_index(hook_key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      descriptor => node%get_descriptor()
      call descriptor%get_declared_expression(expression)
      referenced_names = parser_variables_in_expression(expression, _RC)
      this_name = this%get_name(_RC)

      allocate(source_ids(0))
      all_resolved = .true.

      associate (b => referenced_names%begin(), e => referenced_names%end())
         iter = b
         do while (iter /= e)
            name => iter%of()

            found = find_export_node_id(this, SELF_COMPONENT_NAME, name, export_node_id, export_node, _RC)
            if (.not. found) then
               all_resolved = .false.
               call unresolved%push_back(GraphResolutionEntry(this_name, name))
            else
               source_ids = [source_ids, export_node_id]
               call source_names%push_back(name)
            end if

            call iter%next()
         end do
      end associate

      ! A referenced name with no matching sibling export is reported above;
      ! the transform is not built for this item (not a hard failure).
      if (.not. all_resolved) then
         _RETURN(_SUCCESS)
      end if

      transformer = ExpressionTransform(expression, _RC)
      transform_id = graph%next_node_id(_RC)
      call graph%register_node(TransformGraphNode(transform_id, transformer), _RC)

      net_id = graph%get_default_network_id()
      i = 0
      associate (b => source_names%begin(), e => source_names%end())
         source_names_iter = b
         do while (source_names_iter /= e)
            source_name => source_names_iter%of()
            i = i + 1
            call graph%bind_port(net_id, transform_id, source_name, source_ids(i), _RC)
            call graph%add_dependency(net_id, source_ids(i), transform_id, _RC)
            call source_names_iter%next()
         end do
      end associate
      call graph%bind_port(net_id, transform_id, 'destination_field', node_id, _RC)
      call graph%add_dependency(net_id, transform_id, node_id, _RC)

      call graph%add_resource_index(hook_key, transform_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_expression_item

   ! Orchestrates one callback-declaring destination's full resolution:
   ! wildcard match + interface conformance (resolve_callback_import),
   ! materialization (materialize_callback_collection), wiring to the
   ! advertised destination import via an ordinary add_dependency edge, and
   ! per-match CallbackStateBinding construction
   ! (build_callback_state_binding). Idempotent: a resource-index key derived
   ! from the import's NodeId is checked first (as find_or_build_extension_chain's
   ! chain_key is), so a repeat resolve_connections() call does not duplicate
   ! collection members.
   subroutine resolve_callback_destination(this, src_pt, dst_pt, dst_short_name, expected_interface_id, rejected, rc)
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
      existing_collection => graph%get_resource_index(collection_key)
      if (associated(existing_collection)) then
         if (present(rejected)) rejected = StringVector()
         _RETURN(_SUCCESS)
      end if

      matches = resolve_callback_import(this, src_pt, expected_interface_id, rejected, _RC)

      collection_id = materialize_callback_collection(this, src_pt, matches, _RC)
      call graph%add_resource_index(collection_key, collection_id, _RC)

      net_id = graph%get_default_network_id()
      call graph%add_dependency(net_id, collection_id, import_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_callback_destination

   ! Finds the ComponentGraph in meta's subtree that owns node_id. A
   ! destination's own graph does not necessarily own a destination import's
   ! NodeId: graphbuilder_run_propagate_hook mirrors an unresolved import
   ! upward into ancestors' import-port tables by VALUE, without changing
   ! which graph owns the NodeId. For an all_unsatisfied-style connection
   ! declared several levels above the import's advertiser, add_resource_index
   ! (which asserts owns_node()) must therefore go through the owning graph.
   recursive function find_owning_component_graph(meta, node_id, rc) result(owning_graph)
      class(OuterMetaComponent), target, intent(inout) :: meta
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc
      type(ComponentGraph), pointer :: owning_graph

      integer :: status
      type(ComponentGraph), pointer :: own_graph
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta

      owning_graph => null()

      own_graph => meta%get_component_graph()
      if (own_graph%owns_node(node_id)) then
         owning_graph => own_graph
         _RETURN(_SUCCESS)
      end if

      num_children = meta%get_num_children()
      do i = 1, num_children
         child_name = meta%get_child_name(i, status)
         if (status /= 0) cycle
         child_meta => meta%get_child_outer_meta(child_name, status)
         if (status /= 0) cycle
         owning_graph => find_owning_component_graph(child_meta, node_id, status)
         if (associated(owning_graph)) then
            _RETURN(_SUCCESS)
         end if
      end do

      _RETURN(_SUCCESS)
   end function find_owning_component_graph

   ! Resolves one WILDCARD-typed destination import by re-running its own
   ! declared pattern against the connection's source export namespace
   ! (build_qualified_export_namespace) and fanning the result out into one
   ! composite member per match, bound in place onto dst_node's own NodeId
   ! rather than a freshly-synthesized collection node. Every match is bound
   ! via add_state_member (pure reference binding), never add_dependency: a
   ! WILDCARD import gains no DependencyNetwork edge through this path.
   subroutine resolve_wildcard_destination(this, src_pt, dst_pt, dst_node, unresolved, rc)
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
      class(OuterMetaComponent), pointer :: dst_meta
      type(QualifiedExportEntry), allocatable :: namespace(:)
      type(QualifiedExportEntry), allocatable :: matches(:)
      type(QualifiedExportEntry), allocatable :: grown_matches(:)
      type(VirtualConnectionPt) :: wildcard_v_pt
      type(VirtualConnectionPt) :: candidate_v_pt
      type(GraphStateItem), pointer :: payload
      type(ESMF_State) :: state
      type(NodeId) :: member_id
      type(NodeId) :: dst_node_id
      integer :: i, n

      ! dst_node's NodeId belongs to dst's ComponentGraph or to a descendant's
      ! (see find_owning_component_graph). add_resource_index asserts
      ! owns_node(), so the resource-index guard/write below must use the
      ! owning graph.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_node_id = dst_node%get_node_id()
      graph => find_owning_component_graph(dst_meta, dst_node_id, _RC)
      _ASSERT(associated(graph), 'GraphBuilder: resolve_wildcard_destination - no ComponentGraph in dst subtree owns this import NodeId')

      ! Idempotency, keyed off the import's NodeId (as
      ! resolve_callback_destination does).
      collection_key = 'WILDCARD_COLLECTION:' // dst_node_id%to_string()
      existing_collection => graph%get_resource_index(collection_key)
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

      wildcard_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, dst_node%get_short_name())

      root_meta => component_for(this, src_pt%component_name, _RC)
      namespace = build_qualified_export_namespace(root_meta, _RC)

      do i = 1, size(namespace)
         candidate_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, namespace(i)%qualified_name)
         if (.not. wildcard_v_pt%matches(candidate_v_pt)) cycle

         ! Grow via a fresh temporary + move_alloc: a self-referencing array
         ! constructor ICEs classic ifort.
         n = size(matches)
         allocate(grown_matches(n + 1))
         grown_matches(1:n) = matches
         grown_matches(n + 1) = namespace(i)
         call move_alloc(grown_matches, matches)
      end do

      if (size(matches) == 0) then
         ! Zero matches: reported as unresolved. No resource-index entry, so a
         ! later connection that supplies a match is not blocked by a stale guard.
         call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name()))
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

      call graph%add_resource_index(collection_key, dst_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_wildcard_destination

   ! Mutable view of a node's descriptor, reached through the graph rather
   ! than through an intent(in) node dummy (MIRRORED adoption writes to it).
   function mutable_descriptor(graph, node_id, rc) result(descriptor)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc
      type(StateItemDescriptor), pointer :: descriptor

      class(GraphNode), pointer :: node

      descriptor => null()
      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: mutable_descriptor - NodeId not found')
      select type (node)
      class is (StateItemNode)
         descriptor => node%get_descriptor()
      class default
         _FAIL('GraphBuilder: mutable_descriptor - NodeId is not a StateItemNode')
      end select

      _RETURN(_SUCCESS)
   end function mutable_descriptor

end submodule GraphBuilder_ConnectionResolution_smod
