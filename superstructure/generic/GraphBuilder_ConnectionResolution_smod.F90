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
   use esmf, only: ESMF_State, ESMF_StateCreate, ESMF_StateGet, &
        ESMF_FieldDestroy, &
        ESMF_STATEITEM_NOTFOUND, &
        ESMF_STATEINTENT_INTERNAL, operator(==)
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

   subroutine push_warnings(warnings, component_name, short_name, messages)
      type(GraphResolutionEntryVector), intent(inout) :: warnings
      character(*), intent(in) :: component_name
      character(*), intent(in) :: short_name
      type(StringVector), target, intent(in) :: messages

      integer :: i

      do i = 1, messages%size()
         call warnings%push_back(GraphResolutionEntry(component_name, short_name, messages%of(i)))
      end do
   end subroutine push_warnings

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

   ! Whether a name-matching export may be a member of a Wildcard import.
   ! Only FIELD exports are members. Shared by resolve_wildcard_destination
   ! and wildcard_import_has_match so the two cannot disagree.
   logical function is_wildcard_member_shape(entry)
      type(QualifiedExportEntry), intent(in) :: entry

      is_wildcard_member_shape = &
           (entry%var_spec%itemType == MAPL_STATEITEM_FIELD)
   end function is_wildcard_member_shape

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
         if (.not. is_wildcard_member_shape(namespace(i))) cycle
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
         ! Set by try_alias_export_field. When true the export node already has a
         ! real field, so try_materialize_field_extension is skipped (its
         ! FieldCreate would overwrite the alias).
         logical :: alias_found
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
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name()))
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
               call conflicts%push_back(GraphResolutionEntry( &
                    dst_pt%component_name, dst_node%get_short_name(), &
                    incompatible_pair))
               _RETURN(_SUCCESS)
            end if
            if (expression_problem /= '') then
               call unsupported%push_back(GraphResolutionEntry( &
                    dst_pt%component_name, dst_node%get_short_name(), &
                    expression_problem))
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
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 status_conflict))
            _RETURN(_SUCCESS)
         end if
         call push_warnings(warnings, dst_pt%component_name, dst_node%get_short_name(), match_warnings)

         if (size(mismatched) == 0) then
            ! Exact match: wire directly, no extension.
            if (second_producer(export_node_id, import_node_id, dst_node%get_short_name())) then
               _RETURN(_SUCCESS)
            end if
            call this_graph%add_dependency(net_id, export_node_id, import_node_id, _RC)

            ! Give the export node a real, allocated field using the same geom/vgrid
            ! resolution as the mismatch branch. Gated on graph mode, not on
            ! materialize_extensions_enabled() (an orthogonal gate); no-op in
            ! GRAPH_MODE_REGISTRY.
            if (graph_mode() /= GRAPH_MODE_REGISTRY) then
               ! Try aliasing the export component's real field first. If found, the
               ! node already has a real field and try_materialize_field_extension must
               ! be skipped: its FieldCreate would overwrite the alias.
               alias_found = .false.
               if (.not. is_evaluation) then
                  call try_alias_export_field(this, src_pt%component_name, export_node%get_short_name(), &
                       this_graph, export_node_id, alias_found, _RC)
               end if
               if (.not. alias_found) then
                  call try_materialize_field_extension(export_item, &
                       import_item, this_graph, export_node_id, &
                       materialization_failure, &
                       name=export_node%get_short_name(), _RC)
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
              final_node_id, intermediate_node_ids, unsupported_characteristic, conflict=incompatible_pair, _RC)

         if (incompatible_pair /= '') then
            ! Reported apart from "no registered extension provider": this pair
            ! can never connect. The component's other connections still resolve.
            call conflicts%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 incompatible_pair))
            _RETURN(_SUCCESS)
         end if

         if (unsupported_characteristic /= '') then
            ! Distinguishable from "import has no matching export".
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 unsupported_characteristic))
            ! Kept for the freeze check, which fails on these in strict mode.
            call this%note_unsupported_pair(dst_pt%component_name // ':' // dst_node%get_short_name() // ':' // &
                 unsupported_characteristic)
            _RETURN(_SUCCESS)
         end if

         ! The chain's input node (export_node_id) must be materialized too: the
         ! chain's compute() fails on an unallocated input. export_item is passed
         ! as BOTH export and import so try_materialize_field_extension uses the
         ! export's typekind/units here (the chain's final node, below, keeps the
         ! import's). Gated on graph_mode() like the exact-match branch.
         if (graph_mode() /= GRAPH_MODE_REGISTRY) then
            ! Same alias-first treatment as the exact-match branch.
            alias_found = .false.
            if (.not. is_evaluation) then
               call try_alias_export_field(this, src_pt%component_name, export_node%get_short_name(), &
                    this_graph, export_node_id, alias_found, _RC)
            end if
            if (.not. alias_found) then
               ! export_item is passed as both export and import, so loosening the
               ! export-side gate would reach the FIELD-only import-side check and
               ! abandon the connection. A non-FIELD (e.g. bracket) input has no
               ! field-shaped version to materialize, so skip it here and let the
               ! chain's own materialization below run.
               if (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD) then
                  call try_materialize_field_extension(export_item, &
                       export_item, this_graph, export_node_id, &
                       materialization_failure, &
                       name=export_node%get_short_name(), _RC)
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
         ! intermediate_node_ids from that link's own descriptor.
         if (materialize_extensions_enabled()) then
            call materialize_extension_chain(export_item, import_item, &
                 intermediate_node_ids, this_graph, &
                 materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! Distinguishable from "no matching export" and "unregistered
               ! characteristic" above.
               call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                    materialization_failure))
               _RETURN(_SUCCESS)
            end if
         end if

         if (second_producer(final_node_id, import_node_id, dst_node%get_short_name())) then
            _RETURN(_SUCCESS)
         end if
         call this_graph%add_dependency(net_id, final_node_id, import_node_id, _RC)

         _RETURN(_SUCCESS)
      end subroutine resolve_one_pair

      ! Records a pair-level conflict, instead of failing the whole pass,
      ! when another connection already produces this import.
      logical function second_producer(source_id, import_id, short_name)
         type(NodeId), intent(in) :: source_id
         type(NodeId), intent(in) :: import_id
         character(*), intent(in) :: short_name

         character(:), allocatable :: item_name

         item_name = this_graph%forward_producer_conflict(source_id, import_id)
         second_producer = (item_name /= '')
         if (second_producer) then
            call conflicts%push_back(GraphResolutionEntry(dst_pt%component_name, &
                 short_name, 'import "' // item_name // &
                 '" already has a different producer (another connection)'))
         end if
      end function second_producer

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
               if (len(qualified_name) == 0) cycle

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

   ! See the interface in GraphBuilder.F90. A repeat call (same name, same
   ! node) is a no-op.
   module subroutine graphbuilder_run_reexport_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      comp_spec => this%get_component_spec()
      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            type is (ReexportConnection)
               call reexport_one(this, c, status)
               _VERIFY(status)
            class default
               ! Not a re-export.
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_run_reexport_hook

   subroutine reexport_one(this, conn, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      integer, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: src_meta
      type(ComponentGraph), pointer :: src_graph, this_graph
      type(NodeId), pointer :: internal_id
      class(StateItemNode), pointer :: export_node
      type(NodeId) :: src_node_id, existing_id
      type(PortId) :: new_port_id
      character(:), allocatable :: dst_name, src_name, msg
      logical :: taken

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()
      src_name = src_pt%v_pt%get_esmf_name()
      dst_name = dst_pt%v_pt%get_esmf_name()
      this_graph => this%get_component_graph()

      src_meta => resolve_qualified_component(this, src_pt%component_name, _RC)
      src_graph => src_meta%get_component_graph()

      if (src_pt%v_pt%is_internal()) then
         internal_id => src_graph%get_resource_index( &
              item_key(ESMF_STATEINTENT_INTERNAL, src_name))
         msg = 'Re-export source does not exist: ' // src_name
         _ASSERT(associated(internal_id), msg)
         src_node_id = internal_id
      else
         export_node => find_export_node(src_meta, src_name)
         msg = 'Re-export source does not exist: ' // src_name
         _ASSERT(associated(export_node), msg)
         src_node_id = export_node%get_node_id()
      end if

      call find_export_port_by_name(this_graph, dst_name, taken, existing_id)
      if (taken) then
         msg = 'Re-export destination already exists: ' // dst_name
         _ASSERT(existing_id == src_node_id, msg)
         _RETURN(_SUCCESS)
      end if

      new_port_id = this_graph%next_port_id(_RC)
      call this_graph%add_export_port(new_port_id, src_node_id, status, &
           qualified_name=dst_name)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine reexport_one

   subroutine find_export_port_by_name(graph, name, found, node_id)
      type(ComponentGraph), target, intent(in) :: graph
      character(*), intent(in) :: name
      logical, intent(out) :: found
      type(NodeId), intent(out) :: node_id

      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter

      found = .false.
      export_ports = graph%get_export_ports()
      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            if (graph%get_export_port_name(iter%first()) == name) then
               found = .true.
               node_id = iter%second()
               return
            end if
         end do
      end associate
   end subroutine find_export_port_by_name

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

      allocate(qualified_name, source='')
      own_name = child_graph%get_export_port_name(child_port_id)
      if (len(own_name) == 0) return

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
            if (len(candidate_name) == 0) cycle
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
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
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

      comp_spec => this%get_component_spec()

      call this%begin_connection_pass()
      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call resolve_match_connection(this, c, unresolved, unsupported, conflicts, warnings, _RC)
            class is (SimpleConnection)
               ! Same call as the MatchConnection arm: resolve_match_connection only
               ! uses Connection-base accessors.
               call resolve_match_connection(this, c, unresolved, unsupported, conflicts, warnings, _RC)
            class default
               ! Other subtypes (e.g. ReexportConnection) are not resolved here.
            end select
         end do
      end associate

      call this%end_connection_pass()

      if (present(unresolved_imports)) unresolved_imports = unresolved
      if (present(unsupported_characteristics)) unsupported_characteristics = unsupported
      if (present(connection_conflicts)) connection_conflicts = conflicts
      if (present(connection_warnings)) connection_warnings = warnings

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_resolve_connections

   ! Applies graphbuilder_fallback_unsupplied_deferred to meta and every
   ! descendant, so a connection resolved directly (bypassing each
   ! descendant's own connect hook) still sees a non-fatal DEFERRED side
   ! converted before matching.
   recursive subroutine fallback_unsupplied_deferred_tree(meta, rc)
      class(OuterMetaComponent), target, intent(inout) :: meta
      integer, optional, intent(out) :: rc

      integer :: status, j
      type(GraphResolutionEntryVector) :: fallback
      type(GraphResolutionEntry), pointer :: entry
      class(Logger), pointer :: lgr
      character(:), allocatable :: msg
      type(OuterMetaComponent), pointer :: child_meta
      character(:), allocatable :: child_name

      call graphbuilder_fallback_unsupplied_deferred(meta, fallback, _RC)
      lgr => meta%get_logger()
      do j = 1, fallback%size()
         entry => fallback%of(j)
         msg = 'GraphBuilder: ' // entry%component_name // ': ' // entry%reason
         call lgr%warning(msg)
      end do
      do j = 1, meta%get_num_children()
         child_name = meta%get_child_name(j, _RC)
         child_meta => meta%get_child_outer_meta(child_name, _RC)
         call fallback_unsupplied_deferred_tree(child_meta, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine fallback_unsupplied_deferred_tree

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
               if (len(candidate_name) == 0) cycle
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
      character(:), allocatable :: status_conflict, item_name
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
      call resolve_descriptors(owner_item, borrower_item, mismatched, status_conflict, warnings=match_warnings, _RC)
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

      ! Direct-alias case: edge in the forward network (as in the ordinary
      ! exact-match case) plus a return edge in a fresh per-pairing
      ! write-back network. No TransformGraphNode for either.
      ! A different owner already feeds this borrower: record it and skip the
      ! pairing (no forward edge, no return network).
      item_name = graph%forward_producer_conflict(owner_node_id, import_node_id)
      if (item_name /= '') then
         call conflicts%push_back(GraphResolutionEntry(dst_pt%component_name, &
              dst_node%get_short_name(), 'import "' // item_name // &
              '" already has a different producer (another connection)'))
         _RETURN(_SUCCESS)
      end if
      call graph%add_dependency(net_id, owner_node_id, import_node_id, _RC)
      call push_warnings(warnings, dst_pt%component_name, dst_node%get_short_name(), match_warnings)

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
   !
   ! With graph_populates_user_states() there is nothing to alias: the state
   ! holds the node's own created Field, so found = .false. and the caller
   ! completes that Field in place. With it off, the created Field is
   ! replaced by the alias and destroyed (nothing else holds it).
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
      type(ESMF_Field) :: alias, created
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload

      found = .false.
      _RETURN_IF(graph_populates_user_states())

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
         if (field_extension_is_created(graph, node_id)) then
            created = payload%get_field(_RC)
            call destroy_if_not_aliased(created, field, _RC)
         end if
         call payload%set(alias, _RC)
      class default
         _FAIL('GraphBuilder: try_alias_export_field - NodeId does not refer to a StateItemNode')
      end select

      found = .true.

      _RETURN(_SUCCESS)
   end subroutine try_alias_export_field

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

   ! Gives node_id (a Field-typed export) a real ESMF_Field built from its
   ! own descriptor. Otherwise sets failure_reason (empty on success): a
   ! non-Field item class on either side, or a descriptor still missing a
   ! value field creation needs.
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
      call materialize_field_extension(graph, node_id, name=name, _RC)

      _RETURN(_SUCCESS)
   end subroutine try_materialize_field_extension

   ! failure_reason is empty when a field can be created for `item`: the
   ! export side admits BRACKET alongside FIELD (a bracket export resolved
   ! into a field import is always materialized as a plain Field); the
   ! import side stays FIELD-only; and item lacks no creation value.
   subroutine check_materializable(export_item, import_item, item, &
        failure_reason)
      type(StateItemDescriptor), intent(in) :: export_item
      type(StateItemDescriptor), intent(in) :: import_item
      type(StateItemDescriptor), intent(in) :: item
      character(:), allocatable, intent(out) :: failure_reason

      type(StateItemCharacteristicKind), allocatable :: incomplete(:)

      failure_reason = ''
      if (.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD &
           .or. export_item%get_declared_item_type() == MAPL_STATEITEM_BRACKET)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         return
      end if
      if (.not. (import_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
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
   ! check_materializable's failure_reason. `name` names the link fields,
   ! for consumers that look their inputs up by field name.
   subroutine materialize_extension_chain(export_item, import_item, &
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

      failure_reason = ''
      do i = 1, size(intermediate_node_ids)
         link_node => graph%get_node(intermediate_node_ids(i))
         _ASSERT(associated(link_node), 'GraphBuilder: materialize_extension_chain - link NodeId not found in graph')
         select type (link_node)
         class is (StateItemNode)
            call check_materializable(export_item, import_item, &
                 link_node%get_descriptor(), failure_reason)
         class default
            _FAIL('GraphBuilder: materialize_extension_chain - link NodeId is not a StateItemNode')
         end select
         _RETURN_IF(failure_reason /= '')
         call materialize_field_extension(graph, intermediate_node_ids(i), &
              name=name, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine materialize_extension_chain

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
   ! excluded either: build_expression_evaluation and
   ! resolve_wildcard_destination resolve them.
   logical function is_ordinary_resolvable_import(node) result(is_ordinary)
      class(StateItemNode), target, intent(in) :: node

      type(StateItemDescriptor), pointer :: descriptor
      type(ESMF_StateItem_Flag) :: declared_type

      descriptor => node%get_descriptor()
      declared_type = descriptor%get_declared_item_type()

      is_ordinary = .not. (declared_type == MAPL_STATEITEM_SERVICE)
   end function is_ordinary_resolvable_import
   ! Names check for each of `this`'s EXPRESSION items. The item itself is a
   ! placeholder: evaluation is built per consumer at connect time
   ! (build_expression_evaluation), so this hook adds no nodes or edges.
   ! A missing name is reported in unresolved; a nested, self-referencing
   ! or empty expression in unsupported.
   module subroutine graphbuilder_run_expression_hook(this, unusable, &
        unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: node_iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node
      type(GraphResolutionEntryVector) :: local_unresolved
      type(GraphResolutionEntryVector) :: local_unsupported
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
            call check_expression_item(this, generic_node, local_unresolved, &
                 local_unsupported, _RC)
         end select
      end do

      if (present(unresolved)) unresolved = local_unresolved
      if (present(unsupported)) unsupported = local_unsupported

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_run_expression_hook

   ! Per-item worker for graphbuilder_run_expression_hook: one entry per
   ! problem found in node's expression.
   subroutine check_expression_item(this, node, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(StateItemNode), target, intent(in) :: node
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemDescriptor), pointer :: descriptor, ref_descriptor
      character(:), allocatable :: expression, this_name, item_name
      type(StringVector) :: referenced_names
      type(StringVectorIterator) :: iter
      character(:), pointer :: name
      type(NodeId) :: ref_id
      class(StateItemNode), pointer :: ref_node
      logical :: found

      this_name = this%get_name(_RC)
      item_name = node%get_short_name()
      descriptor => node%get_descriptor()
      call descriptor%get_expression(expression, _RC)
      if (len_trim(expression) == 0) then
         call unsupported%push_back(GraphResolutionEntry(this_name, item_name, &
              'expression is empty'))
         _RETURN(_SUCCESS)
      end if

      referenced_names = parser_variables_in_expression(expression, _RC)
      iter = referenced_names%begin()
      do while (iter /= referenced_names%end())
         name => iter%of()
         call iter%next()
         if (name == item_name) then
            call unsupported%push_back(GraphResolutionEntry(this_name, &
                 item_name, 'expression references itself'))
            cycle
         end if
         found = find_export_node_id(this, SELF_COMPONENT_NAME, name, &
              ref_id, ref_node, _RC)
         if (.not. found) then
            call unresolved%push_back(GraphResolutionEntry(this_name, name, &
                 'expression ' // item_name // ' references missing item ' // name))
            cycle
         end if
         ref_descriptor => ref_node%get_descriptor()
         if (ref_descriptor%get_declared_item_type() == MAPL_STATEITEM_EXPRESSION) then
            call unsupported%push_back(GraphResolutionEntry(this_name, &
                 item_name, 'expression references expression item'))
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine check_expression_item

   ! One evaluation of export_node's expression for a consumer described
   ! by import_item, built in `graph` (the connecting graph): a result node
   ! fed by an ExpressionTransform whose inputs are the referenced sibling
   ! exports, each through its own extension chain to the result's
   ! consumer-adopted kinds. Consumers whose results agree on those kinds
   ! share one evaluation (EXPREVAL: key). All input chains are built before
   ! the transform, so a failing input leaves no partial evaluation.
   ! `evaluated` is false when the names are invalid (the expression hook
   ! reports those), when a side is DEFERRED (`waiting`), or when `problem`
   ! or `conflict` is set. `strict` marks a `problem` that is an
   ! unsupported kind (kept for the freeze check).
   subroutine build_expression_evaluation(this, graph, net_id, src_comp, &
        export_node, import_item, result_id, evaluated, problem, conflict, &
        waiting, strict, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: net_id
      character(*), intent(in) :: src_comp
      class(StateItemNode), target, intent(in) :: export_node
      type(StateItemDescriptor), intent(in) :: import_item
      type(NodeId), intent(out) :: result_id
      logical, intent(out) :: evaluated
      character(:), allocatable, intent(out) :: problem
      character(:), allocatable, intent(out) :: conflict
      logical, intent(inout) :: waiting
      logical, intent(out) :: strict
      integer, optional, intent(out) :: rc

      integer :: status, i, k
      type(StateItemDescriptor), pointer :: export_desc, input_desc, stored
      type(StateItemCharacteristicKind), allocatable :: kinds(:)
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)
      type(StateItemCharacteristicKind), allocatable :: adopted(:)
      logical, allocatable :: declared_wins(:)
      class(StateItemCharacteristic), allocatable :: c
      character(:), allocatable :: key, expression, item_name
      character(:), allocatable :: status_conflict, unsupported_kind
      type(NodeId), pointer :: existing
      type(StringVector) :: names
      character(:), pointer :: name
      type(NodeId), allocatable :: input_ids(:), bound_ids(:)
      type(NodeId), allocatable :: intermediates(:)
      type(NodeId) :: input_id, transform_id, export_id
      class(StateItemNode), pointer :: input_node
      type(ExpressionTransform) :: transformer
      type(GraphStateItem) :: empty_payload
      type(NodeRevision) :: empty_revision
      type(StateItemCharacteristicMap), target :: export_map
      type(StateItemCharacteristicMapIterator) :: iter
      logical :: found, has, input_waiting
      character(:), allocatable :: failure
      type(ESMF_StateItem_Flag) :: input_type

      evaluated = .false.
      problem = ''
      conflict = ''
      strict = .false.
      item_name = export_node%get_short_name()
      export_desc => export_node%get_descriptor()
      call export_desc%get_consumer_adopted_kinds(kinds, &
           declared_wins=declared_wins, _RC)

      export_id = export_node%get_node_id()
      key = 'EXPREVAL:' // export_id%to_string()
      do k = 1, size(kinds)
         call result_characteristic(k, c, has, _RC)
         if (.not. has) cycle
         if (c%get_status() == CHARACTERISTIC_STATUS_DEFERRED) then
            waiting = .true.
            _RETURN(_SUCCESS)
         end if
         key = key // ':' // c%get_signature()
      end do
      existing => graph%get_reuse_entry(key, _RC)
      if (associated(existing)) then
         result_id = existing
         evaluated = .true.
         _RETURN(_SUCCESS)
      end if

      call export_desc%get_expression(expression, _RC)
      _RETURN_IF(len_trim(expression) == 0)
      names = parser_variables_in_expression(expression, _RC)
      allocate(input_ids(names%size()), bound_ids(names%size()))
      do i = 1, names%size()
         name => names%of(i)
         _RETURN_IF(name == item_name)
         found = find_export_node_id(this, src_comp, name, input_id, &
              input_node, _RC)
         _RETURN_UNLESS(found)
         input_desc => input_node%get_descriptor()
         input_type = input_desc%get_declared_item_type()
         _RETURN_IF(input_type == MAPL_STATEITEM_EXPRESSION)
         input_ids(i) = input_id
      end do

      ! Every input settled and comparable before anything is built.
      do i = 1, names%size()
         input_desc => mutable_descriptor(graph, input_ids(i), _RC)
         block
            type(StateItemDescriptor) :: goal
            call input_goal(input_desc, goal, _RC)
            call resolve_characteristics(input_desc%get_characteristics(), &
                 goal%get_characteristics(), &
                 input_desc%get_declared_item_type(), mismatched, adopted, &
                 status_conflict, rc=status, waiting=input_waiting)
            _VERIFY(status)
         end block
         if (input_waiting) then
            waiting = .true.
            _RETURN(_SUCCESS)
         end if
         if (status_conflict /= '') then
            problem = status_conflict
            _RETURN(_SUCCESS)
         end if
      end do

      ! Inputs need real fields for the transform to compute.
      if (graph_mode() /= GRAPH_MODE_REGISTRY) then
         do i = 1, names%size()
            name => names%of(i)
            call try_alias_export_field(this, src_comp, name, graph, &
                 input_ids(i), found, _RC)
            if (found) cycle
            input_desc => mutable_descriptor(graph, input_ids(i), _RC)
            call try_materialize_field_extension(input_desc, &
                 input_desc, graph, input_ids(i), failure, name=name, _RC)
            if (failure /= '') then
               problem = failure
               _RETURN(_SUCCESS)
            end if
         end do
      end if

      do i = 1, names%size()
         input_desc => mutable_descriptor(graph, input_ids(i), _RC)
         bound_ids(i) = input_ids(i)
         block
            type(StateItemDescriptor) :: goal, full_goal
            call input_goal(input_desc, goal, _RC)
            call resolve_characteristics(input_desc%get_characteristics(), &
                 goal%get_characteristics(), &
                 input_desc%get_declared_item_type(), mismatched, adopted, &
                 status_conflict, _RC)
            if (size(mismatched) == 0) cycle
            call find_or_build_extension_chain(graph, net_id, input_ids(i), &
                 input_desc%get_characteristics(), goal%get_characteristics(), &
                 mismatched, bound_ids(i), intermediates, unsupported_kind, &
                 conflict=conflict, _RC)
            _RETURN_IF(conflict /= '')
            if (unsupported_kind /= '') then
               problem = unsupported_kind
               strict = .true.
               _RETURN(_SUCCESS)
            end if
            if (size(intermediates) == 0) cycle
            if (materialize_extensions_enabled()) then
               call input_goal(input_desc, full_goal, full=.true., _RC)
               ! StateEval finds each input by its field name.
               call materialize_extension_chain(input_desc, &
                    full_goal, intermediates, graph, failure, &
                    name=names%of(i), _RC)
               if (failure /= '') then
                  problem = failure
                  _RETURN(_SUCCESS)
               end if
            end if
         end block
      end do

      transformer = ExpressionTransform(expression, _RC)
      transform_id = graph%next_node_id(_RC)
      call graph%register_node(TransformGraphNode(transform_id, transformer), _RC)
      result_id = graph%next_node_id(_RC)
      call graph%register_node(StateItemNode(result_id, empty_payload, &
           empty_revision), _RC)

      ! Filled entry by entry: whole-descriptor assignment copies a map of
      ! polymorphic values, which gfortran does not do reliably.
      stored => mutable_descriptor(graph, result_id, _RC)
      call stored%set_declared_item_type(MAPL_STATEITEM_FIELD)
      export_map = export_desc%get_characteristics()
      iter = export_map%ftn_begin()
      do while (iter /= export_map%ftn_end())
         call iter%next()
         call stored%set_characteristic(iter%first(), iter%second(), _RC)
      end do
      do k = 1, size(kinds)
         call result_characteristic(k, c, has, _RC)
         if (has) call stored%set_characteristic(kinds(k), c, _RC)
      end do
      call stored%add_expression_result_defaults(_RC)

      do i = 1, names%size()
         name => names%of(i)
         call graph%bind_port(net_id, transform_id, name, bound_ids(i), _RC)
         call graph%add_dependency(net_id, bound_ids(i), transform_id, _RC)
      end do
      call graph%bind_port(net_id, transform_id, 'destination_field', &
           result_id, _RC)
      call graph%add_dependency(net_id, transform_id, result_id, _RC)
      call graph%add_reuse_entry(key, result_id, _RC)
      evaluated = .true.

      _RETURN(_SUCCESS)
   contains

      ! The result's value for adopted kind kinds(k): a SPECIFIED value
      ! declared on the expression where the kind lets it win; else the
      ! consumer's unless it mirrors; else the expression's own.
      subroutine result_characteristic(k, value, has, rc)
         integer, intent(in) :: k
         class(StateItemCharacteristic), allocatable, intent(out) :: value
         logical, intent(out) :: has
         integer, optional, intent(out) :: rc

         integer :: status

         has = .false.
         if (declared_wins(k) .and. &
              export_desc%has_characteristic(kinds(k))) then
            call export_desc%get_characteristic(kinds(k), value, _RC)
            has = value%get_status() == CHARACTERISTIC_STATUS_SPECIFIED
            if (has) then
               _RETURN(_SUCCESS)
            end if
            deallocate(value)
         end if
         if (import_item%has_characteristic(kinds(k))) then
            call import_item%get_characteristic(kinds(k), value, _RC)
            has = .not. (value%get_status() == CHARACTERISTIC_STATUS_MIRRORED)
            if (has) then
               _RETURN(_SUCCESS)
            end if
            deallocate(value)
         end if
         if (export_desc%has_characteristic(kinds(k))) then
            call export_desc%get_characteristic(kinds(k), value, _RC)
            has = .true.
         end if
         _RETURN(_SUCCESS)
      end subroutine result_characteristic

      ! What an input must become: the result's value for each adopted kind
      ! the input has. With `full`, also the input's other entries (for
      ! materialization), so units and dimensions stay the input's own.
      subroutine input_goal(input, goal, rc, full)
         type(StateItemDescriptor), target, intent(in) :: input
         type(StateItemDescriptor), intent(inout) :: goal
         integer, optional, intent(out) :: rc
         logical, optional, intent(in) :: full

         integer :: status, j
         type(StateItemCharacteristicKind), allocatable :: present_kinds(:)
         class(StateItemCharacteristic), allocatable :: value
         logical :: has_value

         call goal%set_declared_item_type(input%get_declared_item_type())
         if (present(full)) then
            if (full) then
               ! Entry by entry: no copy of the polymorphic map.
               call input%get_kinds(present_kinds)
               do j = 1, size(present_kinds)
                  call input%get_characteristic(present_kinds(j), value, _RC)
                  call goal%set_characteristic(present_kinds(j), value, _RC)
                  deallocate(value)
               end do
            end if
         end if
         do j = 1, size(kinds)
            if (.not. input%has_characteristic(kinds(j))) cycle
            call result_characteristic(j, value, has_value, _RC)
            if (has_value) call goal%set_characteristic(kinds(j), value, _RC)
         end do
         _RETURN(_SUCCESS)
      end subroutine input_goal

   end subroutine build_expression_evaluation

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

      root_meta => component_for(this, src_pt%component_name, _RC)
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

      call graph%add_reuse_entry(collection_key, dst_node_id, _RC)

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
