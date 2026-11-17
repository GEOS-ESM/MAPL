#include "MAPL.h"

! Connection resolution: wires SimpleConnection/MatchConnection destinations
! to export nodes, propagates unresolved imports and exports up the
! hierarchy, and resolves expression, callback, wildcard and inout
! destinations. Split by concern across sibling submodules of
! mapl_GraphBuilder_mod: GraphBuilder_ConnectionMatching_smod
! (match-connection resolution), GraphBuilder_ConnectionDestinations_smod
! (inout/callback/wildcard destination resolution),
! GraphBuilder_ImportExportPropagation_smod (hierarchy import/export
! propagation), GraphBuilder_PayloadMaterialization_smod (export payload
! materialization), GraphBuilder_ExpressionResolution_smod (expression-item
! resolution). This file holds the helpers shared by more than one of
! those; each is a module procedure declared in mapl_GraphBuilder_mod's own
! interface block (not a nested submodule - NAG rejects a descendant
! submodule's reference to an ancestor-interface-only derived type coming
! through a parent submodule two levels up), so every sibling file reaches
! them directly.
!
! Identity (short_name, intent, inout/callback markers) and the mismatch
! characteristics are read off each already-advertised StateItemNode, never
! re-derived from a VariableSpec.
submodule (mapl_GraphBuilder_mod) GraphBuilder_ConnectionResolution_smod
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


   module subroutine push_warnings(warnings, component_name, short_name, messages)
      type(GraphResolutionEntryVector), intent(inout) :: warnings
      character(*), intent(in) :: component_name
      character(*), intent(in) :: short_name
      type(StringVector), target, intent(in) :: messages

      integer :: i

      do i = 1, messages%size()
         call warnings%push_back(GraphResolutionEntry(component_name, short_name, messages%of(i)))
      end do
   end subroutine push_warnings

   ! Entry for a destination import; the caller chooses the vector.
   module subroutine push_entry(entries, dst_pt, dst_node, reason)
      type(GraphResolutionEntryVector), intent(inout) :: entries
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), intent(in) :: dst_node
      character(*), optional, intent(in) :: reason

      call entries%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), reason))
   end subroutine push_entry

   ! Records a pair-level conflict, instead of failing the whole pass, when
   ! another connection already produces this import. True if recorded.
   logical module function record_second_producer(graph, source_id, import_id, dst_pt, dst_node, conflicts)
      type(ComponentGraph), intent(in) :: graph
      type(NodeId), intent(in) :: source_id
      type(NodeId), intent(in) :: import_id
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), intent(in) :: dst_node
      type(GraphResolutionEntryVector), intent(inout) :: conflicts

      character(:), allocatable :: item_name

      item_name = graph%forward_producer_conflict(source_id, import_id)
      record_second_producer = (item_name /= '')
      if (record_second_producer) then
         call push_entry(conflicts, dst_pt, dst_node, 'import "' // item_name // &
              '" already has a different producer (another connection)')
      end if
   end function record_second_producer

   ! The one place a Connection's concrete subtype matters for resolution.
   ! SimpleConnection: its declared source name, which may differ from the
   ! destination's name. MatchConnection (class default): the destination's own
   ! name (the same-name wildcard semantic of connection/MatchConnection.F90).
   ! Callers stay subtype-agnostic by going through this function.
   module function effective_source_name(conn, dst_node) result(name)
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
   logical module function is_wildcard_member_shape(entry)
      type(QualifiedExportEntry), intent(in) :: entry

      is_wildcard_member_shape = &
           (entry%var_spec%itemType == MAPL_STATEITEM_FIELD)
   end function is_wildcard_member_shape

   ! Read-only counterpart of resolve_wildcard_destination's matching loop,
   ! used by the "already resolved?" check (source_satisfies_import). The
   ! exact-name export lookup cannot work for a WILDCARD import: its short
   ! name is the regex pattern itself. The pattern is rebuilt here with
   ! EXPORT intent because the connection's own dst_pt%v_pt is IMPORT-intent
   ! and so never matches an export.
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

      root_meta => resolve_qualified_component(owner, src_component_name, _RC)
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

   ! The one loop over a component's declared connections for ordinary
   ! resolution. Only MatchConnection and SimpleConnection reach op; other
   ! subtypes (e.g. ReexportConnection) are skipped (some callers handle them
   ! separately, e.g. the reexport loop; connection_resolves_import skips them
   ! deliberately, so mirroring is not wrongly suppressed).
   module subroutine for_each_connection_pair(owner, op, rc)
      class(OuterMetaComponent), target, intent(inout) :: owner
      procedure(I_pair_op) :: op
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      logical :: is_pair, done

      comp_spec => owner%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               is_pair = .true.
            class is (SimpleConnection)
               is_pair = .true.
            class default
               is_pair = .false.
            end select
            if (.not. is_pair) cycle
            call op(c, done, _RC)
            if (done) exit
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine for_each_connection_pair

   ! Read-only: does conn's source satisfy the destination import node? A
   ! callback destination needs at least one interface match, a WILDCARD one
   ! a regex match; any other (an inout borrower included) an exact-name export.
   ! The caller may pass src_meta already resolved; otherwise it is resolved
   ! only for the exact-name lookup, so a bad source path does not fail there.
   logical module function source_satisfies_import(owner, conn, node, src_meta, rc) result(satisfied)
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(Connection), intent(in) :: conn
      class(StateItemNode), target, intent(in) :: node
      class(OuterMetaComponent), target, optional, intent(inout) :: src_meta
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt
      type(CallbackInterfaceId) :: callback_interface_id
      type(QualifiedExportEntry), allocatable :: callback_matches(:)
      type(StateItemDescriptor), pointer :: node_descriptor
      class(OuterMetaComponent), pointer :: lookup_meta

      satisfied = .false.
      src_pt = conn%get_source()

      ! Checked against the flattened namespace, not by exact name.
      ! resolve_callback_import never mutates the graph.
      ! NAG rejects `f()%method()` on a function result; use a local temporary.
      callback_interface_id = node%get_callback_interface_id()
      if (callback_interface_id%is_valid()) then
         callback_matches = resolve_callback_import(owner, src_pt, callback_interface_id, rc=status)
         _VERIFY(status)
         satisfied = (size(callback_matches) > 0)
         _RETURN(_SUCCESS)
      end if

      ! The exact-name find_export_node below can never match a regex short name.
      node_descriptor => node%get_descriptor()
      if (node_descriptor%get_declared_item_type() == MAPL_STATEITEM_WILDCARD) then
         satisfied = wildcard_import_has_match(owner, src_pt%component_name, node, _RC)
         _RETURN(_SUCCESS)
      end if

      if (present(src_meta)) then
         lookup_meta => src_meta
      else
         lookup_meta => resolve_qualified_component(owner, src_pt%component_name, _RC)
      end if
      satisfied = associated(find_export_node(lookup_meta, effective_source_name(conn, node)))

      _RETURN(_SUCCESS)
   end function source_satisfies_import

   ! Read-only scan of src_meta's ComponentGraph export-port table for a node
   ! whose effective port name matches short_name. Used by
   ! find_export_node_id (which then mutates the graph) and by
   ! source_satisfies_import, which must NOT create graph structure.
   !
   ! Matches against get_export_port_name (the export-propagation override
   ! when one exists, else the node's short_name), not the node's
   ! short_name: a propagated export's StateItemNode%short_name stays
   ! unqualified, while this graph's port-table entry carries the
   ! ancestor-qualified name (`child_name/short_name`), mirroring legacy
   ! propagate_exports_virtual_pt.
   module function find_export_node(src_meta, short_name) result(export_node)
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
   module function resolve_local_node_id(this, comp_name, state_intent, short_name, rc) result(node_id)
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
   logical module function is_ordinary_resolvable_import(node) result(is_ordinary)
      class(StateItemNode), target, intent(in) :: node

      type(StateItemDescriptor), pointer :: descriptor
      type(ESMF_StateItem_Flag) :: declared_type

      descriptor => node%get_descriptor()
      declared_type = descriptor%get_declared_item_type()

      is_ordinary = .not. (declared_type == MAPL_STATEITEM_SERVICE)
   end function is_ordinary_resolvable_import

   ! Mutable view of a node's descriptor, reached through the graph rather
   ! than through an intent(in) node dummy (MIRRORED adoption writes to it).
   module function mutable_descriptor(graph, node_id, rc) result(descriptor)
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
