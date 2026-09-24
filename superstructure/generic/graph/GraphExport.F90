#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphExport: graph-neutral topology exporter (spec/19-visualization-
! export.md §19.2, REQ-VIZ-001..003, REQ-VIZ-006..012, REQ-VIZ-014,
! REQ-VIZ-016/016a, REQ-VIZ-017). Depends only on ComponentGraph's and
! DependencyNetwork's public query API and NodeId%to_string() - no
! OuterComponent/StateRegistry/connection-point type is referenced.
! Nodes are labeled only by NodeId%to_string() at this layer (REQ-VIZ-
! 003/014); a future GraphBuilder-level enrichment layer (REQ-VIZ-004,
! Phase 3, out of scope here) would wrap this module's output with
! human-readable names, not modify it.
!
! Read-only: never executes a TransformGraphNode, never advances a
! NodeRevision (REQ-VIZ-002/016a) - only get_node()/get_network()/
! get_predecessors()/get_successors()/get_port_bindings()/get_revision()
! are called, all pre-existing read accessors.
!
! Output schema (REQ-VIZ-009a: intentionally not fixed in the spec text,
! documented here instead, versioned via GRAPH_EXPORT_SCHEMA_VERSION):
!
!   DOT:  digraph ComponentGraph {
!           "<NodeId>" [label="<NodeId or supplied label>", kind="<kind>",
!                       payload="<payload kind>"?, revision="<rev>"?,
!                       shape="doublecircle"?];
!           "<source NodeId>" -> "<target NodeId>"
!                       [network="<DependencyNetworkId>", label="<port>"?];
!         }
!   JSON: { "schema_version": <int>,
!           "nodes": [ {"id": "<NodeId>", "kind": "<kind>",
!                       "payload_kind": "<kind>"?, "revision": "<rev>"?,
!                       "label": "<NodeId or supplied label>"?,
!                       "proxy": true?} ],
!           "edges": [ {"source": "<NodeId>", "target": "<NodeId>",
!                       "network": "<DependencyNetworkId>",
!                       "port": "<port>"?} ] }
!
! ("?" marks optional fields, present only when applicable/requested.)
! DOT and JSON carry the same node/edge/metadata content (REQ-VIZ-009).
!
! Label/proxy enrichment (REQ-VIZ-004/015, visualization-enrichment-layer
! change): both exporters take an optional, caller-supplied NodeIdLabelMap
! (NodeId -> NodeLabel). When present and an entry exists for a node,
! its label text replaces NodeId%to_string() in DOT's existing label="..."
! attribute, and is emitted as a new "label" field in JSON (JSON has no
! prior "label" field to overload, so this is purely additive - omitted
! entirely when no label_map argument is supplied, preserving prior
! output byte-for-byte). An entry's is_proxy flag adds a
! shape="doublecircle" DOT attribute and a "proxy": true JSON field; a
! node with no entry, or no label_map at all, carries neither. This
! module still resolves nothing itself (REQ-VIZ-003 unchanged) - the
! caller (GraphBuilder.F90's build_label_map, Phase 3) is responsible
! for building the lookup.
!
! Not implemented here (Phase 3 / open questions, see design.md):
! REQ-VIZ-005/013 (hierarchy-wide export, component clustering - both
! require multi-ComponentGraph composition absent from this
! single-graph, graph-neutral core), REQ-VIZ-007a/009a exact
! repeated-call/schema-freezing guarantees (Q15/Q16,
! spec/17-open-questions.md).
!------------------------------------------------------------------------------
module mapl_GraphExport_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_DependencyNetworkIdSet_mod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_OperationGraphNode_mod, only: OperationGraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_NodeId_mod, only: NodeId, operator(==)
   use mapl_NodeIdSet_mod
   use mapl_NodeLabel_mod, only: NodeLabel
   use mapl_NodeIdLabelMap_mod, only: NodeIdLabelMap
   use mapl_StateItemMemberMap_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GRAPH_EXPORT_SCHEMA_VERSION
   public :: export_graph_dot
   public :: export_graph_json

   integer, parameter :: GRAPH_EXPORT_SCHEMA_VERSION = 1

contains

   ! -- public exporters ----------------------------------------------------

   ! REQ-VIZ-008: primary Graphviz DOT output.
   function export_graph_dot(graph, include_revisions, label_map, rc) result(dot_text)
      class(ComponentGraph), target, intent(in) :: graph
      logical, optional, intent(in) :: include_revisions
      type(NodeIdLabelMap), optional, target, intent(in) :: label_map
      integer, optional, intent(out) :: rc
      character(:), allocatable :: dot_text

      logical :: want_revisions
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: node_iter
      type(DependencyNetworkIdSet), target :: network_ids
      type(DependencyNetworkIdSetIterator) :: net_iter
      type(NodeId) :: id, target_id
      class(GraphNode), pointer :: node
      character(:), allocatable :: kind_label, payload_kind_label, revision_label
      character(:), allocatable :: attrs, edge_attrs, port_label
      character(:), allocatable :: node_label
      logical :: has_payload_kind, has_revision, is_proxy
      type(DependencyNetworkId) :: network_id
      type(DependencyNetwork), pointer :: network
      type(NodeIdSet), target :: successors
      type(NodeIdSetIterator) :: succ_iter
      integer :: status

      want_revisions = .false.
      if (present(include_revisions)) want_revisions = include_revisions

      dot_text = 'digraph ComponentGraph {' // NEW_LINE('a')
      dot_text = dot_text // '  // schema_version=' // int_to_string(GRAPH_EXPORT_SCHEMA_VERSION) // NEW_LINE('a')
      dot_text = dot_text // '  rankdir=LR;' // NEW_LINE('a')

      node_ids = graph%get_node_ids()

      node_iter = node_ids%ftn_begin()
      do while (node_iter /= node_ids%ftn_end())
         call node_iter%next()
         id = node_iter%of()
         node => graph%get_node(id)
         _ASSERT(associated(node), 'GraphExport: enumerated NodeId is not retrievable')

         call describe_node(node, kind_label, has_payload_kind, payload_kind_label, &
                              has_revision, revision_label, _RC)

         call resolve_node_label(label_map, id, node_label, is_proxy)

         attrs = 'label="' // dot_escape(node_label) // '", kind="' // dot_escape(kind_label) // '"'
         if (has_payload_kind) attrs = attrs // ', payload="' // dot_escape(payload_kind_label) // '"'
         if (want_revisions .and. has_revision) attrs = attrs // ', revision="' // dot_escape(revision_label) // '"'
         if (is_proxy) attrs = attrs // ', shape="doublecircle"'

         dot_text = dot_text // '  "' // dot_escape(id%to_string()) // '" [' // attrs // '];' // NEW_LINE('a')
      end do

      network_ids = graph%get_network_ids()

      net_iter = network_ids%ftn_begin()
      do while (net_iter /= network_ids%ftn_end())
         call net_iter%next()
         network_id = net_iter%of()
         network => graph%get_network(network_id)
         _ASSERT(associated(network), 'GraphExport: enumerated DependencyNetworkId is not retrievable')

         node_iter = node_ids%ftn_begin()
         do while (node_iter /= node_ids%ftn_end())
            call node_iter%next()
            id = node_iter%of()
            if (.not. network%has_successors(id)) cycle
            successors = network%get_successors(id)

            succ_iter = successors%ftn_begin()
            do while (succ_iter /= successors%ftn_end())
               call succ_iter%next()
               target_id = succ_iter%of()

               port_label = find_port_label(graph, network_id, id, target_id, _RC)

               edge_attrs = 'network="' // dot_escape(network_id%to_string()) // '"'
               if (len(port_label) > 0) edge_attrs = edge_attrs // ', label="' // dot_escape(port_label) // '"'

               dot_text = dot_text // '  "' // dot_escape(id%to_string()) // '" -> "' // &
                           dot_escape(target_id%to_string()) // '" [' // edge_attrs // '];' // NEW_LINE('a')
            end do
         end do
      end do

      dot_text = dot_text // '}' // NEW_LINE('a')

      _RETURN(_SUCCESS)
   end function export_graph_dot

   ! REQ-VIZ-009: secondary JSON output, same content as DOT.
   function export_graph_json(graph, include_revisions, label_map, rc) result(json_text)
      class(ComponentGraph), target, intent(in) :: graph
      logical, optional, intent(in) :: include_revisions
      type(NodeIdLabelMap), optional, target, intent(in) :: label_map
      integer, optional, intent(out) :: rc
      character(:), allocatable :: json_text

      logical :: want_revisions
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: node_iter
      type(DependencyNetworkIdSet), target :: network_ids
      type(DependencyNetworkIdSetIterator) :: net_iter
      type(NodeId) :: id, target_id
      class(GraphNode), pointer :: node
      character(:), allocatable :: kind_label, payload_kind_label, revision_label
      character(:), allocatable :: entry, port_label
      character(:), allocatable :: node_label
      logical :: has_payload_kind, has_revision, is_proxy
      logical :: first_entry
      type(DependencyNetworkId) :: network_id
      type(DependencyNetwork), pointer :: network
      type(NodeIdSet), target :: successors
      type(NodeIdSetIterator) :: succ_iter
      integer :: status

      want_revisions = .false.
      if (present(include_revisions)) want_revisions = include_revisions

      json_text = '{' // NEW_LINE('a')
      json_text = json_text // '  "schema_version": ' // int_to_string(GRAPH_EXPORT_SCHEMA_VERSION) // ',' // NEW_LINE('a')
      json_text = json_text // '  "nodes": [' // NEW_LINE('a')

      node_ids = graph%get_node_ids()

      first_entry = .true.
      node_iter = node_ids%ftn_begin()
      do while (node_iter /= node_ids%ftn_end())
         call node_iter%next()
         id = node_iter%of()
         node => graph%get_node(id)
         _ASSERT(associated(node), 'GraphExport: enumerated NodeId is not retrievable')

         call describe_node(node, kind_label, has_payload_kind, payload_kind_label, &
                              has_revision, revision_label, _RC)

         call resolve_node_label(label_map, id, node_label, is_proxy)

         entry = '    {"id": "' // json_escape(id%to_string()) // '", "kind": "' // json_escape(kind_label) // '"'
         if (has_payload_kind) entry = entry // ', "payload_kind": "' // json_escape(payload_kind_label) // '"'
         if (want_revisions .and. has_revision) entry = entry // ', "revision": "' // json_escape(revision_label) // '"'
         if (present(label_map)) entry = entry // ', "label": "' // json_escape(node_label) // '"'
         if (is_proxy) entry = entry // ', "proxy": true'
         entry = entry // '}'

         if (.not. first_entry) json_text = json_text // ',' // NEW_LINE('a')
         json_text = json_text // entry
         first_entry = .false.
      end do

      json_text = json_text // NEW_LINE('a') // '  ],' // NEW_LINE('a')
      json_text = json_text // '  "edges": [' // NEW_LINE('a')

      network_ids = graph%get_network_ids()

      first_entry = .true.
      net_iter = network_ids%ftn_begin()
      do while (net_iter /= network_ids%ftn_end())
         call net_iter%next()
         network_id = net_iter%of()
         network => graph%get_network(network_id)
         _ASSERT(associated(network), 'GraphExport: enumerated DependencyNetworkId is not retrievable')

         node_iter = node_ids%ftn_begin()
         do while (node_iter /= node_ids%ftn_end())
            call node_iter%next()
            id = node_iter%of()
            if (.not. network%has_successors(id)) cycle
            successors = network%get_successors(id)

            succ_iter = successors%ftn_begin()
            do while (succ_iter /= successors%ftn_end())
               call succ_iter%next()
               target_id = succ_iter%of()

               port_label = find_port_label(graph, network_id, id, target_id, _RC)

               entry = '    {"source": "' // json_escape(id%to_string()) // &
                        '", "target": "' // json_escape(target_id%to_string()) // &
                        '", "network": "' // json_escape(network_id%to_string()) // '"'
               if (len(port_label) > 0) entry = entry // ', "port": "' // json_escape(port_label) // '"'
               entry = entry // '}'

               if (.not. first_entry) json_text = json_text // ',' // NEW_LINE('a')
               json_text = json_text // entry
               first_entry = .false.
            end do
         end do
      end do

      json_text = json_text // NEW_LINE('a') // '  ]' // NEW_LINE('a')
      json_text = json_text // '}' // NEW_LINE('a')

      _RETURN(_SUCCESS)
   end function export_graph_json

   ! -- per-node-kind metadata (REQ-VIZ-010/010a's requested isolation) -----

   ! One seam for "how do I describe this node kind for visualization" -
   ! resolving the still-open payload representation question
   ! (REQ-VIZ-010a, 17-open-questions.md Q11 - already resolved for
   ! GraphStateItem, per 03-graph-node-hierarchy.md REQ-NODE-003a) would only
   ! require updating this one function, not the exporters above.
   subroutine describe_node(node, kind_label, has_payload_kind, payload_kind_label, &
                              has_revision, revision_label, rc)
      class(GraphNode), intent(in) :: node
      character(:), allocatable, intent(out) :: kind_label
      logical, intent(out) :: has_payload_kind
      character(:), allocatable, intent(out) :: payload_kind_label
      logical, intent(out) :: has_revision
      character(:), allocatable, intent(out) :: revision_label
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(MAPL_StateItem_Flag) :: variant_flag

      has_payload_kind = .false.
      payload_kind_label = ''
      has_revision = .false.
      revision_label = ''

      select type (node)
      class is (TransformGraphNode)
         kind_label = 'TransformGraphNode'
      class is (StateItemNode)
         kind_label = 'StateItemNode'

         payload = node%get_payload()
         variant_flag = payload%variant(_RC)
         payload_kind_label = variant_flag%to_string()
         has_payload_kind = .true.

         revision = node%get_revision()
         has_revision = revision%is_valid()
         if (has_revision) revision_label = revision%to_string()
      class is (OperationGraphNode)
         ! Any OperationGraphNode descendant besides TransformGraphNode
         ! (e.g. a future MethodGraphNode) - generic label, no
         ! payload/revision (it does not carry either).
         kind_label = 'OperationGraphNode'
      class default
         ! A bare BaseGraphNode with no more specific role, or any
         ! future GraphNode kind not yet known to this exporter -
         ! reported generically rather than failing (REQ-VIZ-014's
         ! "never fail the export" spirit extended to node kind, not
         ! just to name resolution).
         kind_label = 'GraphNode'
      end select

      _RETURN(_SUCCESS)
   end subroutine describe_node

   ! -- optional node-label enrichment (REQ-VIZ-004/015) --------------------

   ! Resolves one node's display label and proxy status against an
   ! optional, caller-supplied NodeIdLabelMap. Absent a label_map, or a
   ! node with no entry in it, label falls back to id%to_string() and
   ! is_proxy is .false. - REQ-VIZ-014's existing fallback, unchanged.
   ! This exporter never resolves a label on its own (REQ-VIZ-003) -
   ! it only ever looks up an entry the caller already built.
   subroutine resolve_node_label(label_map, id, label, is_proxy)
      type(NodeIdLabelMap), optional, target, intent(in) :: label_map
      type(NodeId), intent(in) :: id
      character(:), allocatable, intent(out) :: label
      logical, intent(out) :: is_proxy

      type(NodeLabel), pointer :: entry

      label = id%to_string()
      is_proxy = .false.

      if (.not. present(label_map)) return

      entry => label_map%at(id)
      if (.not. associated(entry)) return

      label = entry%get_label()
      is_proxy = entry%get_is_proxy()
   end subroutine resolve_node_label

   ! -- port-binding edge labels (REQ-VIZ-012) ------------------------------

   ! Whichever endpoint of the edge (source_id -> target_id) is a
   ! TransformGraphNode owns the relevant port-binding table entry
   ! (REQ-XFORM-005); the other endpoint's id is reverse-looked-up
   ! against that (network, transform) pair's port name -> NodeId map.
   ! Returns an empty string (not an error) when neither endpoint is a
   ! TransformGraphNode, or no matching binding-table entry exists -
   ! REQ-VIZ-012's "degrade gracefully, omit the label, keep the edge."
   function find_port_label(graph, network_id, source_id, target_id, rc) result(label)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source_id
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc
      character(:), allocatable :: label

      class(GraphNode), pointer :: source_node, target_node
      type(StateItemMemberMap), target :: bindings

      label = ''

      source_node => graph%get_node(source_id)
      target_node => graph%get_node(target_id)

      select type (target_node)
      class is (TransformGraphNode)
         ! source_id -> target_id: target is the transform; source
         ! fills one of its declared input ports.
         bindings = graph%get_port_bindings(network_id, target_id)
         label = reverse_find_name(bindings, source_id)
         _RETURN(_SUCCESS)
      class default
         ! Target is not a TransformGraphNode - check the source next.
      end select

      select type (source_node)
      class is (TransformGraphNode)
         ! source_id -> target_id: source is the transform; target
         ! fills one of its declared output ports.
         bindings = graph%get_port_bindings(network_id, source_id)
         label = reverse_find_name(bindings, target_id)
         _RETURN(_SUCCESS)
      class default
         ! Neither endpoint is a TransformGraphNode - no port label
         ! applies; label stays empty.
      end select

      _RETURN(_SUCCESS)
   end function find_port_label

   function reverse_find_name(bindings, needle_id) result(name)
      type(StateItemMemberMap), target, intent(in) :: bindings
      type(NodeId), intent(in) :: needle_id
      character(:), allocatable :: name

      type(StateItemMemberMapIterator) :: iter
      type(NodeId) :: candidate

      name = ''
      iter = bindings%ftn_begin()
      do while (iter /= bindings%ftn_end())
         call iter%next()
         candidate = iter%second()
         if (candidate == needle_id) then
            name = iter%first()
            return
         end if
      end do
   end function reverse_find_name

   ! -- text formatting helpers ---------------------------------------------

   function int_to_string(value) result(text)
      integer, intent(in) :: value
      character(:), allocatable :: text
      character(16) :: buffer

      write(buffer, '(I0)') value
      text = trim(adjustl(buffer))
   end function int_to_string

   function dot_escape(text) result(escaped)
      character(*), intent(in) :: text
      character(:), allocatable :: escaped

      integer :: i

      escaped = ''
      do i = 1, len(text)
         select case (text(i:i))
         case ('"')
            escaped = escaped // '\"'
         case ('\')
            escaped = escaped // '\\'
         case default
            escaped = escaped // text(i:i)
         end select
      end do
   end function dot_escape

   function json_escape(text) result(escaped)
      character(*), intent(in) :: text
      character(:), allocatable :: escaped

      integer :: i

      escaped = ''
      do i = 1, len(text)
         select case (text(i:i))
         case ('"')
            escaped = escaped // '\"'
         case ('\')
            escaped = escaped // '\\'
         case default
            escaped = escaped // text(i:i)
         end select
      end do
   end function json_escape

end module mapl_GraphExport_mod
