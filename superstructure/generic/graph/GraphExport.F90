#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphExport: graph-neutral topology exporter. Depends only
! on ComponentGraph's and DependencyNetwork's public query API and
! NodeId%to_string(); no OuterComponent/StateRegistry/connection-point type
! is referenced. Nodes are labeled by NodeId%to_string() unless the caller
! supplies a label map (see below).
!
! Read-only: never executes a TransformGraphNode, never advances a
! NodeRevision; only read accessors are called
! (get_node()/get_network()/get_predecessors()/get_successors()/
! get_port_bindings()/get_revision()).
!
! Output schema (not fixed in the spec text, documented here,
! versioned via GRAPH_EXPORT_SCHEMA_VERSION):
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
! DOT and JSON carry the same node/edge/metadata content.
!
! Label/proxy enrichment: both exporters take an optional,
! caller-supplied NodeIdLabelMap (NodeId -> NodeLabel). When an entry exists
! for a node, its label replaces NodeId%to_string() in DOT's label="..."
! attribute and is emitted as a "label" field in JSON (omitted entirely with
! no label_map). An entry's is_proxy flag adds shape="doublecircle" in DOT
! and "proxy": true in JSON. This module resolves nothing itself
! (REQ-VIZ-003); the caller (GraphBuilder's build_label_map) builds the
! lookup.
!
! Not implemented: hierarchy-wide export and component clustering, which need
! multi-ComponentGraph composition absent from this single-graph core, and
! exact repeated-call/schema-freezing guarantees.
!
! A cross-component edge declared at THIS component (both endpoints owned by
! children) is single-ComponentGraph data: this component's DependencyNetwork
! stores it (ComponentGraph's allowed_foreign_node admits it) and it is
! exported; see export_graph_dot. That is distinct from stitching two
! separate ComponentGraph instances, which is not implemented.
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

   ! Primary Graphviz DOT output.
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

      network_ids = graph%get_network_ids()

      ! A cross-component connection declared at THIS component (e.g. a parent
      ! naming two children's exports/imports by qualified path) calls
      ! add_dependency() on this component's network with endpoints this component
      ! does not own. Walking only graph%get_node_ids() would never declare those
      ! foreign ids or check them via has_successors(). So union in each network's
      ! get_referenced_ids() before declaring nodes and walking edges; every
      ! referenced endpoint is then declared (unlabeled if need be) and
      ! reachable by the edge walk.
      node_ids = graph%get_node_ids()
      net_iter = network_ids%ftn_begin()
      do while (net_iter /= network_ids%ftn_end())
         call net_iter%next()
         network_id = net_iter%of()
         network => graph%get_network(network_id)
         _ASSERT(associated(network), 'GraphExport: enumerated DependencyNetworkId is not retrievable')
         call union_into(node_ids, network%get_referenced_ids())
      end do

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

   ! Secondary JSON output, same content as DOT.
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

      network_ids = graph%get_network_ids()

      ! As in export_graph_dot: union in every network's get_referenced_ids() so
      ! foreign-but-referenced endpoints are declared and reachable by the edge
      ! walk instead of silently dropped.
      node_ids = graph%get_node_ids()
      net_iter = network_ids%ftn_begin()
      do while (net_iter /= network_ids%ftn_end())
         call net_iter%next()
         network_id = net_iter%of()
         network => graph%get_network(network_id)
         _ASSERT(associated(network), 'GraphExport: enumerated DependencyNetworkId is not retrievable')
         call union_into(node_ids, network%get_referenced_ids())
      end do

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

   ! -- per-node-kind metadata --

   ! One seam for "how do I describe this node kind for visualization": a
   ! change to the payload representation would touch only this function, not
   ! the exporters.
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
         ! Any OperationGraphNode other than TransformGraphNode (e.g.
         ! MethodGraphNode): generic label, no payload/revision.
         kind_label = 'OperationGraphNode'
      class default
         ! A bare BaseGraphNode, or a GraphNode kind this exporter does not know:
         ! reported generically rather than failing (the exporter never fails
         ! on an unknown node kind).
         kind_label = 'GraphNode'
      end select

      _RETURN(_SUCCESS)
   end subroutine describe_node

   ! -- optional node-label enrichment --

   ! Resolves one node's display label and proxy status against an optional,
   ! caller-supplied NodeIdLabelMap. With no label_map, or no entry for the node,
   ! label falls back to id%to_string() and is_proxy is false.
   ! This exporter never resolves a label itself (REQ-VIZ-003); it only looks up
   ! an entry the caller built.
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

   ! -- port-binding edge labels ------------------------------

   ! Whichever endpoint of the edge (source_id -> target_id) is a
   ! TransformGraphNode owns the relevant port-binding table entry;
   ! the other endpoint's id is reverse-looked-up
   ! against that (network, transform) pair's port name -> NodeId map.
   ! Returns an empty string (not an error) when neither endpoint is a
   ! TransformGraphNode, or no matching binding-table entry exists -
   ! the exporter degrades gracefully: it omits the label and keeps the edge.
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

   ! -- node-id set union --

   ! Inserts every element of `addend` into `target_set`. gFTL NodeIdSet has no
   ! built-in union, and `insert` is idempotent, so this is just a loop.
   subroutine union_into(target_set, addend)
      type(NodeIdSet), intent(inout) :: target_set
      type(NodeIdSet), target, intent(in) :: addend

      type(NodeIdSetIterator) :: iter
      type(NodeId) :: id

      iter = addend%ftn_begin()
      do while (iter /= addend%ftn_end())
         call iter%next()
         id = iter%of()
         call target_set%insert(id)
      end do
   end subroutine union_into

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
