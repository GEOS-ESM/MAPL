#include "MAPL.h"

!------------------------------------------------------------------------------
! ComponentGraph: graph-neutral owner of polymorphic GraphNodes, dependency
! networks, identity generators, public import/export ports, child-port
! bindings, semantic resource indexes, and mutable/frozen/finalized
! lifecycle state (spec/07-component-graph.md REQ-CG-001).
!
! MUST NOT depend on OuterComponent, StateRegistry, GriddedComponentDriver,
! GraphBuilder, or any component-hierarchy type (REQ-CG-002) - this module
! only uses NodeId/DependencyNetworkId/PortId, the GraphNode hierarchy, and
! DependencyNetwork, all of which are already graph-neutral.
!
! Public import/export/child-port storage is a bare PortId/String -> NodeId
! lookup with lifecycle-aware add/get. Named-argument transform-port
! semantics (REQ-XFORM-004/005) are a separate concern, added on top: port
! *declarations* live on TransformGraphNode itself (graph-neutral, no
! ComponentGraph involvement); ComponentGraph additionally owns the
! external port *binding* table (bind_port()/get_port_binding()/
! get_port_bindings(), keyed by (DependencyNetworkId, NodeId) -> port name
! -> NodeId, mapl_PortBindingTable_mod) - REQ-XFORM-005 settles this as external
! storage, never on-node.
!
! update() (spec/11-revision-and-update.md REQ-REV-005..007/009) is a
! type-bound procedure here rather than a free subroutine in its own
! module: it is, in every observable sense, "a ComponentGraph operation"
! (exactly like validate()/freeze()), and giving it that home matches
! this type's existing API style rather than introducing an external
! module whose only reason to exist would be file-size management - see
! ComponentGraph_DemandDrivenUpdate.F90 (a genuine Fortran submodule) for
! why file size is handled that way instead. This is a deliberate
! contrast with mapl_GraphExport_mod (spec/19-visualization-export.md
! REQ-VIZ-003), which stays a genuinely separate module on purpose: a
! submodule has full access to this type's private components (the
! standard does not treat a submodule as an additional encapsulation
! boundary), so folding the exporter in the same way would silently
! remove the "depends only on the public query API" guarantee REQ-VIZ-003
! asks for - update() has no such external requirement to satisfy.
!
! REQ-CG-001 lists ten things ComponentGraph must own. Rather than ten-plus
! flat top-level components, the three near-identical id generators are
! bundled into GraphIdGenerators and the three PortId -> NodeId lookup
! tables into GraphPortRegistry (both private, module-internal aggregation
! types - not part of the public API, and ownership is unaffected since
! they are still transitively owned by ComponentGraph). The two independent
! frozen/finalized booleans are likewise collapsed into a single
! lifecycle_status, since a ComponentGraph only ever occupies one of three
! mutually exclusive states (mutable/frozen/finalized) - see graph_freeze()
! and graph_finalize() below for the only two transitions.
!
! Iteration convention: every gFTL walk here uses ftn_begin()/ftn_end() with
! the %next() call at the top of the loop body (iter = c%ftn_begin() sits
! one position before the first element; next() must run before the first
! use of iter).
!------------------------------------------------------------------------------
module mapl_ComponentGraph_mod
   use mapl_NodeId_mod
   use mapl_DependencyNetworkId_mod
   use mapl_PortId_mod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_OperationGraphNode_mod, only: OperationGraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_NodeIdSet_mod
   use mapl_NodeIdGraphNodeMap_mod
   use mapl_DependencyNetworkIdNetworkMap_mod
   use mapl_DependencyNetworkIdSet_mod, only: DependencyNetworkIdSet
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_PortBindingTable_mod, only: PortBindingTable
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ComponentGraph

   integer, parameter :: GRAPH_LIFECYCLE_MUTABLE = 1
   integer, parameter :: GRAPH_LIFECYCLE_FROZEN = 2
   integer, parameter :: GRAPH_LIFECYCLE_FINALIZED = 3

   ! Private aggregation of the three per-instance identity generators
   ! (REQ-CG-001) - a single sub-component in place of three near-identical
   ! generator fields directly on ComponentGraph.
   type :: GraphIdGenerators
      private
      type(NodeIdGenerator) :: node
      type(DependencyNetworkIdGenerator) :: network
      type(PortIdGenerator) :: port
   end type GraphIdGenerators

   ! Private aggregation of the three PortId -> NodeId lookup tables
   ! (REQ-CG-001: public import ports, public export ports, child-port
   ! bindings) - same rationale as GraphIdGenerators above.
   type :: GraphPortRegistry
      private
      type(PortIdNodeIdMap) :: import_ports
      type(PortIdNodeIdMap) :: export_ports
      type(PortIdNodeIdMap) :: child_bindings
   end type GraphPortRegistry

   type :: ComponentGraph
      private
      type(NodeIdGraphNodeMap) :: nodes
      type(DependencyNetworkIdNetworkMap) :: networks
      type(GraphIdGenerators) :: generators
      type(DependencyNetworkId) :: default_network_id
      type(GraphPortRegistry) :: ports
      type(PortBindingTable) :: transform_port_bindings
      type(StateItemMemberMap) :: resource_index
      integer :: lifecycle_status = GRAPH_LIFECYCLE_MUTABLE
   contains
      ! -- node registry --------------------------------------------------
      procedure :: next_node_id => graph_next_node_id
      procedure :: register_node => graph_register_node
      procedure :: get_node => graph_get_node
      procedure :: owns_node => graph_owns_node
      ! -- network registry ------------------------------------------------
      procedure :: get_default_network_id => graph_get_default_network_id
      procedure :: create_network => graph_create_network
      procedure :: get_network => graph_get_network
      procedure :: owns_network => graph_owns_network
      procedure :: add_dependency => graph_add_dependency
      ! -- ports / bindings / resource index --------------------------------
      procedure :: next_port_id => graph_next_port_id
      procedure :: add_import_port => graph_add_import_port
      procedure :: add_export_port => graph_add_export_port
      procedure :: add_child_port_binding => graph_add_child_port_binding
      procedure :: get_import_port => graph_get_import_port
      procedure :: get_export_port => graph_get_export_port
      procedure :: get_child_port_binding => graph_get_child_port_binding
      ! -- transform port bindings (REQ-XFORM-005) --------------------------
      procedure :: bind_port => graph_bind_port
      procedure :: get_port_binding => graph_get_port_binding
      procedure :: get_port_bindings => graph_get_port_bindings
      procedure :: add_resource_index => graph_add_resource_index
      procedure :: get_resource_index => graph_get_resource_index
      ! -- full-graph enumeration (for exporters / full-graph walks) --------
      procedure :: get_node_ids => graph_get_node_ids
      procedure :: get_network_ids => graph_get_network_ids
      ! -- demand-driven update (spec/11-revision-and-update.md REQ-REV-005..007) --
      procedure :: update => graph_update
      ! -- validation / lifecycle -------------------------------------------
      procedure :: validate => graph_validate
      procedure :: freeze => graph_freeze
      procedure :: is_frozen => graph_is_frozen
      procedure :: finalize => graph_finalize
      procedure :: is_finalized => graph_is_finalized
      ! -- private helpers ---------------------------------------------------
      procedure, private :: owned_node_ids => graph_owned_node_ids
      procedure, private :: classify_owned_nodes => graph_classify_owned_nodes
   end type ComponentGraph

   interface ComponentGraph
      module procedure new_ComponentGraph
   end interface ComponentGraph

   ! One explicit-stack frame for graph_update's iterative traversal
   ! (mapl_ComponentGraph_DemandDrivenUpdate_smod, below) - not itself
   ! part of ComponentGraph's own public API. Deliberately declared here
   ! in the ancestor module rather than inside that submodule: this is
   ! the only submodule anywhere in this codebase that would otherwise
   ! declare its own module-level derived type, and doing so triggered a
   ! shared-library symbol-versioning gap under ifx (the auto-generated
   ! .so version script had no entry for the type's compiler-generated
   ! descriptor symbol, whose name encodes both the ancestor module and
   ! submodule - `ld: version node not found for symbol ...@..._UPDATEFRAME`).
   ! Every other submodule in the codebase only implements interface
   ! bodies, never declares its own types - matching that established,
   ! universally-working pattern here avoids relying on an apparently
   ! untested corner of the toolchain, rather than chasing the exact
   ! root cause in ifx/CMake's version-script generation.
   type :: UpdateFrame
      type(NodeId) :: node_id
      class(GraphNode), pointer :: node => null()
      type(NodeId), allocatable :: predecessor_ids(:)
      integer :: next_index = 1
   end type UpdateFrame

   ! update()'s implementation (post-order dependency-graph traversal,
   ! REQ-REV-005..007 - explicit-stack, not literal Fortran recursion;
   ! see that submodule's own header comment) lives in a separate
   ! file/submodule
   ! (mapl_ComponentGraph_DemandDrivenUpdate_smod,
   ! ComponentGraph_DemandDrivenUpdate.F90) rather than inline below,
   ! purely to keep this already-large file from growing further - a
   ! submodule is still "the same module" for every visibility/API
   ! purpose (callers see an ordinary `graph%update(...)` type-bound
   ! call; there is no separate public interface to maintain).
   interface
      module subroutine graph_update(this, network_id, node_id, rc)
         class(ComponentGraph), target, intent(in) :: this
         type(DependencyNetworkId), intent(in) :: network_id
         type(NodeId), intent(in) :: node_id
         integer, optional, intent(out) :: rc
      end subroutine graph_update
   end interface

contains

   ! REQ-CG-003a/004: structure constructor performs the unconditional
   ! bootstrap (generators start fresh; the default network's id is the
   ! first DependencyNetworkId issued and cannot hit exhaustion).
   function new_ComponentGraph() result(graph)
      type(ComponentGraph) :: graph

      type(DependencyNetwork) :: empty_network

      graph%default_network_id = graph%generators%network%next()
      call graph%networks%insert(graph%default_network_id, empty_network)
   end function new_ComponentGraph

   ! -- node registry -------------------------------------------------------

   function graph_next_node_id(this, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeId) :: id

      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: next_node_id called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: next_node_id called on a frozen graph')

      id = this%generators%node%next(status)
      _ASSERT(status == 0, 'ComponentGraph: NodeId generator exhausted')

      _RETURN(_SUCCESS)
   end function graph_next_node_id

   ! REQ-CG-001/REQ-ID-005: the map is authoritative; a node cannot make
   ! itself a member by claiming an id, and no second node silently
   ! replaces an already-registered one.
   subroutine graph_register_node(this, node, rc)
      class(ComponentGraph), intent(inout) :: this
      class(GraphNode), intent(in) :: node
      integer, optional, intent(out) :: rc

      type(NodeId) :: id

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: register_node called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: register_node called on a frozen graph')

      id = node%get_node_id()
      _ASSERT(id%is_valid(), 'ComponentGraph: register_node requires a NodeId obtained via next_node_id()')
      _ASSERT(this%nodes%count(id) == 0, 'ComponentGraph: register_node - NodeId is already registered')

      call this%nodes%insert(id, node)

      _RETURN(_SUCCESS)
   end subroutine graph_register_node

   function graph_get_node(this, id) result(node)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeId), intent(in) :: id
      class(GraphNode), pointer :: node

      node => null()
      if (this%is_finalized()) return
      node => this%nodes%at(id)
   end function graph_get_node

   logical function graph_owns_node(this, id) result(owns)
      class(ComponentGraph), intent(in) :: this
      type(NodeId), intent(in) :: id

      owns = (.not. this%is_finalized()) .and. (this%nodes%count(id) > 0)
   end function graph_owns_node

   ! -- network registry ------------------------------------------------------

   function graph_get_default_network_id(this) result(id)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId) :: id

      id = this%default_network_id
   end function graph_get_default_network_id

   ! REQ-CG-006: no new DependencyNetworks may be created after freeze.
   function graph_create_network(this, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(DependencyNetworkId) :: id

      type(DependencyNetwork) :: empty_network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: create_network called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: create_network called on a frozen graph')

      id = this%generators%network%next(status)
      _ASSERT(status == 0, 'ComponentGraph: DependencyNetworkId generator exhausted')

      call this%networks%insert(id, empty_network)

      _RETURN(_SUCCESS)
   end function graph_create_network

   function graph_get_network(this, id) result(network)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id
      type(DependencyNetwork), pointer :: network

      network => null()
      if (this%is_finalized()) return
      network => this%networks%at(id)
   end function graph_get_network

   logical function graph_owns_network(this, id) result(owns)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id

      owns = (.not. this%is_finalized()) .and. (this%networks%count(id) > 0)
   end function graph_owns_network

   ! Convenience wrapper: forwards to DependencyNetwork%add_dependency()
   ! with this graph's full owned-node-id set, so callers get the
   ! REQ-CG-004's "Invalid foreign node is rejected" scenario for free.
   subroutine graph_add_dependency(this, network_id, source, target, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
      type(NodeIdSet) :: owned_ids

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_dependency called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_dependency called on a frozen graph')

      network => this%networks%at(network_id)
      _ASSERT(associated(network), 'ComponentGraph: add_dependency - unknown DependencyNetworkId')

      owned_ids = this%owned_node_ids()
      call network%add_dependency(source, target, rc=rc, valid_ids=owned_ids)
   end subroutine graph_add_dependency

   ! -- ports / bindings / resource index ---------------------------------

   function graph_next_port_id(this, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(PortId) :: id

      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: next_port_id called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: next_port_id called on a frozen graph')

      id = this%generators%port%next(status)
      _ASSERT(status == 0, 'ComponentGraph: PortId generator exhausted')

      _RETURN(_SUCCESS)
   end function graph_next_port_id

   ! REQ-CG-001/005/006: not finalized, not frozen, port_id valid and not
   ! already bound (no silent replace), node_id owned by this graph.
   ! (Deliberately not factored through a shared helper that would take
   ! both `this` and one of its own map components as separate dummy
   ! arguments - that aliases the same storage across two arguments,
   ! one of them INTENT(INOUT), which strict compilers may reject.)
   subroutine graph_add_import_port(this, port_id, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_import_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_import_port called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_import_port requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%import_ports%count(port_id) == 0, 'ComponentGraph: add_import_port - PortId is already bound')
      _ASSERT(this%nodes%count(node_id) > 0, 'ComponentGraph: add_import_port - NodeId is not owned by this graph')

      call this%ports%import_ports%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_import_port

   subroutine graph_add_export_port(this, port_id, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_export_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_export_port called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_export_port requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%export_ports%count(port_id) == 0, 'ComponentGraph: add_export_port - PortId is already bound')
      _ASSERT(this%nodes%count(node_id) > 0, 'ComponentGraph: add_export_port - NodeId is not owned by this graph')

      call this%ports%export_ports%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_export_port

   subroutine graph_add_child_port_binding(this, port_id, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_child_port_binding called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_child_port_binding called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_child_port_binding requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%child_bindings%count(port_id) == 0, 'ComponentGraph: add_child_port_binding - PortId is already bound')
      _ASSERT(this%nodes%count(node_id) > 0, 'ComponentGraph: add_child_port_binding - NodeId is not owned by this graph')

      call this%ports%child_bindings%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_child_port_binding

   function graph_get_import_port(this, port_id) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%ports%import_ports%at(port_id)
   end function graph_get_import_port

   function graph_get_export_port(this, port_id) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%ports%export_ports%at(port_id)
   end function graph_get_export_port

   function graph_get_child_port_binding(this, port_id) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%ports%child_bindings%at(port_id)
   end function graph_get_child_port_binding

   ! -- transform port bindings (spec/10-transforms-and-ports.md REQ-XFORM-005) --

   ! Validates: graph not finalized/frozen; network_id and both node ids
   ! owned by this graph; node_id refers to a TransformGraphNode that has
   ! actually declared port_name (as either an input or output -
   ! REQ-XFORM-004's "binding requires a matching declaration"); if that
   ! declaration is kind-constrained and target_id is a StateItemNode,
   ! its GraphStateItem%variant() matches the declared expectation. Delegates
   ! the actual no-silent-replace storage to PortBindingTable%bind().
   subroutine graph_bind_port(this, network_id, node_id, port_name, target_id, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

      class(GraphNode), pointer :: node, target_node
      logical :: is_input, is_output
      type(PortSpec) :: spec
      type(MAPL_StateItem_Flag) :: expected_kind, actual_kind
      type(GraphStateItem) :: target_payload
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: bind_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: bind_port called on a frozen graph')
      _ASSERT(this%owns_network(network_id), 'ComponentGraph: bind_port - unknown DependencyNetworkId')
      _ASSERT(this%nodes%count(node_id) > 0, 'ComponentGraph: bind_port - transform NodeId is not owned by this graph')
      _ASSERT(this%nodes%count(target_id) > 0, 'ComponentGraph: bind_port - target NodeId is not owned by this graph')

      node => this%nodes%at(node_id)
      select type (node)
      class is (TransformGraphNode)
         is_input = node%is_input_port(port_name)
         is_output = node%is_output_port(port_name)
         _ASSERT(is_input .or. is_output, 'ComponentGraph: bind_port - port name not declared by this TransformGraphNode')

         if (is_input) then
            spec = node%get_input_port(port_name, _RC)
         else
            spec = node%get_output_port(port_name, _RC)
         end if

         if (spec%is_kind_constrained()) then
            expected_kind = spec%get_expected_kind(_RC)
            target_node => this%nodes%at(target_id)
            select type (target_node)
            class is (StateItemNode)
               target_payload = target_node%get_payload()
               actual_kind = target_payload%variant(_RC)
               _ASSERT(actual_kind == expected_kind, 'ComponentGraph: bind_port - bound NodeId kind does not match declared port kind constraint')
            class default
               ! Binding target is not a StateItemNode: the kind
               ! constraint is not checkable at this layer - documented
               ! intentional no-op, not an oversight. In practice every
               ! port binding target is expected to be a StateItemNode.
            end select
         end if
      class default
         _ASSERT(.false., 'ComponentGraph: bind_port - node_id does not refer to a TransformGraphNode')
      end select

      call this%transform_port_bindings%bind(network_id, node_id, port_name, target_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_bind_port

   function graph_get_port_binding(this, network_id, node_id, port_name) result(target_id)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), pointer :: target_id

      target_id => null()
      if (this%is_finalized()) return
      target_id => this%transform_port_bindings%get_binding(network_id, node_id, port_name)
   end function graph_get_port_binding

   function graph_get_port_bindings(this, network_id, node_id) result(port_map)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      type(StateItemMemberMap) :: port_map

      if (this%is_finalized()) return
      port_map = this%transform_port_bindings%get_bindings(network_id, node_id)
   end function graph_get_port_bindings

   subroutine graph_add_resource_index(this, key, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      character(*), intent(in) :: key
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_resource_index called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_resource_index called on a frozen graph')
      _ASSERT(this%nodes%count(node_id) > 0, 'ComponentGraph: add_resource_index - NodeId is not owned by this graph')
      _ASSERT(this%resource_index%count(key) == 0, 'ComponentGraph: add_resource_index - key is already bound')

      call this%resource_index%insert(key, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_resource_index

   function graph_get_resource_index(this, key) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      character(*), intent(in) :: key
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%resource_index%at(key)
   end function graph_get_resource_index

   ! -- validation / lifecycle -----------------------------------------------

   ! REQ-CG-004 scenario "Invalid foreign node is rejected" and "Same-pass
   ! cross-network write is rejected": validates every owned network
   ! (passing this graph's ownership/producer/state-item classification
   ! sets down to DependencyNetwork%validate()), then checks REQ-DEP-008a
   ! across all owned networks.
   subroutine graph_validate(this, rc)
      class(ComponentGraph), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      type(NodeIdSet) :: owned_ids, producer_ids, state_item_ids
      type(DependencyNetworkIdNetworkMapIterator) :: net_iter
      type(DependencyNetwork), pointer :: network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: validate called after finalize')

      owned_ids = this%owned_node_ids()
      call this%classify_owned_nodes(producer_ids, state_item_ids)

      net_iter = this%networks%ftn_begin()
      do while (net_iter /= this%networks%ftn_end())
         call net_iter%next()
         network => net_iter%second()
         call network%validate(valid_ids=owned_ids, producer_ids=producer_ids, &
                                 state_item_ids=state_item_ids, _RC)
      end do

      call validate_cross_network_writes(this, state_item_ids, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_validate

   ! REQ-DEP-008a: across all owned networks, no StateItemNode may be
   ! written (have a non-empty predecessor set) in more than one network.
   subroutine validate_cross_network_writes(this, state_item_ids, rc)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet), target, intent(in) :: state_item_ids
      integer, optional, intent(out) :: rc

      type(NodeIdSetIterator) :: item_iter
      type(DependencyNetworkIdNetworkMapIterator) :: net_iter
      type(NodeId) :: item
      type(DependencyNetwork), pointer :: network
      integer :: write_count

      item_iter = state_item_ids%ftn_begin()
      do while (item_iter /= state_item_ids%ftn_end())
         call item_iter%next()
         item = item_iter%of()

         write_count = 0
         net_iter = this%networks%ftn_begin()
         do while (net_iter /= this%networks%ftn_end())
            call net_iter%next()
            network => net_iter%second()
            if (network%has_predecessors(item)) write_count = write_count + 1
         end do

         _ASSERT(write_count <= 1, 'ComponentGraph: validate found a state item written in more than one network within the same update pass')
      end do

      _RETURN(_SUCCESS)
   end subroutine validate_cross_network_writes

   ! REQ-CG-005..007: validates first (no freeze state changes on
   ! failure), then freezes every owned network, then the graph itself.
   ! Idempotent if already frozen.
   subroutine graph_freeze(this, rc)
      class(ComponentGraph), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(DependencyNetworkIdNetworkMapIterator) :: net_iter
      type(DependencyNetwork), pointer :: network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: freeze called after finalize')

      if (this%is_frozen()) then
         _RETURN(_SUCCESS)
      end if

      call this%validate(_RC)

      net_iter = this%networks%ftn_begin()
      do while (net_iter /= this%networks%ftn_end())
         call net_iter%next()
         network => net_iter%second()
         call network%freeze()
      end do

      this%lifecycle_status = GRAPH_LIFECYCLE_FROZEN

      _RETURN(_SUCCESS)
   end subroutine graph_freeze

   ! Frozen and finalized are both "structure is immutable" states -
   ! finalize() may be called directly from mutable (REQ-CG-003b does not
   ! require a prior freeze), so this reports true for either.
   logical function graph_is_frozen(this) result(frozen)
      class(ComponentGraph), intent(in) :: this

      frozen = this%lifecycle_status /= GRAPH_LIFECYCLE_MUTABLE
   end function graph_is_frozen

   ! REQ-CG-003b/003c: explicit, fallible, never folded into a destructor.
   subroutine graph_finalize(this, rc)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: finalize called on an already-finalized graph')

      this%lifecycle_status = GRAPH_LIFECYCLE_FINALIZED

      _RETURN(_SUCCESS)
   end subroutine graph_finalize

   logical function graph_is_finalized(this) result(finalized)
      class(ComponentGraph), intent(in) :: this

      finalized = this%lifecycle_status == GRAPH_LIFECYCLE_FINALIZED
   end function graph_is_finalized

   ! -- full-graph enumeration (public; needed by the graph-neutral --------
   ! -- exporter, spec/19-visualization-export.md REQ-VIZ-003, and any -----
   ! -- other caller that must walk the whole graph via public API only) ---

   ! Public counterpart of the private owned_node_ids() helper below,
   ! empty (rather than erroring) once finalized, matching every other
   ! post-finalize accessor in this type.
   function graph_get_node_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet) :: ids

      if (this%is_finalized()) return
      ids = this%owned_node_ids()
   end function graph_get_node_ids

   function graph_get_network_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkIdSet) :: ids

      type(DependencyNetworkIdNetworkMapIterator) :: iter
      type(DependencyNetworkId) :: id

      if (this%is_finalized()) return

      iter = this%networks%ftn_begin()
      do while (iter /= this%networks%ftn_end())
         call iter%next()
         id = iter%first()
         call ids%insert(id)
      end do
   end function graph_get_network_ids

   ! -- private helpers --------------------------------------------------------

   function graph_owned_node_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet) :: ids

      type(NodeIdGraphNodeMapIterator) :: iter
      type(NodeId) :: id

      iter = this%nodes%ftn_begin()
      do while (iter /= this%nodes%ftn_end())
         call iter%next()
         id = iter%first()
         call ids%insert(id)
      end do
   end function graph_owned_node_ids

   ! Classifies owned nodes for producer/state-item purposes (REQ-DEP-008):
   ! an OperationGraphNode descendant is a producer-capable node; a
   ! StateItemNode is subject to the one-producer-per-state-item check.
   subroutine graph_classify_owned_nodes(this, producer_ids, state_item_ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet), intent(out) :: producer_ids
      type(NodeIdSet), intent(out) :: state_item_ids

      type(NodeIdGraphNodeMapIterator) :: iter
      type(NodeId) :: id
      class(GraphNode), pointer :: node

      iter = this%nodes%ftn_begin()
      do while (iter /= this%nodes%ftn_end())
         call iter%next()
         id = iter%first()
         node => iter%second()
         select type (node)
         class is (OperationGraphNode)
            call producer_ids%insert(id)
         class is (StateItemNode)
            call state_item_ids%insert(id)
         class default
            ! Any other GraphNode kind (e.g. a bare BaseGraphNode with no
            ! more specific role) is neither producer-capable nor subject
            ! to the one-producer-per-state-item constraint - deliberately
            ! left unclassified, not an oversight.
         end select
      end do
   end subroutine graph_classify_owned_nodes

end module mapl_ComponentGraph_mod
