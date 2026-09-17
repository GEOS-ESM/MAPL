#include "MAPL.h"

!------------------------------------------------------------------------------
! Submodule implementing ComponentGraph%update() - the runtime-
! interpreted demand-driven update algorithm (spec/11-revision-and-
! update.md REQ-REV-005..007, REQ-REV-009). Split into its own file
! purely for size (ComponentGraph.F90 is already the largest file in
! this repo); a submodule is still "the same module" as far as callers
! and the public API are concerned - see ComponentGraph.F90's module
! header for why this is a submodule (full ComponentGraph-internal
! access, ordinary type-bound call) rather than mapl_GraphExport_mod's
! approach (a genuinely separate module, deliberately unable to reach
! ComponentGraph's private components - see that module's header).
!
! update(this, network_id, node_id, rc): "ensure this node's value is
! current, within this network, by whatever demand-driven work that
! requires." Dispatches on the node's dynamic kind:
!
!   - StateItemNode: find its sole producer (predecessor) in this
!     network. Zero predecessors means externally supplied - nothing to
!     do. More than one predecessor is a defensive assert (REQ-DEP-008
!     validation should already forbid this). If the sole predecessor is
!     a TransformGraphNode, recurse into it.
!   - TransformGraphNode: recursively update every DependencyNetwork
!     predecessor first (REQ-REV-006 step 1) - by spec/10 §10.3's own
!     framing, adjacency and the port-binding table describe the same
!     edges from two angles, so this is exactly the transform's declared
!     input set. Then assemble the current declared-input revision
!     snapshot via the external port-binding table
!     (get_port_binding(), REQ-XFORM-005), ask the transform whether it
!     needs to execute (REQ-REV-006 steps 2-4), execute if so, advance
!     every declared output's revision together (REQ-REV-007), and
!     record the new baseline (REQ-REV-006 step 6).
!   - any other GraphNode kind (e.g. a future MethodGraphNode): not
!     demand-driven per REQ-NODE-007 - intentionally not scheduled here,
!     documented via an explicit class default branch.
!
! No cycle/visited-set bookkeeping is added: acyclicity is already a
! graph invariant enforced by DependencyNetwork%validate()/
! ComponentGraph%freeze(), and re-visiting an already-current node
! through a diamond dependency is redundant work, not incorrect (a
! second needs_execution() check against an unchanged baseline
! correctly reports "not stale"). Optimizing repeated-visit cost is
! exactly what a future compiled form (spec/11 §11.5, Q9) would address,
! not this runtime-interpreted reference implementation.
!
! Iteration convention: every gFTL walk here uses ftn_begin()/ftn_end()
! with the %next() call at the top of the loop body, matching
! DependencyNetwork/ComponentGraph. Every local gFTL container iterated
! this way (or subject to %at()) is declared TARGET, since the pointer/
! iterator association it produces must remain valid across the whole
! loop, not merely for the duration of the call that produced it.
!------------------------------------------------------------------------------
submodule (mapl_ComponentGraph_mod) mapl_ComponentGraph_DemandDrivenUpdate_smod
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_TransformGraphNode_mod, only: TransformGraphNode, PortSpecMap, PortSpecMapIterator, &
                                       PortNameRevisionMap, operator(==), operator(/=)
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_NodeIdSet_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Public entry point (ComponentGraph%update()): recursively ensures
   ! node_id's value is current within network_id.
   module recursive subroutine graph_update(this, network_id, node_id, rc)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      class(GraphNode), pointer :: node
      integer :: status

      node => this%get_node(node_id)
      _ASSERT(associated(node), 'ComponentGraph: update - unknown NodeId')

      select type (node)
      class is (StateItemNode)
         call update_state_item(this, network_id, node_id, _RC)
      class is (TransformGraphNode)
         call ensure_transform_current(this, network_id, node_id, node, _RC)
      class default
         ! Not demand-driven (REQ-NODE-007) - e.g. a future
         ! MethodGraphNode, or a bare BaseGraphNode with no more
         ! specific role. Intentionally a no-op, not an oversight.
      end select

      _RETURN(_SUCCESS)
   end subroutine graph_update

   ! REQ-REV-006 step 1 (the StateItemNode side of it): a value with no
   ! producer in this network is externally supplied - nothing to
   ! demand-drive. A value with exactly one producer recurses into it
   ! (REQ-DEP-008 already forbids more than one within a single
   ! network; the count()==1 assert below is a defensive check, not new
   ! validation logic).
   recursive subroutine update_state_item(graph, network_id, node_id, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
      type(NodeIdSet), target :: predecessors
      type(NodeIdSetIterator) :: iter
      type(NodeId) :: producer_id
      integer :: producer_count
      integer :: status

      network => graph%get_network(network_id)
      _ASSERT(associated(network), 'ComponentGraph: update - unknown DependencyNetworkId')

      predecessors = network%get_predecessors(node_id)

      producer_count = 0
      iter = predecessors%ftn_begin()
      do while (iter /= predecessors%ftn_end())
         call iter%next()
         producer_id = iter%of()
         producer_count = producer_count + 1
      end do

      if (producer_count == 0) then
         _RETURN(_SUCCESS)
      end if
      _ASSERT(producer_count == 1, 'ComponentGraph: update - state item has more than one producer in this network')

      ! producer_id still holds the sole predecessor assigned by the
      ! single loop iteration above (producer_count == 1 guarantees
      ! exactly one assignment occurred).
      call graph%update(network_id, producer_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine update_state_item

   ! REQ-REV-006 steps 1-6 (the TransformGraphNode side).
   recursive subroutine ensure_transform_current(graph, network_id, transform_id, transform_node, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: transform_id
      class(TransformGraphNode), intent(inout) :: transform_node
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
      type(NodeIdSet), target :: predecessors
      type(NodeIdSetIterator) :: pred_iter
      type(NodeId) :: predecessor_id
      type(PortNameRevisionMap), target :: current_input_revisions
      type(PortSpecMap), target :: input_ports, output_ports
      type(PortSpecMapIterator) :: port_iter
      character(:), allocatable :: port_name
      type(NodeId), pointer :: bound_id
      class(GraphNode), pointer :: bound_node
      type(NodeRevision) :: bound_revision
      integer :: status

      network => graph%get_network(network_id)
      _ASSERT(associated(network), 'ComponentGraph: update - unknown DependencyNetworkId')

      ! Step 1: recursively update every predecessor first (this
      ! transform's declared inputs, per spec/10 §10.3's "adjacency and
      ! bindings describe the same edges" framing).
      predecessors = network%get_predecessors(transform_id)
      pred_iter = predecessors%ftn_begin()
      do while (pred_iter /= predecessors%ftn_end())
         call pred_iter%next()
         predecessor_id = pred_iter%of()
         call graph%update(network_id, predecessor_id, _RC)
      end do

      ! Steps 2-3: assemble the current declared-input revision snapshot
      ! via the external port-binding table (REQ-XFORM-005).
      input_ports = transform_node%get_input_ports()
      port_iter = input_ports%ftn_begin()
      do while (port_iter /= input_ports%ftn_end())
         call port_iter%next()
         port_name = port_iter%first()

         bound_id => graph%get_port_binding(network_id, transform_id, port_name)
         _ASSERT(associated(bound_id), 'ComponentGraph: update - declared input port has no binding')

         bound_node => graph%get_node(bound_id)
         _ASSERT(associated(bound_node), 'ComponentGraph: update - bound input NodeId does not exist')

         select type (bound_node)
         class is (StateItemNode)
            bound_revision = bound_node%get_revision()
         class default
            _ASSERT(.false., 'ComponentGraph: update - bound input NodeId is not a StateItemNode')
         end select

         call current_input_revisions%insert(port_name, bound_revision)
      end do

      ! Step 4: execute only if never run, or if stale.
      if (transform_node%needs_execution(current_input_revisions)) then
         call transform_node%execute(_RC)

         ! Step 5: advance every declared output together (REQ-REV-007:
         ! all-or-nothing, never partial).
         output_ports = transform_node%get_output_ports()
         port_iter = output_ports%ftn_begin()
         do while (port_iter /= output_ports%ftn_end())
            call port_iter%next()
            port_name = port_iter%first()

            bound_id => graph%get_port_binding(network_id, transform_id, port_name)
            _ASSERT(associated(bound_id), 'ComponentGraph: update - declared output port has no binding')

            bound_node => graph%get_node(bound_id)
            _ASSERT(associated(bound_node), 'ComponentGraph: update - bound output NodeId does not exist')

            select type (bound_node)
            class is (StateItemNode)
               call bound_node%advance_revision(_RC)
            class default
               _ASSERT(.false., 'ComponentGraph: update - bound output NodeId is not a StateItemNode')
            end select
         end do

         ! Step 6: the executed input revisions become the new baseline.
         call transform_node%record_run(current_input_revisions, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine ensure_transform_current

end submodule mapl_ComponentGraph_DemandDrivenUpdate_smod
