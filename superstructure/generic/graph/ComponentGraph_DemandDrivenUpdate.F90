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
!
! Implementation note: the description above is written in terms of the
! conceptual traversal (predecessors updated before the node that needs
! them); the actual implementation (graph_update below) is an
! explicit-stack, non-recursive traversal, not literal Fortran
! recursion - see graph_update's own header comment for why.
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

   ! One explicit-stack frame for graph_update's iterative rewrite (see
   ! that subroutine's own header comment for why this is no longer a
   ! recursive Fortran subroutine). Unlike DependencyNetwork's
   ! has_cycle_from (mapl_DependencyNetwork_mod), this traversal's
   ! per-node predecessor set is small and does not need a resumable
   ! iterator: it is materialized into a plain array once, up front,
   ! when the frame is created, and walked with a plain integer index.
   type :: UpdateFrame
      type(NodeId) :: node_id
      class(GraphNode), pointer :: node => null()
      type(NodeId), allocatable :: predecessor_ids(:)
      integer :: next_index = 1
   end type UpdateFrame

contains

   ! Public entry point (ComponentGraph%update()): ensures node_id's
   ! value is current within network_id, by whatever demand-driven work
   ! that requires. Explicit-stack (non-recursive) rewrite - the
   ! original mutually-recursive graph_update/update_state_item/
   ! ensure_transform_current trio reliably corrupted local variables on
   ! return from a deeper recursive call under gfortran (same failure
   ! signature confirmed in mapl_DependencyNetwork_mod's has_cycle_from;
   ! never reproduced under NAG) - removing Fortran call-stack
   ! recursion entirely sidesteps it regardless of compiler flags.
   !
   ! The original three-procedure split decomposed cleanly into two
   ! phases with no other cross-phase state: "make sure every
   ! predecessor is current first" (the only genuinely recursive part -
   ! a plain post-order DFS over the predecessor DAG, dispatch-by-kind
   ! only affects *which* predecessors a node has) and "now do this
   ! node's own work" (entirely self-contained per node - never itself
   ! recurses). Those become make_update_frame() (predecessor discovery)
   ! and finish_update_frame()/finish_transform() (the old steps 2-6,
   ! moved verbatim, non-recursive already) below.
   module subroutine graph_update(this, network_id, node_id, rc)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      type(UpdateFrame), allocatable :: stack(:)
      type(UpdateFrame) :: new_frame
      type(NodeId) :: next_id
      integer :: top
      integer :: status

      ! The error-handling convenience macro used below cannot be used
      ! inside another expression such as an array constructor - it
      ! expands to more than one statement - so each make_update_frame
      ! call is its own statement here, with the result assigned to a
      ! plain local before being folded into the stack array below.
      new_frame = make_update_frame(this, network_id, node_id, _RC)
      stack = [new_frame]

      do while (size(stack) > 0)
         top = size(stack)
         if (stack(top)%next_index <= size(stack(top)%predecessor_ids)) then
            next_id = stack(top)%predecessor_ids(stack(top)%next_index)
            stack(top)%next_index = stack(top)%next_index + 1
            new_frame = make_update_frame(this, network_id, next_id, _RC)
            stack = [stack, new_frame]
         else
            call finish_update_frame(this, network_id, stack(top), _RC)
            stack = stack(1:top - 1)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine graph_update

   ! Builds one graph_update stack frame for `node_id`: the node itself
   ! (dispatch target for finish_update_frame below) and the full list
   ! of predecessor NodeIds that must be current before this node's own
   ! work can run.
   !
   ! REQ-REV-006 step 1 (the StateItemNode side of it): a value with no
   ! producer in this network is externally supplied - nothing to
   ! demand-drive, an empty predecessor list. A value with exactly one
   ! producer depends on it (REQ-DEP-008 already forbids more than one
   ! within a single network; the count<=1 assert below is a defensive
   ! check, not new validation logic). A TransformGraphNode depends on
   ! every declared input (spec/10 §10.3's "adjacency and bindings
   ! describe the same edges" framing). Any other GraphNode kind (e.g. a
   ! future MethodGraphNode) is not demand-driven per REQ-NODE-007 - an
   ! empty predecessor list and a no-op in finish_update_frame,
   ! intentionally, not an oversight.
   function make_update_frame(this, network_id, node_id, rc) result(frame)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc
      type(UpdateFrame) :: frame

      type(DependencyNetwork), pointer :: network
      type(NodeIdSet), target :: predecessors
      type(NodeIdSetIterator) :: iter
      type(NodeId), pointer :: predecessor_id
      integer :: n, i
      integer :: status

      frame%node_id = node_id
      frame%next_index = 1
      frame%node => this%get_node(node_id)
      _ASSERT(associated(frame%node), 'ComponentGraph: update - unknown NodeId')

      select type (node => frame%node)
      class is (StateItemNode)
         network => this%get_network(network_id)
         _ASSERT(associated(network), 'ComponentGraph: update - unknown DependencyNetworkId')
         predecessors = network%get_predecessors(node_id)
         n = int(predecessors%size())
         _ASSERT(n <= 1, 'ComponentGraph: update - state item has more than one producer in this network')
         allocate(frame%predecessor_ids(n))
         if (n == 1) then
            iter = predecessors%ftn_begin()
            call iter%next()
            predecessor_id => iter%of()
            _ASSERT(associated(predecessor_id), 'ComponentGraph: update - iterator%of() unassociated')
            frame%predecessor_ids(1) = predecessor_id
         end if
      class is (TransformGraphNode)
         network => this%get_network(network_id)
         _ASSERT(associated(network), 'ComponentGraph: update - unknown DependencyNetworkId')
         predecessors = network%get_predecessors(node_id)
         n = int(predecessors%size())
         allocate(frame%predecessor_ids(n))
         iter = predecessors%ftn_begin()
         do i = 1, n
            call iter%next()
            predecessor_id => iter%of()
            _ASSERT(associated(predecessor_id), 'ComponentGraph: update - iterator%of() unassociated')
            frame%predecessor_ids(i) = predecessor_id
         end do
      class default
         allocate(frame%predecessor_ids(0))
      end select

      _RETURN(_SUCCESS)
   end function make_update_frame

   ! Dispatches a frame's own (non-recursive) work, once every
   ! predecessor it declared is already current.
   subroutine finish_update_frame(graph, network_id, frame, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(UpdateFrame), intent(in) :: frame
      integer, optional, intent(out) :: rc

      integer :: status

      select type (node => frame%node)
      class is (TransformGraphNode)
         call finish_transform(graph, network_id, frame%node_id, node, _RC)
      class default
         ! StateItemNode (nothing further to do once its sole producer,
         ! if any, is current) and any other non-demand-driven kind
         ! (REQ-NODE-007) - intentionally a no-op, not an oversight.
      end select

      _RETURN(_SUCCESS)
   end subroutine finish_update_frame

   ! REQ-REV-006 steps 2-6 (the TransformGraphNode side) - unchanged
   ! from the original ensure_transform_current except for its own step
   ! 1 (recursing into predecessors), which is now
   ! make_update_frame()'s job above; this part of the original was
   ! never itself recursive.
   subroutine finish_transform(graph, network_id, transform_id, transform_node, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: transform_id
      class(TransformGraphNode), intent(inout) :: transform_node
      integer, optional, intent(out) :: rc

      type(PortNameRevisionMap), target :: current_input_revisions
      type(PortSpecMap), target :: input_ports, output_ports
      type(PortSpecMapIterator) :: port_iter
      character(:), pointer :: port_name
      type(NodeId), pointer :: bound_id
      class(GraphNode), pointer :: bound_node
      type(NodeRevision) :: bound_revision
      integer :: status

      ! Steps 2-3: assemble the current declared-input revision snapshot
      ! via the external port-binding table (REQ-XFORM-005).
      input_ports = transform_node%get_input_ports()
      port_iter = input_ports%ftn_begin()
      do while (port_iter /= input_ports%ftn_end())
         call port_iter%next()
         port_name => port_iter%first()
         _ASSERT(associated(port_name), 'ComponentGraph: update - iterator%first() unassociated')

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
            port_name => port_iter%first()
            _ASSERT(associated(port_name), 'ComponentGraph: update - iterator%first() unassociated')

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
   end subroutine finish_transform

end submodule mapl_ComponentGraph_DemandDrivenUpdate_smod
