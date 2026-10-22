#include "MAPL.h"

!------------------------------------------------------------------------------
! Submodule implementing ComponentGraph%update(), the runtime-interpreted
! demand-driven update algorithm (REQ-REV-005..007, REQ-REV-009). It is a
! submodule (not a separate module) so it can use ComponentGraph's private
! components; see ComponentGraph.F90's header for the contrast with
! mapl_GraphExport_mod.
!
! update(this, network_id, node_id, rc): ensure this node's value is current
! within this network. Dispatches on the node's dynamic kind:
!
!   - StateItemNode: find its sole producer (predecessor) in this network.
!     Zero predecessors means externally supplied - nothing to do. More than
!     one is a defensive assert (REQ-DEP-008 validation already forbids it).
!     If the sole predecessor is a TransformGraphNode, update it first.
!   - TransformGraphNode: update every DependencyNetwork predecessor first
!     (REQ-REV-006 step 1; adjacency and the port-binding table describe the
!     same edges, so these are exactly the declared inputs). Then assemble
!     the current input-revision snapshot via get_port_binding()
!     (REQ-XFORM-005), ask the transform whether it needs to execute
!     (REQ-REV-006 steps 2-4), execute if so, advance every declared output's
!     revision together (REQ-REV-007) and record the new baseline
!     (REQ-REV-006 step 6).
!   - any other GraphNode kind (e.g. MethodGraphNode): not demand-driven
!     (REQ-NODE-007), deliberately not scheduled here.
!
! No cycle/visited-set bookkeeping: acyclicity is enforced by
! DependencyNetwork%validate()/ComponentGraph%freeze(), and re-visiting a
! node through a diamond dependency is redundant, not incorrect (a second
! needs_execution() check against an unchanged baseline reports "not stale").
!
! Iteration convention: gFTL walks use ftn_begin()/ftn_end() with %next() at
! the top of the loop body. Local gFTL containers that are iterated or
! subject to %at() are TARGET so the pointers/iterators stay valid across the
! whole loop.
!
! The traversal is explicit-stack, not Fortran recursion; see graph_update.
!------------------------------------------------------------------------------
submodule (mapl_ComponentGraph_mod) mapl_ComponentGraph_DemandDrivenUpdate_smod
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_TransformGraphNode_mod, only: TransformGraphNode, operator(==), operator(/=)
   use mapl_PortSpecMap_mod, only: PortSpecMap, PortSpecMapIterator
   use mapl_PortNameRevisionMap_mod, only: PortNameRevisionMap
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_NodeIdSet_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)

   ! UpdateFrame is declared in the ancestor module (ComponentGraph.F90), not
   ! here; see the comment there.

contains

   ! Public entry point (ComponentGraph%update()): ensures node_id's value is
   ! current within network_id.
   !
   ! Explicit-stack (non-recursive) traversal: mutually recursive update
   ! procedures corrupted local variables on return from a deeper call under
   ! gfortran (same signature as DependencyNetwork's has_cycle_from; never seen
   ! under NAG). The work splits into two phases with no other shared state:
   ! a post-order DFS over the predecessor DAG (make_update_frame() discovers
   ! predecessors) and each node's own non-recursive work
   ! (finish_update_frame()/finish_transform()).
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

      ! The error-handling macro expands to more than one statement, so it
      ! cannot appear inside an array constructor; assign to a local first.
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

   ! Builds one graph_update stack frame for node_id: the node itself and the
   ! list of predecessor NodeIds that must be current before its own work runs.
   !
   ! REQ-REV-006 step 1: a StateItemNode with no producer in this network is
   ! externally supplied (empty predecessor list); with one producer it depends
   ! on it (the n <= 1 assert is defensive, REQ-DEP-008 already forbids more). A
   ! TransformGraphNode depends on every declared input. Any other GraphNode
   ! kind (e.g. MethodGraphNode) is not demand-driven (REQ-NODE-007): empty
   ! predecessor list, and a no-op in finish_update_frame.
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

   ! Dispatches a frame's own (non-recursive) work, once every predecessor it
   ! declared is current.
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

   ! REQ-REV-006 steps 2-6 (the TransformGraphNode side). Predecessor
   ! updates (step 1) are done by the caller via make_update_frame().
   subroutine finish_transform(graph, network_id, transform_id, transform_node, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: transform_id
      class(TransformGraphNode), intent(inout) :: transform_node
      integer, optional, intent(out) :: rc

      type(PortNameRevisionMap), target :: current_input_revisions
      type(PortNameGraphStateItemMap), target :: input_items, output_items
      type(PortSpecMap), target :: input_ports, output_ports
      type(PortSpecMapIterator) :: port_iter
      character(:), pointer :: port_name
      type(NodeId), pointer :: bound_id
      class(GraphNode), pointer :: bound_node
      type(NodeRevision) :: bound_revision
      integer :: status

      ! Steps 2-3: assemble the current declared-input revision snapshot via the
      ! external port-binding table (REQ-XFORM-005) and, in the same loop, each
      ! bound input port's GraphStateItem value, to pass into execute() if needed.
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
            call input_items%insert(port_name, bound_node%get_payload())
         class default
            _ASSERT(.false., 'ComponentGraph: update - bound input NodeId is not a StateItemNode')
         end select

         call current_input_revisions%insert(port_name, bound_revision)
      end do

      ! Step 4: execute only if never run, or if stale.
      if (transform_node%needs_execution(current_input_revisions)) then

         ! Resolve each declared output port's bound GraphStateItem value to pass
         ! into execute(); a separate loop from the post-execution one below, which
         ! advances revisions.
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
               call output_items%insert(port_name, bound_node%get_payload())
            class default
               _ASSERT(.false., 'ComponentGraph: update - bound output NodeId is not a StateItemNode')
            end select
         end do

         call transform_node%execute(input_items, output_items, _RC)

         ! Step 5: advance every declared output together (REQ-REV-007:
         ! all-or-nothing, never partial).
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
