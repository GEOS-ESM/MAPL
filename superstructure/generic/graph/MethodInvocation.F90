#include "MAPL.h"

!------------------------------------------------------------------------------
! MethodInvocation: REQ-MTH-003a's trigger/advance discipline around a
! MethodGraphNode invocation on the default DependencyNetwork (REQ-REV-011),
! as one graph-neutral procedure rather than an OuterMetaComponent method.
!
! invoke_on_default_network() applies REQ-REV-011's default-network rule,
! unconditionally and all-or-nothing:
!   1. Before invocation: ComponentGraph%update() (REQ-REV-006) over every
!      one of the method node's bound IN/INOUT arguments.
!   2. Invoke the node.
!   3. Only if invocation succeeds: advance the NodeRevision of every one of
!      the method node's bound OUT/INOUT arguments.
!
! REQ-REV-011a: the discipline is "controlled by the OuterMetaComponent
! layer" through who calls this procedure, not by the mechanism living inside
! OuterMetaComponent; it needs only ComponentGraph/MethodGraphNode/
! NodeRevision, so it is testable on a synthetic graph.
!
! Collect-then-process, not one combined loop (same gfortran-safety posture
! as ComponentGraph_DemandDrivenUpdate.F90 and DependencyNetwork.F90, where
! recursive procedures corrupted locals under gfortran).
! collect_bound_names() keeps the StateItemMemberMap iterator loop to
! iteration and plain array-element assignment, with no call to any other
! user-defined procedure while the iterator is live. Two indexed loops
! (pull_bound_inputs/advance_bound_outputs) then do the per-argument work
! (get_argument, graph%update, advance_revision) with no map iterator
! involved. This also insulates the iterator from ComponentGraph%update(),
! which walks the whole DependencyNetwork.
module mapl_MethodInvocation_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_MethodGraphNode_mod, only: MethodGraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_ArgumentSpec_mod, only: ArgumentSpec
   use mapl_AccessSpec_mod, only: AccessSpec, operator(==), &
        MAPL_ACCESS_IN, MAPL_ACCESS_OUT, MAPL_ACCESS_INOUT
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap, StateItemMemberMapIterator, operator(/=)
   use ESMF, only: ESMF_Clock
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: invoke_on_default_network

   ! Argument names are short, framework-declared identifiers (unlike a
   ! component's advertised item short_name); 128 is a generous bound.
   integer, parameter :: MAX_ARGUMENT_NAME_LEN = 128

contains

   subroutine invoke_on_default_network(graph, node_id, rc, clock)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc
      type(ESMF_Clock), optional, intent(in) :: clock

      integer :: status
      class(GraphNode), pointer :: generic_node
      class(MethodGraphNode), pointer :: method_node
      type(StateItemMemberMap) :: bindings
      character(MAX_ARGUMENT_NAME_LEN), allocatable :: names(:)

      generic_node => graph%get_node(node_id)
      _ASSERT(associated(generic_node), 'invoke_on_default_network: node_id not found in graph')

      select type (generic_node)
      class is (MethodGraphNode)
         method_node => generic_node
      class default
         _FAIL('invoke_on_default_network: node_id does not identify a MethodGraphNode')
      end select

      bindings = method_node%get_argument_bindings()
      call collect_bound_names(bindings, names)

      call pull_bound_inputs(graph, method_node, bindings, names, _RC)

      call method_node%invoke(rc=status, clock=clock)
      _VERIFY(status)

      call advance_bound_outputs(graph, method_node, bindings, names, _RC)

      _RETURN(_SUCCESS)
   end subroutine invoke_on_default_network

   ! One uninterrupted iteration pass: only the iterator's own methods and
   ! plain array-element assignment (not even a container call like
   ! StringVector%push_back()); see the module header.
   subroutine collect_bound_names(bindings, names)
      type(StateItemMemberMap), target, intent(in) :: bindings
      character(MAX_ARGUMENT_NAME_LEN), allocatable, intent(out) :: names(:)

      type(StateItemMemberMapIterator) :: iter
      integer :: n

      allocate(names(bindings%size()))

      n = 0
      iter = bindings%ftn_begin()
      do while (iter /= bindings%ftn_end())
         call iter%next()
         n = n + 1
         names(n) = iter%first()
      end do
   end subroutine collect_bound_names

   ! REQ-REV-011's "pull-all-before": ComponentGraph%update() over every bound
   ! IN/INOUT argument, on the default network. Indexed loop over the
   ! pre-collected name list; no map iterator.
   subroutine pull_bound_inputs(graph, method_node, bindings, names, rc)
      class(ComponentGraph), target, intent(in) :: graph
      class(MethodGraphNode), intent(in) :: method_node
      type(StateItemMemberMap), target, intent(in) :: bindings
      character(MAX_ARGUMENT_NAME_LEN), intent(in) :: names(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      character(:), allocatable :: name
      type(NodeId), pointer :: bound_id
      type(ArgumentSpec) :: arg
      type(AccessSpec) :: access

      do i = 1, size(names)
         name = trim(names(i))
         bound_id => bindings%at(name)
         _ASSERT(associated(bound_id), 'pull_bound_inputs: bound name not found in bindings')

         arg = method_node%get_argument(name, _RC)
         access = arg%get_access()

         if (access == MAPL_ACCESS_IN .or. access == MAPL_ACCESS_INOUT) then
            call graph%update(graph%get_default_network_id(), bound_id, _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine pull_bound_inputs

   ! REQ-REV-011's "advance-all-after": NodeRevision%advance() on every bound
   ! OUT/INOUT argument, reached only once invoke() has succeeded (the verify macro
   ! above returns early otherwise). Indexed loop, as in pull_bound_inputs.
   subroutine advance_bound_outputs(graph, method_node, bindings, names, rc)
      class(ComponentGraph), target, intent(in) :: graph
      class(MethodGraphNode), intent(in) :: method_node
      type(StateItemMemberMap), target, intent(in) :: bindings
      character(MAX_ARGUMENT_NAME_LEN), intent(in) :: names(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      character(:), allocatable :: name
      type(NodeId), pointer :: bound_id
      type(ArgumentSpec) :: arg
      type(AccessSpec) :: access
      class(GraphNode), pointer :: generic_node

      do i = 1, size(names)
         name = trim(names(i))
         bound_id => bindings%at(name)
         _ASSERT(associated(bound_id), 'advance_bound_outputs: bound name not found in bindings')

         arg = method_node%get_argument(name, _RC)
         access = arg%get_access()

         if (access == MAPL_ACCESS_OUT .or. access == MAPL_ACCESS_INOUT) then
            generic_node => graph%get_node(bound_id)
            _ASSERT(associated(generic_node), 'advance_bound_outputs: bound NodeId not found in graph')
            select type (generic_node)
            class is (StateItemNode)
               call generic_node%advance_revision(_RC)
            class default
               _FAIL('advance_bound_outputs: bound NodeId does not identify a StateItemNode')
            end select
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine advance_bound_outputs

end module mapl_MethodInvocation_mod
