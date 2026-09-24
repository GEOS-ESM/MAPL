#include "MAPL.h"

!------------------------------------------------------------------------------
! MethodInvocation: REQ-MTH-003a's trigger/advance discipline around a
! MethodGraphNode invocation on the default DependencyNetwork
! (spec/12-methods-and-drivers.md sec 12.2, spec/11-revision-and-update.md
! REQ-REV-011) - griddedcomponentdriver-integration-lifecycle design.md
! Decisions ("REQ-MTH-003a's trigger/advance discipline is one new
! graph-neutral procedure, not an OuterMetaComponent method").
!
! invoke_on_default_network() does exactly REQ-REV-011's default-network
! rule, unconditionally/all-or-nothing:
!   1. Before invocation: ComponentGraph%update() (REQ-REV-006) over
!      every one of the method node's bound IN/INOUT arguments.
!   2. Invoke the node.
!   3. Only if invocation succeeds: advance the NodeRevision of every
!      one of the method node's bound OUT/INOUT arguments.
!
! REQ-REV-011a: this discipline is "controlled by the OuterMetaComponent
! layer" through *who calls* this procedure (an OuterMetaComponent call
! site, once one exists), not by the mechanism living inside
! OuterMetaComponent itself - this procedure needs only
! ComponentGraph/MethodGraphNode/NodeRevision, exactly Phase 1-3's
! existing synthetic-graph-testable shape, no OuterMetaComponent, no
! ESMF component.
!
! Explicit-worklist (collect-then-process), not a single combined loop -
! same gfortran-safety posture already established in this module
! family (ComponentGraph_DemandDrivenUpdate.F90's own explicit-stack
! rewrite, DependencyNetwork.F90's own explicit-worklist search): both
! document "a recursive Fortran procedure reliably corrupted its own
! local [state] on return from a deeper recursive call under gfortran."
! collect_bound_names() below keeps the StateItemMemberMap
! iterator (%ftn_begin/%ftn_end/%next/%first) loop doing nothing but
! iteration and plain array-element assignment - no call to any other
! user-defined procedure while the iterator is live - then hands off to
! two ordinary indexed loops (pull_bound_inputs/advance_bound_outputs)
! that do the real per-argument work (get_argument, graph%update,
! advance_revision) with no map iterator involved at all. This was
! defensive hardening applied while chasing a real gfortran test
! failure in this area; the failure's actual root cause turned out to
! be unrelated to this file (a shared test-fixture helper returning a
! freshly-built ComponentGraph through an intent(out) dummy argument -
! see Test_MethodInvocation.pf's own header comment) - but the
! collect-then-process shape here is kept regardless, both because it
! matches this module family's own established precedent and because a
! map iterator that must stay valid across several potentially-complex
! intervening calls (ComponentGraph%update() in particular, which walks
! the whole DependencyNetwork) is worth insulating from on general
! principle, not only for this one now-understood failure.
!------------------------------------------------------------------------------
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

   ! Argument names are short, framework-declared identifiers (unlike
   ! e.g. a component's own advertised item short_name) - 128 is a
   ! generous bound, matching this module's own test fixtures ('x_in'
   ! etc.) and MethodGraphNode's own callback-argument-name use case.
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

   ! One clean, uninterrupted iteration pass - nothing but the
   ! iterator's own methods and plain array-element assignment happen
   ! in this loop (module header - why this must stay separate from the
   ! per-name work below, and why it avoids even a container call like
   ! StringVector%push_back()).
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

   ! REQ-REV-011's "pull-all-before": ComponentGraph%update() over every
   ! bound IN/INOUT argument, on the default network. Ordinary indexed
   ! loop over the pre-collected name list - no map iterator here.
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

   ! REQ-REV-011's "advance-all-after": NodeRevision%advance() on every
   ! bound OUT/INOUT argument - only reached once invoke() has already
   ! succeeded (invoke_on_default_network's own _VERIFY above returns
   ! early otherwise, spec scenario "Failed invocation does not advance
   ! bound outputs"). Ordinary indexed loop, same reason as
   ! pull_bound_inputs above.
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
