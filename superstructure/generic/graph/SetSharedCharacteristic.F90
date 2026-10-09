#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_SetSharedCharacteristic_mod: the dedicated, synchronous mutator
! entry point REQ-CHAR-016 requires for changing a shared
! ReferenceCharacteristic's underlying value (docs/graph/spec/
! 18-state-item-characteristics.md §18.8, openspec/changes/
! state-item-characteristics design.md D7) - "a MAPL_SetGeom-style
! wrapper" in shape, though this change wires no real call site to it
! (design.md Non-Goals).
!
! set_shared_characteristic(graph, node_id, new_payload, rc), in one
! synchronous call:
!   1. Installs `new_payload` as `node_id`'s own GraphStateItem payload.
!   2. Walks `graph`'s default DependencyNetwork%get_successors from
!      `node_id`, filtering to StateItemNode successors only
!      (TransformGraphNode successors are left to the lazy,
!      REQ-REV-006 path - REQ-CHAR-016's own "left to the lazy path").
!      For each such dependent: resets its own revision to the invalid
!      sentinel (design.md D7's mechanism note - a fresh
!      default-constructed NodeRevision, via the dependent's own existing
!      set_revision() setter; no new NodeRevision/StateItemNode API is
!      added), and, if the dependent itself holds a
!      ReferenceCharacteristic entry referencing this same `node_id`,
!      marks that entry's CharacteristicStatus INVALID too (REQ-CHAR-004,
!      best-effort - absence of such an entry is not an error).
!   3. Advances `node_id`'s own revision via its existing
!      advance_revision() (forward, never reset - distinct from step 2's
!      dependents, design.md D7's mechanism note).
!
! REQ-CHAR-017: never looks up or invokes a MethodGraphNode anywhere in
! this call graph - only DependencyNetwork query methods and each
! affected StateItemNode's/GraphStateItem's own structural-reset/revision
! methods. This is what makes the call safe from any phase/nesting
! context (§18.9's rejected "dedicated ChangeGeom phase" alternative is
! not needed).
!
! Scope note (design.md D7 addendum): only the graph-neutral half of
! "structural reset" is performed here (revision/status, synthetic-node
! testable, no ESMF involvement) - actually reallocating an ESMF payload
! to the new shape (e.g. ESMF_FieldEmptyReset) is real ESMF-aware work
! left to whichever future integration layer wires this mutator into a
! real call site; not attempted here.
!------------------------------------------------------------------------------
module mapl_SetSharedCharacteristic_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_NodeId_mod, only: NodeId, operator(==)
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic, &
                                                StateItemCharacteristicMap, StateItemCharacteristicMapIterator, &
                                                operator(==), operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_CharacteristicStatus_mod, only: CHARACTERISTIC_STATUS_INVALID
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: set_shared_characteristic

contains

   subroutine set_shared_characteristic(graph, node_id, new_payload, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(GraphStateItem), intent(in) :: new_payload
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: generic_node
      class(StateItemNode), pointer :: shared_node
      type(DependencyNetwork), pointer :: network
      type(NodeIdSet) :: successors
      type(NodeId), allocatable :: dependent_ids(:)
      integer :: i

      ! Step 1: install the new payload on the shared node itself.
      generic_node => graph%get_node(node_id)
      _ASSERT(associated(generic_node), 'set_shared_characteristic: node_id not found in graph')
      select type (generic_node)
      class is (StateItemNode)
         shared_node => generic_node
      class default
         _FAIL('set_shared_characteristic: node_id does not identify a StateItemNode')
      end select
      call shared_node%set_payload(new_payload)

      ! Step 2: eager structural reset of direct StateItemNode successors
      ! only (TransformGraphNode successors are left to the lazy path).
      !
      ! Collect-then-process (not a single combined loop): drain the
      ! successor set's own iterator into a plain NodeId array first,
      ! with nothing but the iterator's own methods and plain
      ! array-element assignment in that loop; only then run an
      ! ordinary indexed loop that calls another procedure
      ! (reset_dependent_if_state_item, which itself does further graph
      ! lookups/mutations) per element. Same discipline already
      ! established in this module family for exactly this reason -
      ! MethodInvocation.F90's own header comment: a set/map iterator
      ! that must stay valid across a potentially-complex intervening
      ! call is not safe to keep live across that call. Confirmed the
      ! hard way here: a single combined loop (iterator comparison +
      ! reset_dependent_if_state_item call inside the same do-while)
      ! produced a NAG runtime dangling-pointer abort
      ! (MAPL_NODEIDSET_MOD:SET_FTN_BEGIN) - this is not merely a
      ! gfortran-specific risk, as that header comment's own "on general
      ! principle" framing already anticipated.
      network => graph%get_network(graph%get_default_network_id())
      _ASSERT(associated(network), 'set_shared_characteristic: default DependencyNetwork not found')
      successors = network%get_successors(node_id)
      call collect_node_ids(successors, dependent_ids)

      do i = 1, size(dependent_ids)
         call reset_dependent_if_state_item(graph, node_id, dependent_ids(i), _RC)
      end do

      ! Step 3: advance the shared node's own revision last, so
      ! content-side (lazy) consumers recognize staleness on next demand
      ! (REQ-CHAR-018/REQ-REV-006, unchanged).
      call shared_node%advance_revision(_RC)

      _RETURN(_SUCCESS)
   end subroutine set_shared_characteristic

   ! One clean, uninterrupted iteration pass - nothing but the
   ! iterator's own methods and plain array-element assignment happen in
   ! this loop (see set_shared_characteristic's own comment on why).
   subroutine collect_node_ids(set, ids)
      type(NodeIdSet), target, intent(in) :: set
      type(NodeId), allocatable, intent(out) :: ids(:)

      type(NodeIdSetIterator) :: iter
      integer :: n, i

      n = int(set%size())
      allocate(ids(n))

      iter = set%ftn_begin()
      do i = 1, n
         call iter%next()
         ids(i) = iter%of()
      end do
   end subroutine collect_node_ids

   ! Resets one direct successor if (and only if) it is itself a
   ! StateItemNode (REQ-CHAR-016's own "as opposed to TransformGraphNodes,
   ! which are left to the lazy path" - a TransformGraphNode successor is
   ! silently skipped, not an error).
   subroutine reset_dependent_if_state_item(graph, shared_node_id, dependent_id, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: shared_node_id
      type(NodeId), intent(in) :: dependent_id
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: generic_node
      class(StateItemNode), pointer :: dependent_node
      type(NodeRevision) :: invalid_revision

      generic_node => graph%get_node(dependent_id)
      _ASSERT(associated(generic_node), 'set_shared_characteristic: dependent NodeId not found in graph')

      select type (generic_node)
      class is (StateItemNode)
         dependent_node => generic_node
      class default
         ! Not a StateItemNode (e.g. a TransformGraphNode) - left to the
         ! lazy path, REQ-CHAR-016's own scope boundary.
         _RETURN(_SUCCESS)
      end select

      ! REQ-CHAR-004: reset this dependent's own revision to the invalid
      ! sentinel - a fresh default-constructed NodeRevision is already
      ! invalid by construction (design.md D7's mechanism note); no new
      ! NodeRevision/StateItemNode API is added.
      call dependent_node%set_revision(invalid_revision)

      ! Best-effort: if this dependent itself holds a
      ! ReferenceCharacteristic entry referencing the same shared node,
      ! mark that entry's status INVALID too (REQ-CHAR-004). Absence of
      ! such an entry is not an error - plenty of structural dependents
      ! are reached through plain DependencyNetwork adjacency with no
      ! StateItemCharacteristic entry of their own.
      call invalidate_matching_reference_characteristics(dependent_node, shared_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine reset_dependent_if_state_item

   subroutine invalidate_matching_reference_characteristics(dependent_node, shared_node_id, rc)
      class(StateItemNode), intent(inout) :: dependent_node
      type(NodeId), intent(in) :: shared_node_id
      integer, optional, intent(out) :: rc

      type(GraphStateItem) :: payload
      type(StateItemCharacteristicMap) :: characteristics
      type(StateItemCharacteristicKind), allocatable :: matching_kinds(:)
      class(StateItemCharacteristic), allocatable :: characteristic
      integer :: i, status

      payload = dependent_node%get_payload()
      characteristics = payload%get_characteristics()

      ! Collect-then-process (set_shared_characteristic's own comment
      ! explains why): find which kinds are matching ReferenceCharacteristic
      ! entries first, with no mutation and no other procedure call while
      ! the map iterator is live; only then mutate payload's own map in
      ! an ordinary indexed loop.
      call collect_matching_reference_kinds(characteristics, shared_node_id, matching_kinds)

      do i = 1, size(matching_kinds)
         call payload%get_characteristic(matching_kinds(i), characteristic, rc=status)
         _ASSERT(status == _SUCCESS, 'set_shared_characteristic: matched kind unexpectedly absent')
         call characteristic%set_status(CHARACTERISTIC_STATUS_INVALID)
         call payload%set_characteristic(matching_kinds(i), characteristic)
      end do

      call dependent_node%set_payload(payload)

      _RETURN(_SUCCESS)
   end subroutine invalidate_matching_reference_characteristics

   ! One clean, uninterrupted iteration pass (see set_shared_characteristic's
   ! own comment): no mutation, no other procedure call, while `map`'s
   ! iterator is live.
   subroutine collect_matching_reference_kinds(map, shared_node_id, matching_kinds)
      type(StateItemCharacteristicMap), target, intent(in) :: map
      type(NodeId), intent(in) :: shared_node_id
      type(StateItemCharacteristicKind), allocatable, intent(out) :: matching_kinds(:)

      type(StateItemCharacteristicMapIterator) :: iter
      type(StateItemCharacteristicKind) :: kind
      class(StateItemCharacteristic), pointer :: characteristic

      allocate(matching_kinds(0))

      iter = map%ftn_begin()
      do while (iter /= map%ftn_end())
         call iter%next()
         kind = iter%first()
         characteristic => iter%second()

         select type (characteristic)
         class is (ReferenceCharacteristic)
            if (characteristic%get_referenced_node_id() == shared_node_id) then
               matching_kinds = [matching_kinds, kind]
            end if
         end select
      end do
   end subroutine collect_matching_reference_kinds

end module mapl_SetSharedCharacteristic_mod
