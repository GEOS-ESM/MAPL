#include "MAPL.h"

!------------------------------------------------------------------------------
! set_shared_characteristic replaces one characteristic of an already
! connected graph item, for items whose geometry changes while the
! component runs (History samplers). In one synchronous call it sets the
! entry under `kind` in the node's descriptor, resets the direct
! StateItemNode successors' revisions so they are recomputed, and
! advances the node's own revision. TransformGraphNode successors are
! deliberately left to the lazy, demand-driven path.
!
! It never looks up or invokes a MethodGraphNode, which is what makes it
! safe to call from any phase or nesting context.
!
! The node's ESMF payload is not rebuilt to match the new
! characteristic; that needs a field-materialization helper that does
! not exist yet.
!
! Known gap: only downstream nodes are reset. When the changed item is
! an import, the transform step feeding it is upstream and keeps its
! stale state (e.g. old regrid weights).
!------------------------------------------------------------------------------
module mapl_SetSharedCharacteristic_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
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

   subroutine set_shared_characteristic(graph, node_id, kind, characteristic, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), intent(in) :: characteristic
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: generic_node
      class(StateItemNode), pointer :: shared_node
      type(StateItemDescriptor), pointer :: descriptor
      type(DependencyNetwork), pointer :: network
      type(NodeIdSet) :: successors
      type(NodeId), allocatable :: dependent_ids(:)
      integer :: i

      ! Step 1: replace the characteristic on the shared node itself.
      generic_node => graph%get_node(node_id)
      _ASSERT(associated(generic_node), 'set_shared_characteristic: node_id not found in graph')
      select type (generic_node)
      class is (StateItemNode)
         shared_node => generic_node
      class default
         _FAIL('set_shared_characteristic: node_id does not identify a StateItemNode')
      end select
      descriptor => shared_node%get_descriptor()
      call descriptor%set_characteristic(kind, characteristic, _RC)

      ! Step 2: eager structural reset of direct StateItemNode successors
      ! only (TransformGraphNode successors are left to the lazy path).
      !
      ! Drain the successor iterator into a plain array before the loop
      ! that calls out per element: keeping the iterator live across
      ! reset_dependent_if_state_item, which does further graph lookups
      ! and mutations, produced a NAG dangling-pointer abort.
      network => graph%get_network(graph%get_default_network_id())
      _ASSERT(associated(network), 'set_shared_characteristic: default DependencyNetwork not found')
      successors = network%get_successors(node_id)
      call collect_node_ids(successors, dependent_ids)

      do i = 1, size(dependent_ids)
         call reset_dependent_if_state_item(graph, node_id, dependent_ids(i), _RC)
      end do

      ! Step 3: advance the shared node's own revision last, so
      ! content-side (lazy) consumers recognize staleness on next demand.
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
   ! StateItemNode; a TransformGraphNode successor is silently skipped,
   ! not an error, because it is served by the lazy path.
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
         ! lazy path.
         _RETURN(_SUCCESS)
      end select

      ! Reset this dependent's own revision to the invalid sentinel - a
      ! fresh default-constructed NodeRevision is already invalid by
      ! construction, so no new NodeRevision/StateItemNode API is needed.
      call dependent_node%set_revision(invalid_revision)

      ! Best-effort: if this dependent itself holds a
      ! ReferenceCharacteristic entry referencing the same shared node,
      ! mark that entry's status INVALID too. Absence of
      ! such an entry is not an error - plenty of structural dependents
      ! are reached through plain DependencyNetwork adjacency with no
      ! StateItemCharacteristic entry of their own.
      call invalidate_matching_reference_characteristics(dependent_node, shared_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine reset_dependent_if_state_item

   subroutine invalidate_matching_reference_characteristics(dependent_node, shared_node_id, rc)
      class(StateItemNode), target, intent(inout) :: dependent_node
      type(NodeId), intent(in) :: shared_node_id
      integer, optional, intent(out) :: rc

      type(StateItemDescriptor), pointer :: descriptor
      type(StateItemCharacteristicMap) :: characteristics
      type(StateItemCharacteristicKind), allocatable :: matching_kinds(:)
      class(StateItemCharacteristic), allocatable :: characteristic
      integer :: i, status

      descriptor => dependent_node%get_descriptor()
      characteristics = descriptor%get_characteristics()

      ! Collect-then-process (set_shared_characteristic's own comment
      ! explains why): find which kinds are matching ReferenceCharacteristic
      ! entries first, with no mutation and no other procedure call while
      ! the map iterator is live; only then mutate the descriptor's own map
      ! in an ordinary indexed loop.
      call collect_matching_reference_kinds(characteristics, shared_node_id, matching_kinds)

      do i = 1, size(matching_kinds)
         call descriptor%get_characteristic(matching_kinds(i), characteristic, rc=status)
         _ASSERT(status == _SUCCESS, 'set_shared_characteristic: matched kind unexpectedly absent')
         call characteristic%set_status(CHARACTERISTIC_STATUS_INVALID)
         call descriptor%set_characteristic(matching_kinds(i), characteristic)
      end do

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
