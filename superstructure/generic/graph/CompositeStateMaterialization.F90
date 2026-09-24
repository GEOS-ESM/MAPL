#include "MAPL.h"

!------------------------------------------------------------------------------
! CompositeStateMaterialization: recursive walk that turns a VariableSpec
! with declared members (superstructure/generic/specs/VariableSpec.F90,
! openspec/changes/composite-state-spec) into real graph structure - one
! StateItemNode per tree node (leaves and nested-state levels alike),
! with each nested level's GraphStateItem state_members map
! (spec/04-graph-value-hierarchy.md REQ-SI-006) populated with its
! declared children's NodeIds. Mirrors the existing
! ExtensionResolution/ExtensionMaterialization separation GraphBuilder.F90
! already depends on for a different concern (module header,
! GraphBuilder.F90:1-79) - a dedicated module GraphBuilder.F90 calls into,
! not inlined there.
!
! A "leaf" is a VariableSpec with no declared members - gets the exact
! same unallocated "advertised, not yet realized" GraphStateItem payload
! advertise_one already gives flat items (GraphBuilder.F90:207-252). A
! "nested" member is a VariableSpec with declared members - gets a real,
! structurally empty ESMF_State (design.md Decisions "Nested-state
! materialization allocates a real, structurally-empty ESMF_State") -
! enough to satisfy GraphStateItem's own esmf_kind()==STATE gate on
! state_members, not a realization of the state's real ESMF membership.
! Which role a given VariableSpec plays is read directly from whether it
! has declared members (get_member_names()) - no wrapper type, no
! itemType branch needed here (declare_member already enforces
! itemType==MAPL_STATEITEM_STATE at declaration time).
!------------------------------------------------------------------------------
module mapl_CompositeStateMaterialization_mod
   use ESMF, only: ESMF_State, ESMF_StateCreate
   use gFTL2_StringVector, only: StringVector
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_VariableSpec_mod, only: VariableSpec
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: materialize_composite
   public :: materialize_member

contains

   ! Builds a StateItemNode for var_spec's own top-level identity,
   ! recursing into every declared member first so the returned node's
   ! GraphStateItem is fully populated (state_members map complete) at
   ! the moment it is registered - avoids ever needing to fetch the node
   ! back out of the graph to mutate its payload after registration.
   recursive function materialize_composite(graph, var_spec, rc) result(node_id)
      class(ComponentGraph), intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      type(ESMF_State) :: state
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(StateItemNode) :: node
      type(StringVector) :: names
      integer :: i
      character(:), allocatable :: name
      type(VariableSpec) :: member
      type(NodeId) :: child_id

      state = ESMF_StateCreate(_RC)
      call payload%set(state, _RC)

      names = var_spec%get_member_names()
      do i = 1, names%size()
         name = names%of(i)
         member = var_spec%get_member(name, _RC)
         child_id = materialize_member(graph, member, _RC)
         call payload%add_state_member(name, child_id, _RC)
      end do

      node_id = graph%next_node_id(_RC)
      node = StateItemNode(node_id, payload, revision)
      call graph%register_node(node, _RC)

      _RETURN(_SUCCESS)
   end function materialize_composite

   ! One declared member - a leaf (no members of its own) or a nested
   ! composite (has members), determined directly from var_spec%
   ! get_member_names() (design.md Decisions - "a composite declaration
   ! is a VariableSpec... no wrapper type").
   recursive function materialize_member(graph, var_spec, rc) result(node_id)
      class(ComponentGraph), intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(StateItemNode) :: node
      type(StringVector) :: names

      names = var_spec%get_member_names()
      if (names%size() > 0) then
         node_id = materialize_composite(graph, var_spec, _RC)
      else
         ! Unallocated payload/default revision - "advertised, not yet
         ! realized", exactly mirroring advertise_one for a flat item
         ! (GraphBuilder.F90:207-252). The leaf's own remaining fields
         ! are not needed here: realization is out of scope (design.md
         ! Non-Goals).
         node_id = graph%next_node_id(_RC)
         node = StateItemNode(node_id, payload, revision)
         call graph%register_node(node, _RC)
      end if

      _RETURN(_SUCCESS)
   end function materialize_member

end module mapl_CompositeStateMaterialization_mod
