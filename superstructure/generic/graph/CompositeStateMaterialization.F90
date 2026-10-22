#include "MAPL.h"

!------------------------------------------------------------------------------
! CompositeStateMaterialization: recursive walk that turns a VariableSpec
! with declared members (specs/VariableSpec.F90) into graph structure: one
! StateItemNode per tree node (leaves and nested-state levels alike), with
! each nested level's GraphStateItem state_members map (REQ-SI-006)
! populated with its declared children's NodeIds. A separate module that
! GraphBuilder calls into, like ExtensionResolution/ExtensionMaterialization.
!
! A "leaf" is a VariableSpec with no declared members; it gets the same
! unallocated "advertised, not yet realized" GraphStateItem payload that
! advertise_one gives flat items. A "nested" member (has declared members)
! gets a real, structurally empty ESMF_State, enough to satisfy
! GraphStateItem's esmf_kind()==STATE gate on state_members; it does not
! realize the state's ESMF membership. The role is read from
! get_member_names(); declare_member already enforces
! itemType==MAPL_STATEITEM_STATE at declaration time.
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
   use mapl_state_api, only: MAPL_StateSetVariant
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: materialize_composite
   public :: materialize_member

contains

   ! Builds a StateItemNode for var_spec's top-level identity, recursing into
   ! every declared member first so the node's GraphStateItem is fully populated
   ! (state_members complete) when registered, with no fetch-and-mutate after
   ! registration.
   recursive function materialize_composite(graph, var_spec, rc) result(node_id)
      class(ComponentGraph), target, intent(inout) :: graph
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
      ! A composite tagged via VariableSpec%state_item_variant (e.g.
      ! MAPL_STATEITEM_VERTICALGRID) gets that variant attached to its
      ! ESMF_State before wrapping; untagged composites keep reporting the plain
      ! STATE variant.
      if (allocated(var_spec%state_item_variant)) then
         call MAPL_StateSetVariant(state, var_spec%state_item_variant, _RC)
      end if
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

   ! One declared member: a leaf (no members) or a nested composite, decided
   ! by var_spec%get_member_names().
   recursive function materialize_member(graph, var_spec, rc) result(node_id)
      class(ComponentGraph), target, intent(inout) :: graph
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
         ! Unallocated payload/default revision: "advertised, not yet realized",
         ! as advertise_one does for a flat item. Realization is out of scope here.
         node_id = graph%next_node_id(_RC)
         node = StateItemNode(node_id, payload, revision)
         call graph%register_node(node, _RC)
      end if

      _RETURN(_SUCCESS)
   end function materialize_member

end module mapl_CompositeStateMaterialization_mod
