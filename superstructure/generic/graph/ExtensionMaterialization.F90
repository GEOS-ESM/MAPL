#include "MAPL.h"

!------------------------------------------------------------------------------
! ExtensionMaterialization: creates the real ESMF_Field of a StateItemNode
! the graph owns (an export or framework-created extension node) from that
! node's own descriptor. Graph-layer only: never depends on StateRegistry,
! VariableSpec or OuterMetaComponent, and names no characteristic.
!
! Idempotent: if the node's payload already has an ESMF_Field (e.g. a reused
! extension chain), materialize_field_extension() is a no-op.
!------------------------------------------------------------------------------
module mapl_ExtensionMaterialization_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
   use mapl_StateItemCharacteristic_mod, only: SYNC_PHASE_STRUCTURAL
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_field_api, only: MAPL_FieldEmptyComplete, MAPL_FieldSet
   use mapl_enums_api, only: MAPL_STATEITEM_ALLOCATION_ALLOCATED
   use ESMF, only: ESMF_Field, ESMF_FieldEmptyCreate
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD, operator(==)
   use mapl_FieldUtilities_mod, only: FieldSet
   use ESMF, only: ESMF_KIND_R4
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: materialize_field_extension

contains

   ! Gives node_id's StateItemNode an allocated ESMF_Field built from its
   ! descriptor: empty-create, write every keyword (sync_to_info STRUCTURAL;
   ! the geom goes on the empty field), then complete from Info. A no-op if
   ! the payload already is a Field. Callers check the materialization gate,
   ! the item class and descriptor completeness; this module does not.
   !
   ! Optional `name` makes the field findable by short name once inserted
   ! into an ESMF_State (ESMF_StateGet is by name).
   subroutine materialize_field_extension(graph, node_id, name, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(StateItemDescriptor), pointer :: descriptor
      type(GraphStateItem) :: payload
      type(ESMF_Field) :: field
      type(ESMF_StateItem_Flag) :: kind_flag
      real(ESMF_KIND_R4) :: fill_value
      logical :: has_fill_value

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'ExtensionMaterialization: NodeId not found in graph')

      select type (node)
      class is (StateItemNode)
         payload = node%get_payload()
         kind_flag = payload%itemType(_RC)
         ! Already materialized (a reused extension chain).
         _RETURN_IF(kind_flag == ESMF_STATEITEM_FIELD)

         descriptor => node%get_descriptor()
         field = ESMF_FieldEmptyCreate(name=name, _RC)
         call payload%set(field, _RC)
         call descriptor%sync_to_info(payload, SYNC_PHASE_STRUCTURAL, _RC)
         call MAPL_FieldEmptyComplete(field, _RC)
         call descriptor%get_fill_value(fill_value, has_fill_value)
         if (has_fill_value) call FieldSet(field, fill_value, _RC)
         call MAPL_FieldSet(field, &
              allocation_status=MAPL_STATEITEM_ALLOCATION_ALLOCATED, _RC)
         call node%set_payload(payload)
      class default
         _FAIL('ExtensionMaterialization: NodeId does not refer to a StateItemNode')
      end select

      _RETURN(_SUCCESS)
   end subroutine materialize_field_extension

end module mapl_ExtensionMaterialization_mod
