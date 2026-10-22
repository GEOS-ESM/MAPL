#include "MAPL.h"

!------------------------------------------------------------------------------
! ExtensionMaterialization: real ESMF_Field materialization for a
! framework-created extension StateItemNode. Graph-layer only: depends on
! ComponentGraph/NodeId/StateItemNode/GraphStateItem and the
! StateRegistry-independent mapl_FieldCreate_mod::FieldCreate, never on
! StateRegistry/StateItemSpec/VariableSpec/OuterMetaComponent. Callers
! (GraphBuilder.F90) extract the plain geom/typekind/ungridded_dims/vgrid/
! units values from their own data; this module never reads those types.
!
! Idempotent: if the target StateItemNode's payload already has an
! allocated ESMF_Field (e.g. a reused extension chain),
! materialize_field_extension() is a no-op.
!------------------------------------------------------------------------------
module mapl_ExtensionMaterialization_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem, operator(==)
   use mapl_FieldCreate_mod, only: FieldCreate
   use mapl_UngriddedDims_mod, only: UngriddedDims
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_vertical_grid_api, only: mapl_VerticalGrid
   use ESMF, only: ESMF_Geom, ESMF_TypeKind_Flag, ESMF_Field
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: materialize_field_extension

contains

   ! Materializes node_id's StateItemNode payload as a real, allocated
   ! ESMF_Field via FieldCreate; a no-op if it already has one. Callers must
   ! already have confirmed the materialization gate
   ! (mapl_ExtensionResolution_mod's materialize_extensions_enabled()) is on,
   ! the item is Field-typed, and geom is resolved; this module checks none of
   ! that.
   !
   ! Optional `name` is passed to FieldCreate(name=name, ...) so a materialized
   ! item is findable by short name once inserted into an ESMF_State
   ! (ESMF_StateGet is by name). Omitted by the mismatch-branch call site,
   ! which never inserts its result into a real ESMF_State.
   subroutine materialize_field_extension(graph, node_id, geom, typekind, units, &
        ungridded_dims, vgrid, vert_staggerloc, name, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      class(mapl_VerticalGrid), optional, intent(in) :: vgrid
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem) :: payload
      type(ESMF_Field) :: field
      type(ESMF_StateItem_Flag) :: kind_flag

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'ExtensionMaterialization: NodeId not found in graph')

      select type (node)
      class is (StateItemNode)
         payload = node%get_payload()
         kind_flag = payload%itemType(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELD) then
            ! Already materialized (a reused extension chain) - no-op.
            _RETURN(_SUCCESS)
         end if

         field = FieldCreate(geom=geom, typekind=typekind, ungridded_dims=ungridded_dims, &
              vgrid=vgrid, vert_staggerloc=vert_staggerloc, units=units, name=name, _RC)

         call payload%set(field, _RC)
         call node%set_payload(payload)
      class default
         _FAIL('ExtensionMaterialization: NodeId does not refer to a StateItemNode')
      end select

      _RETURN(_SUCCESS)
   end subroutine materialize_field_extension

end module mapl_ExtensionMaterialization_mod
