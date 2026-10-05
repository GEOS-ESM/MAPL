#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_GeomProxyField_mod: builds the geometry-proxy GraphStateItem
! (docs/graph/spec/13-geometry-and-vertical-grids.md REQ-GEO-001/003) -
! an incomplete ESMF_Field (ESMF_FIELDSTATUS_GRIDSET: grid assigned, no
! data array allocated) tagged with the MAPL_STATEITEM_GEOM variant,
! wrapped as an ordinary GraphStateItem.
!
! Deliberately does not touch geometry identity (GeomId) at all: unlike
! legacy GeomAspect, which reads/writes an ESMF_Info-attached GeomId
! directly on the geom object, this module's only job is wrapping the
! Field. Geometry identity for graph connection-matching purposes flows
! separately, from OuterMetaComponent%get_geom_id() straight into
! GraphBuilder.F90's build_characteristics (mirroring exactly how
! var_spec%vertical_grid%get_id() already flows into
! VerticalGridCharacteristic there) - not through this constructor and
! not through the proxy Field's own Info
! (openspec/changes/horizontal-geometry-graph-state-item, design.md
! Decision D1).
!------------------------------------------------------------------------------
module mapl_GeomProxyField_mod
   use ESMF, only: ESMF_Geom, ESMF_Field
   use ESMF, only: ESMF_FieldEmptyCreate, ESMF_FieldEmptySet
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_GEOM
   use mapl_StateItemVariantInfo_mod, only: set_variant
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: new_geom_proxy_item

contains

   function new_geom_proxy_item(geom, rc) result(item)
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc
      type(GraphStateItem) :: item

      integer :: status
      type(ESMF_Field) :: field

      field = ESMF_FieldEmptyCreate(_RC)
      call ESMF_FieldEmptySet(field, geom=geom, _RC)
      call set_variant(field, MAPL_STATEITEM_GEOM, _RC)
      call item%set(field, _RC)

      _RETURN(_SUCCESS)
   end function new_geom_proxy_item

end module mapl_GeomProxyField_mod
