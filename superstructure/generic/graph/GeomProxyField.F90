#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_GeomProxyField_mod: builds the geometry-proxy GraphStateItem
! (REQ-GEO-001): an incomplete ESMF_Field (ESMF_FIELDSTATUS_GRIDSET: grid
! assigned, no data array allocated) tagged with the MAPL_STATEITEM_GEOM
! variant, wrapped as an ordinary GraphStateItem.
!
! Deliberately does not touch geometry identity (GeomId). Identity for
! connection matching flows from OuterMetaComponent%get_geom_id() into
! GraphBuilder.F90's build_characteristics (as var_spec%vertical_grid%get_id()
! does for VerticalGridCharacteristic), not through this constructor or the
! proxy Field's Info.
!------------------------------------------------------------------------------
module mapl_GeomProxyField_mod
   use ESMF, only: ESMF_Geom, ESMF_Field
   use ESMF, only: ESMF_FieldEmptyCreate, ESMF_FieldEmptySet
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_GEOM
   use mapl_field_api, only: MAPL_FieldSet
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
      call MAPL_FieldSet(field, state_item_variant=MAPL_STATEITEM_GEOM, _RC)
      call item%set(field, _RC)

      _RETURN(_SUCCESS)
   end function new_geom_proxy_item

end module mapl_GeomProxyField_mod
