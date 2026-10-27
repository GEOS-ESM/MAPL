#include "MAPL.h"

! Resolution of an export's geometry and vertical grid.

submodule (mapl_GraphBuilder_mod) GraphBuilder_PortsAndProxies_smod
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_GeometryCharacteristic_mod, only: GeometryCharacteristic
   use mapl_VerticalGridCharacteristic_mod, only: VerticalGridCharacteristic
   use mapl_VerticalStaggerCharacteristic_mod, only: VerticalStaggerCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: VERTICAL_STAGGER_CHARACTERISTIC_KIND
   use mapl_CharacteristicStatus_mod, only: CharacteristicStatus, operator(==), CHARACTERISTIC_STATUS_SPECIFIED
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Resolution order: (a) the export's own geometry characteristic, if it
   ! holds a geom (declared on the item, or taken from the component when
   ! its DEFERRED entry was resolved), else (b) the owning component's
   ! default via has_geom()/get_geom(), else (c) not found. No
   ! cross-component mirror propagation. export_item is read-only.
   module subroutine resolve_export_geom(export_item, owner, geom, found, rc)
      type(StateItemDescriptor), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      type(ESMF_Geom), intent(out) :: geom
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom), allocatable :: item_geom

      found = .false.

      call get_item_geom(export_item, item_geom, _RC)
      if (allocated(item_geom)) then
         geom = item_geom
         found = .true.
         _RETURN(_SUCCESS)
      end if

      if (owner%has_geom()) then
         geom = owner%get_geom(_RC)
         found = .true.
      end if

      _RETURN(_SUCCESS)
   end subroutine resolve_export_geom

   ! The geom held by the item's GEOMETRY characteristic, if any. Unallocated
   ! when there is no entry, or the entry is DEFERRED, MIRRORED or holds only
   ! a geom id.
   subroutine get_item_geom(item, geom, rc)
      type(StateItemDescriptor), intent(in) :: item
      type(ESMF_Geom), allocatable, intent(out) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      _RETURN_UNLESS(item%has_characteristic(GEOMETRY_CHARACTERISTIC_KIND))

      call item%get_characteristic(GEOMETRY_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (GeometryCharacteristic)
         if (characteristic%has_geom()) geom = characteristic%get_geom()
      class default
         _FAIL('GraphBuilder: get_item_geom - characteristic is not a GeometryCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_item_geom

   ! The grid held by the item's VERTICAL_GRID characteristic, else the
   ! owning component's. An item with no grid has no vertical dimension (a
   ! legitimate 2D field), so "not found" is not a failure. A SPECIFIED NONE
   ! stagger is honored ahead of the component-wide default so a 2D field in
   ! a 3D-capable component does not get that component's vertical grid.
   ! vert_staggerloc is the item's stagger. A connected export always has
   ! one SPECIFIED (a DEFERRED stagger makes its pair wait, and a MIRRORED
   ! export never settles), so its absence is an internal error.
   module subroutine resolve_export_vgrid(export_item, owner, vgrid, vert_staggerloc, rc)
      type(StateItemDescriptor), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(VerticalGrid), allocatable, intent(out) :: vgrid
      type(VerticalStaggerLoc), intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: rc

      class(VerticalGrid), pointer :: component_vgrid
      class(VerticalGrid), allocatable :: item_vgrid
      logical :: stagger_specified
      integer :: status

      call get_item_vertical_stagger(export_item, vert_staggerloc, stagger_specified, _RC)
      _ASSERT(stagger_specified, 'GraphBuilder: resolve_export_vgrid - export has no SPECIFIED vertical stagger')
      if (vert_staggerloc%is_none()) then
         _RETURN(_SUCCESS)
      end if

      call get_item_vgrid(export_item, item_vgrid, _RC)
      if (allocated(item_vgrid)) then
         vgrid = item_vgrid
         _RETURN(_SUCCESS)
      end if

      ! MIRRORED entries (expressions, mirror_vgrid) hold no grid of their own.
      component_vgrid => owner%get_vertical_grid()
      if (associated(component_vgrid)) vgrid = component_vgrid
      _RETURN(_SUCCESS)
   end subroutine resolve_export_vgrid

   ! The stagger held by the item's VERTICAL_STAGGER characteristic.
   ! specified is false when there is no entry or it is not SPECIFIED
   ! (MIRRORED and DEFERRED hold no value).
   subroutine get_item_vertical_stagger(item, vertical_stagger, specified, rc)
      type(StateItemDescriptor), intent(in) :: item
      type(VerticalStaggerLoc), intent(out) :: vertical_stagger
      logical, intent(out) :: specified
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic
      type(CharacteristicStatus) :: s

      specified = .false.
      _RETURN_UNLESS(item%has_characteristic(VERTICAL_STAGGER_CHARACTERISTIC_KIND))

      call item%get_characteristic(VERTICAL_STAGGER_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (VerticalStaggerCharacteristic)
         s = characteristic%get_status()
         if (s == CHARACTERISTIC_STATUS_SPECIFIED) then
            vertical_stagger = characteristic%get_vertical_stagger()
            specified = .true.
         end if
      class default
         _FAIL('GraphBuilder: get_item_vertical_stagger - characteristic is not a VerticalStaggerCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_item_vertical_stagger

   ! The vertical grid held by the item's VERTICAL_GRID characteristic,
   ! if any. Unallocated when there is no entry, or the entry is MIRRORED or
   ! holds only dimension names.
   subroutine get_item_vgrid(item, vgrid, rc)
      type(StateItemDescriptor), intent(in) :: item
      class(VerticalGrid), allocatable, intent(out) :: vgrid
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      _RETURN_UNLESS(item%has_characteristic(VERTICAL_GRID_CHARACTERISTIC_KIND))

      call item%get_characteristic(VERTICAL_GRID_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (VerticalGridCharacteristic)
         if (characteristic%has_vgrid()) vgrid = characteristic%get_vgrid()
      class default
         _FAIL('GraphBuilder: get_item_vgrid - characteristic is not a VerticalGridCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end subroutine get_item_vgrid

end submodule GraphBuilder_PortsAndProxies_smod
