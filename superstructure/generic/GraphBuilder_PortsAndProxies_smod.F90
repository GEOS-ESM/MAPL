#include "MAPL.h"

! Resolution of an export's geometry and vertical grid.

submodule (mapl_GraphBuilder_mod) GraphBuilder_PortsAndProxies_smod
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_NONE, operator(==)
   use mapl_GeometryCharacteristic_mod, only: GeometryCharacteristic
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

   ! Same priority order as resolve_export_geom(), but "not found" means
   ! "no vertical dimension" (a legitimate 2D field), not a failure.
   ! An explicit vertical_stagger == VERTICAL_STAGGER_NONE (from
   ! `vertical_dim_spec: NONE`) is honored ahead of the component-wide
   ! default so a 2D field in a 3D-capable component does not get that
   ! component's vertical grid.
   module subroutine resolve_export_vgrid(export_item, owner, vgrid)
      type(StateItemDescriptor), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(VerticalGrid), allocatable, intent(out) :: vgrid

      class(VerticalGrid), pointer :: component_vgrid
      type(VerticalStaggerLoc), allocatable :: declared_vertical_stagger
      class(VerticalGrid), allocatable :: declared_vertical_grid

      call export_item%get_vertical_stagger(declared_vertical_stagger)
      if (allocated(declared_vertical_stagger)) then
         if (declared_vertical_stagger == VERTICAL_STAGGER_NONE) return
      end if

      call export_item%get_declared_vertical_grid(declared_vertical_grid)
      if (allocated(declared_vertical_grid)) then
         vgrid = declared_vertical_grid
         return
      end if

      component_vgrid => owner%get_vertical_grid()
      if (associated(component_vgrid)) vgrid = component_vgrid
   end subroutine resolve_export_vgrid

end submodule GraphBuilder_PortsAndProxies_smod
