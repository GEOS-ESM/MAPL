#include "MAPL.h"

!------------------------------------------------------------------------------
! GeometryCharacteristic: the "geometry" StateItemCharacteristic
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-001 table) -
! a ReferenceCharacteristic (REQ-CHAR-002a/002b: holds a NodeId
! referencing a shared graph node, not a value of its own).
!
! Named GeometryCharacteristic, NOT GeomCharacteristic (the spec table's
! own suggested name) - design.md D3: an exact, unavoidable collision
! with the existing, unrelated mapl_GeomCharacteristic_mod/
! GeomCharacteristic type already shipped for graph/extension-reuse
! (Fortran module/type names share one global namespace). Same
! collision-avoidance discipline already applied once in this codebase
! for UnitsConverterTransform vs. legacy ConvertUnitsTransform.
!
! needs_extension_for compares the referenced NodeId for equality - two
! GeometryCharacteristics referencing the same underlying node are a
! match (REQ-CHAR-012: sharing is nothing more than two map entries
! holding the same NodeId). No build_transform/adaptation method exists
! at this layer (REQ-CHAR-002); a real horizontal-regrid provider is
! graph/extension-reuse's existing, separate concern (its own
! GeomCharacteristic, not this type).
!------------------------------------------------------------------------------
module mapl_GeometryCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, GEOMETRY_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   implicit none(type, external)
   private

   public :: GeometryCharacteristic

   type, extends(ReferenceCharacteristic) :: GeometryCharacteristic
   contains
      procedure, nopass :: get_kind => geometry_get_kind
      procedure :: needs_extension_for => geometry_needs_extension_for
   end type GeometryCharacteristic

   interface GeometryCharacteristic
      module procedure new_GeometryCharacteristic
   end interface GeometryCharacteristic

contains

   ! referenced_node_id: the NodeId of the ordinary graph node (an
   ! esmf_field-kind StateItemNode in the GRIDSET-only geometry-proxy
   ! role, per 04-graph-value-hierarchy.md §4.6.4/REQ-CHAR-002b) this
   ! characteristic points at - set via the inherited
   ! ReferenceCharacteristic%set_referenced_node_id accessor, not a bare
   ! field assignment (that component is private to
   ! mapl_StateItemCharacteristic_mod).
   function new_GeometryCharacteristic(referenced_node_id) result(characteristic)
      type(NodeId), intent(in) :: referenced_node_id
      type(GeometryCharacteristic) :: characteristic

      call characteristic%set_referenced_node_id(referenced_node_id)
   end function new_GeometryCharacteristic

   function geometry_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = GEOMETRY_CHARACTERISTIC_KIND
   end function geometry_get_kind

   ! `goal` is guaranteed a GeometryCharacteristic for the real call
   ! chain this method is used in: the one production caller,
   ! find_mismatched_state_item_characteristics, looks both sides up by
   ! the same StateItemCharacteristicKind key, and
   ! GraphStateItem%set_characteristic (the only insertion path into that
   ! map) asserts a characteristic's own kind matches its map key before
   ! inserting (REQ-CHAR-006/009, design.md D9). A geometry
   ! characteristic can never be "extended" into any other characteristic
   ! kind (there is no such Transform, nor could there be), so the class
   ! default branch below should be checked by the calling procedure,
   ! matching this codebase's own established `error stop` precedent for
   ! exactly that situation (DependencyNetwork.F90, ActualConnectionPt.F90).
   logical function geometry_needs_extension_for(this, goal) result(needs_extension)
      class(GeometryCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (GeometryCharacteristic)
         needs_extension = (this%get_referenced_node_id() /= goal%get_referenced_node_id())
      class default
         error stop 'GeometryCharacteristic: needs_extension_for called with a non-GeometryCharacteristic goal - should be checked by calling procedure'
      end select
   end function geometry_needs_extension_for

end module mapl_GeometryCharacteristic_mod
