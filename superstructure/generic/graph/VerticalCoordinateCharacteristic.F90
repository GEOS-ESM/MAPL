#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalCoordinateCharacteristic: the "vertical grid" StateItemCharacteristic
! (openspec/changes/state-item-characteristic-subclasses, design.md D1) - a
! ReferenceCharacteristic, same shape as GeometryCharacteristic.F90: holds a
! NodeId referencing a shared graph node, not a value of its own.
!
! Named VerticalCoordinateCharacteristic, NOT VerticalGridCharacteristic -
! design.md D1: an exact, unavoidable collision with the existing, unrelated
! graph/extension-reuse VerticalGridCharacteristic type (Fortran module/type
! names share one global namespace). Same collision-avoidance discipline
! already applied for GeometryCharacteristic vs. legacy GeomCharacteristic.
!
! needs_extension_for compares the referenced NodeId for equality - sharing
! is nothing more than two map entries holding the same NodeId (REQ-CHAR-012).
! No build_transform/adaptation method exists at this layer (REQ-CHAR-002);
! the richer dimension-overlap comparison graph/extension-reuse's own
! VerticalGridCharacteristic implements is that deliberately independent
! family's job, not this one's (design.md D1).
!------------------------------------------------------------------------------
module mapl_VerticalCoordinateCharacteristic_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, ReferenceCharacteristic
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, VERTICAL_COORDINATE_CHARACTERISTIC_KIND
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(/=)
   implicit none(type, external)
   private

   public :: VerticalCoordinateCharacteristic

   type, extends(ReferenceCharacteristic) :: VerticalCoordinateCharacteristic
   contains
      procedure, nopass :: get_kind => vcoord_get_kind
      procedure :: needs_extension_for => vcoord_needs_extension_for
   end type VerticalCoordinateCharacteristic

   interface VerticalCoordinateCharacteristic
      module procedure new_VerticalCoordinateCharacteristic
   end interface VerticalCoordinateCharacteristic

contains

   ! referenced_node_id: the NodeId of the ordinary graph node this
   ! characteristic points at - set via the inherited
   ! ReferenceCharacteristic%set_referenced_node_id accessor, not a bare
   ! field assignment (that component is private to
   ! mapl_StateItemCharacteristic_mod).
   function new_VerticalCoordinateCharacteristic(referenced_node_id) result(characteristic)
      type(NodeId), intent(in) :: referenced_node_id
      type(VerticalCoordinateCharacteristic) :: characteristic

      call characteristic%set_referenced_node_id(referenced_node_id)
   end function new_VerticalCoordinateCharacteristic

   function vcoord_get_kind() result(kind)
      type(StateItemCharacteristicKind) :: kind

      kind = VERTICAL_COORDINATE_CHARACTERISTIC_KIND
   end function vcoord_get_kind

   ! `goal` is guaranteed a VerticalCoordinateCharacteristic for the real
   ! call chain this method is used in (same guarantee documented on
   ! GeometryCharacteristic/PhysicalUnitsCharacteristic: GraphStateItem%
   ! set_characteristic asserts kind match before inserting). The class
   ! default branch below should be checked by the calling procedure.
   logical function vcoord_needs_extension_for(this, goal) result(needs_extension)
      class(VerticalCoordinateCharacteristic), intent(in) :: this
      class(StateItemCharacteristic), intent(in) :: goal

      select type (goal)
      class is (VerticalCoordinateCharacteristic)
         needs_extension = (this%get_referenced_node_id() /= goal%get_referenced_node_id())
      class default
         error stop 'VerticalCoordinateCharacteristic: needs_extension_for called with a non-VerticalCoordinateCharacteristic goal - should be checked by calling procedure'
      end select
   end function vcoord_needs_extension_for

end module mapl_VerticalCoordinateCharacteristic_mod
