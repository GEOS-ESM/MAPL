!------------------------------------------------------------------------------
! StateItemCharacteristic: abstract per-characteristic model attached to a
! GraphStateItem (docs/graph/spec/18-state-item-characteristics.md
! REQ-CHAR-001/002, openspec/changes/state-item-characteristics). Branches
! into two abstract intermediate kinds (REQ-CHAR-002a):
!
!   - ValueCharacteristic: holds its value inline, no identity beyond its
!     owning GraphStateItem, never shared (PhysicalUnitsCharacteristic.F90,
!     TypeKindCharacteristic.F90).
!   - ReferenceCharacteristic: holds a NodeId referencing a shared graph
!     node elsewhere, rather than a value of its own
!     (GeometryCharacteristic.F90). REQ-CHAR-002b: the referenced node is
!     an ordinary graph node with its own real NodeId/NodeRevision
!     identity - this type only holds the reference, never the payload.
!
! REQ-CHAR-009's detection-completeness requirement ("iterate every entry
! through the common StateItemCharacteristic interface only, never
! branching on Value-vs-Reference kind") is why the only two deferred
! methods below (get_kind/needs_extension_for) are declared on this
! common ancestor, not duplicated per kind.
!
! Deliberately independent of graph/extension-reuse's own
! mapl_Characteristic_mod (Characteristic.F90) - see that module's own
! header comment and openspec/changes/state-item-characteristics
! design.md Decision D6. The two hierarchies share a similar shape
! (abstract base, nopass get_kind()/get_id(), needs_extension_for()) by
! deliberate convergent design, not by reuse - neither module uses the
! other.
!
! status (CharacteristicStatus, CharacteristicStatus.F90) and, for
! ReferenceCharacteristic, the referenced NodeId are exposed only through
! ordinary type-bound get_*/set_* accessors (same "mutation through
! accessor methods, not arbitrary field access" discipline as
! NodeRevision/StateItemNode) so a concrete subclass defined in a
! different module (PhysicalUnitsCharacteristic.F90,
! TypeKindCharacteristic.F90, GeometryCharacteristic.F90) can still
! establish/update these private components without this module exposing
! them directly.
!
! StateItemCharacteristicMap (StateItemCharacteristicKind ->
! class(StateItemCharacteristic), REQ-CHAR-007's key/value types) is a
! real gFTL2 polymorphic map, generated the same way
! mapl_Characteristic_mod generates CharacteristicMap for the
! deliberately-independent graph/extension-reuse hierarchy - instantiated
! here, alongside the type it maps, rather than in a separate
! containers/ file, mirroring that module's own precedent exactly.
!------------------------------------------------------------------------------
module mapl_StateItemCharacteristic_mod
   use iso_fortran_env, only: INT64
   use mapl_CharacteristicStatus_mod, only: CharacteristicStatus, CHARACTERISTIC_STATUS_INVALID
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, operator(<)
   use mapl_NodeId_mod, only: NodeId
   ! No explicit `implicit none` here - map/header.inc (below) already
   ! supplies one; mapl_Characteristic_mod/mapl_StateItemAspect_mod (the
   ! same gFTL2 map-instantiation pattern) follow the same convention.

#define Key StateItemCharacteristicKind
#define Key_LT(a,b) (a) < (b)
#define T StateItemCharacteristic
#define T_polymorphic
#define Map StateItemCharacteristicMap
#define MapIterator StateItemCharacteristicMapIterator
#define Pair StateItemCharacteristicPair

#define USE_ALT_SET
#include "map/header.inc"
#include "map/public.inc"

   public :: StateItemCharacteristic
   public :: ValueCharacteristic
   public :: ReferenceCharacteristic

   type, abstract :: StateItemCharacteristic
      private
      type(CharacteristicStatus) :: status = CHARACTERISTIC_STATUS_INVALID
   contains
      procedure(I_get_kind), deferred, nopass :: get_kind
      procedure(I_needs_extension_for), deferred :: needs_extension_for
      procedure :: get_status => char_get_status
      procedure :: set_status => char_set_status
   end type StateItemCharacteristic

   ! REQ-CHAR-002a: a pure subtype-shape split - no additional deferred
   ! methods or components at this tier. ValueCharacteristic never holds
   ! a NodeId; a concrete subclass (PhysicalUnitsCharacteristic,
   ! TypeKindCharacteristic) holds its own value inline directly.
   type, abstract, extends(StateItemCharacteristic) :: ValueCharacteristic
   end type ValueCharacteristic

   ! REQ-CHAR-002b: holds a reference (NodeId), never a value of its own.
   type, abstract, extends(StateItemCharacteristic) :: ReferenceCharacteristic
      private
      type(NodeId) :: referenced_node_id
   contains
      procedure :: get_referenced_node_id => refchar_get_referenced_node_id
      procedure :: set_referenced_node_id => refchar_set_referenced_node_id
   end type ReferenceCharacteristic

#include "map/specification.inc"

   abstract interface

      ! NOPASS (mirrors mapl_Characteristic_mod's own get_id() -
      ! deliberately convergent shape, not shared code): which concrete
      ! StateItemCharacteristic kind this type is - a fixed property of
      ! the type, not the instance, used as GraphStateItem's
      ! characteristics-map key (graph/state-item).
      function I_get_kind() result(kind)
         import StateItemCharacteristicKind
         type(StateItemCharacteristicKind) :: kind
      end function I_get_kind

      ! REQ-CHAR-009: a plain value/identity comparison only - no
      ! Transform-building logic at this layer (that is
      ! graph/extension-reuse's existing, separate concern; this
      ! hierarchy's job per REQ-CHAR-002 is description, not adaptation).
      ! `goal` is guaranteed the same dynamic type as `this`: the one
      ! production caller, find_mismatched_state_item_characteristics
      ! (mapl_StateItemCharacteristicDetection_mod), looks both sides up
      ! by the SAME StateItemCharacteristicKind key out of two
      ! GraphStateItem.characteristics maps, and the only insertion path
      ! into that map, GraphStateItem%set_characteristic, asserts
      ! `characteristic%get_kind() == kind` before inserting
      ! (REQ-CHAR-006/009, design.md D9) - so any entry retrievable at a
      ! given key is provably that key's own concrete type, for the
      ! actual call chain this method is used in, not merely by
      ! convention. A concrete implementation's `class default` branch
      ! (reached only if some future caller invoked this method directly,
      ! outside that call chain) MAY therefore use `error stop`
      ! (mirrors DependencyNetwork.F90's/ActualConnectionPt.F90's own
      ! precedent: "should be checked by calling procedure") rather than
      ! a catchable `rc`-based failure.
      logical function I_needs_extension_for(this, goal) result(needs_extension)
         import StateItemCharacteristic
         class(StateItemCharacteristic), intent(in) :: this
         class(StateItemCharacteristic), intent(in) :: goal
      end function I_needs_extension_for

   end interface

contains

   function char_get_status(this) result(status)
      class(StateItemCharacteristic), intent(in) :: this
      type(CharacteristicStatus) :: status

      status = this%status
   end function char_get_status

   subroutine char_set_status(this, status)
      class(StateItemCharacteristic), intent(inout) :: this
      type(CharacteristicStatus), intent(in) :: status

      this%status = status
   end subroutine char_set_status

   function refchar_get_referenced_node_id(this) result(node_id)
      class(ReferenceCharacteristic), intent(in) :: this
      type(NodeId) :: node_id

      node_id = this%referenced_node_id
   end function refchar_get_referenced_node_id

   subroutine refchar_set_referenced_node_id(this, node_id)
      class(ReferenceCharacteristic), intent(inout) :: this
      type(NodeId), intent(in) :: node_id

      this%referenced_node_id = node_id
   end subroutine refchar_set_referenced_node_id

#include "map/procedures.inc"
#include "map/tail.inc"

end module mapl_StateItemCharacteristic_mod
