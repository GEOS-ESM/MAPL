#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemCharacteristic: abstract per-characteristic model attached to a
! GraphStateItem (docs/graph/spec/18-state-item-characteristics.md
! REQ-CHAR-001/002). Branches into two abstract intermediate kinds
! (REQ-CHAR-002a):
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
! branching on Value-vs-Reference kind") is why the deferred methods
! below (get_kind/needs_extension_for/build_transform/get_signature) are
! declared on this common ancestor, not duplicated per kind.
!
! build_transform / get_signature here absorb an independent role from
! graph/extension-reuse. A concrete subclass with no real provider
! implemented yet (e.g. VerticalGridCharacteristic) reports
! BUILD_TRANSFORM_UNSUPPORTED from build_transform rather than returning a
! stub Transform. That is an expected outcome, not an error: it raises
! nothing, and the caller reports the pair as unsupported. A pair that can
! never connect reports BUILD_TRANSFORM_INCOMPATIBLE instead; it also raises
! nothing, and the caller reports a connection conflict.
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
   use mapl_TransformBase_mod, only: TransformBase
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_CharacteristicDefaults_mod, only: CharacteristicDefaults
   use mapl_ErrorHandling_mod
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
   public :: SYNC_PHASE_STRUCTURAL
   public :: SYNC_PHASE_DESCRIPTIVE
   public :: BUILD_TRANSFORM_UNSUPPORTED
   public :: report_build_transform_unsupported
   public :: BUILD_TRANSFORM_INCOMPATIBLE
   public :: report_build_transform_incompatible

   ! Phases of StateItemDescriptor%sync_to_info. STRUCTURAL: before the field
   ! is completed, every keyword may be written. DESCRIPTIVE: any time, so
   ! structural keywords (and units, a FieldDelta trigger) are skipped.
   integer, parameter :: SYNC_PHASE_STRUCTURAL = 1
   integer, parameter :: SYNC_PHASE_DESCRIPTIVE = 2

   ! rc value of a build_transform that has no Transform for this pair yet.
   ! Distinct from a failure: nothing is raised.
   integer, parameter :: BUILD_TRANSFORM_UNSUPPORTED = 1

   ! rc value of a build_transform for a pair that can never connect, so no
   ! Transform will ever exist for it. Nothing is raised.
   integer, parameter :: BUILD_TRANSFORM_INCOMPATIBLE = 2

   type, abstract :: StateItemCharacteristic
      private
      type(CharacteristicStatus) :: status = CHARACTERISTIC_STATUS_INVALID
   contains
      procedure(I_get_kind), deferred, nopass :: get_kind
      procedure(I_needs_extension_for), deferred :: needs_extension_for
      procedure(I_build_transform), deferred :: build_transform
      procedure(I_get_signature), deferred :: get_signature
      procedure :: get_status => char_get_status
      procedure :: set_status => char_set_status
      procedure :: write_info => char_write_info
      procedure :: is_structural => char_is_structural
      procedure :: is_per_alias => char_is_per_alias
      procedure :: resolve_deferred => char_resolve_deferred
      procedure :: drop_if_unsupplied => char_drop_if_unsupplied
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
      ! production caller, resolve_pair (mapl_StateItemDescriptor_mod),
      ! receives both sides looked up
      ! by the SAME StateItemCharacteristicKind key out of two
      ! StateItemDescriptor characteristics maps, and the only insertion path
      ! into that map, StateItemDescriptor%set_characteristic, asserts
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

      ! merge-characteristic-families: build a Transform (declared here
      ! as its own TransformBase ancestor - mapl_TransformBase_mod - not
      ! mapl_Transform_mod directly, to avoid a circular module
      ! dependency: Transform's own compute() interface reaches
      ! GraphStateItem via PortNameGraphStateItemMap, and GraphStateItem
      ! itself uses this module for its characteristics map; TransformBase
      ! carries none of that chain, only the port-declaration machinery
      ! every real Transform subclass still inherits unchanged - see
      ! TransformBase.F90's own header comment for the full rationale)
      ! resolving `this` (the established value) into `goal` (the goal
      ! value) - a pure value-to-strategy constructor, with no graph/
      ! NodeId concept involved (post-decouple-transform-from-graph). A
      ! subclass with no real conversion implemented yet MUST report
      ! BUILD_TRANSFORM_UNSUPPORTED (through
      ! report_build_transform_unsupported, raising nothing) and leave
      ! `transformer` unallocated, rather than allocate a no-op/stub
      ! Transform. A pair that can never connect MUST report
      ! BUILD_TRANSFORM_INCOMPATIBLE (through
      ! report_build_transform_incompatible), also raising nothing. Any
      ! other nonzero rc is a genuine error.
      ! `goal` is guaranteed the same dynamic type as `this` for the real
      ! call chain this method is used in (same guarantee documented on
      ! needs_extension_for above). Every concrete override still
      ! allocates a real, full Transform subclass (e.g.
      ! UnitsConverterTransform) into this TransformBase-declared
      ! `transformer` - allocating a more-derived type into a less-
      ! derived polymorphic container is always legal; only the dummy
      ! argument's own declared type is TransformBase, not what gets
      ! allocated into it.
      subroutine I_build_transform(this, goal, transformer, rc)
         import StateItemCharacteristic, TransformBase
         class(StateItemCharacteristic), intent(in) :: this
         class(StateItemCharacteristic), intent(in) :: goal
         class(TransformBase), allocatable, intent(out) :: transformer
         integer, optional, intent(out) :: rc
      end subroutine I_build_transform

      ! merge-characteristic-families: a human-readable, value-
      ! distinguishing signature (e.g. "units=m/s") - used by
      ! mapl_ExtensionResolution_mod's reuse-search cache key to
      ! distinguish two mismatches of the same kind but different
      ! values.
      function I_get_signature(this) result(signature)
         import StateItemCharacteristic
         class(StateItemCharacteristic), intent(in) :: this
         character(:), allocatable :: signature
      end function I_get_signature

   end interface

contains

   function char_get_status(this) result(status)
      class(StateItemCharacteristic), intent(in) :: this
      type(CharacteristicStatus) :: status

      status = this%status
   end function char_get_status

   ! The one way a build_transform reports "no Transform for this pair yet".
   ! rc must be present: an absent rc would swallow the outcome.
   subroutine report_build_transform_unsupported(rc)
      integer, optional, intent(out) :: rc

      _ASSERT(present(rc), 'build_transform reported unsupported but the caller passed no rc')
      rc = BUILD_TRANSFORM_UNSUPPORTED
   end subroutine report_build_transform_unsupported

   ! The one way a build_transform reports "this pair can never connect".
   ! rc must be present: an absent rc would swallow the outcome.
   subroutine report_build_transform_incompatible(rc)
      integer, optional, intent(out) :: rc

      _ASSERT(present(rc), 'build_transform reported incompatible but the caller passed no rc')
      rc = BUILD_TRANSFORM_INCOMPATIBLE
   end subroutine report_build_transform_incompatible

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

   ! Writes this characteristic's own Info keywords onto the handle through
   ! MAPL_FieldSet/MAPL_FieldBundleSet. Called only for SPECIFIED or
   ! UNCHECKED entries. Kinds that have not been given a write method fail.
   subroutine char_write_info(this, handle, phase, rc)
      class(StateItemCharacteristic), intent(in) :: this
      type(GraphStateItem), intent(inout) :: handle
      integer, intent(in) :: phase
      integer, optional, intent(out) :: rc

      type(StateItemCharacteristicKind) :: kind

      kind = this%get_kind()
      _FAIL('StateItemCharacteristic: write_info not implemented for ' // kind%to_string())

      _UNUSED_DUMMY(handle)
      _UNUSED_DUMMY(phase)
   end subroutine char_write_info

   ! A structural characteristic is a creation input of the payload and is
   ! never written in the DESCRIPTIVE phase.
   logical function char_is_structural(this) result(structural)
      class(StateItemCharacteristic), intent(in) :: this

      structural = .false.
      _UNUSED_DUMMY(this)
   end function char_is_structural

   ! A per-alias characteristic is stored under the item's own alias, not
   ! field-wide, so an import that aliases another item's field can write it
   ! without touching the exporter's value.
   logical function char_is_per_alias(this) result(per_alias)
      class(StateItemCharacteristic), intent(in) :: this

      per_alias = .false.
      _UNUSED_DUMMY(this)
   end function char_is_per_alias

   ! Takes the component-supplied value for a DEFERRED characteristic.
   ! Kinds with no component-supplied value are never DEFERRED, so the
   ! default does nothing.
   subroutine char_resolve_deferred(this, defaults, rc)
      class(StateItemCharacteristic), intent(inout) :: this
      type(CharacteristicDefaults), intent(in) :: defaults
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(defaults)
      _RETURN(_SUCCESS)
   end subroutine char_resolve_deferred

   ! True when this DEFERRED characteristic does not apply to the item because
   ! the component supplied no value for it. Most kinds stay DEFERRED instead.
   logical function char_drop_if_unsupplied(this, defaults) result(drop)
      class(StateItemCharacteristic), intent(in) :: this
      type(CharacteristicDefaults), intent(in) :: defaults

      drop = .false.
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(defaults)
   end function char_drop_if_unsupplied

   subroutine refchar_set_referenced_node_id(this, node_id)
      class(ReferenceCharacteristic), intent(inout) :: this
      type(NodeId), intent(in) :: node_id

      this%referenced_node_id = node_id
   end subroutine refchar_set_referenced_node_id

#include "map/procedures.inc"
#include "map/tail.inc"

end module mapl_StateItemCharacteristic_mod
