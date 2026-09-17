#include "MAPL.h"

!------------------------------------------------------------------------------
! Characteristic: the graph's own name for what legacy calls an "Aspect"
! (docs/graph/spec/09-extension-reuse.md, extension-reuse change design.md
! Decisions - "Characteristic is the graph's own name for what legacy
! calls an Aspect, and is a deliberately independent design, not a
! reskin"). Deliberately independent of both `StateItemAspect`
! (superstructure/generic/specs/StateItemAspect.F90, AspectMap-based,
! built for StateRegistry's coupler-state bookkeeping) and
! docs/graph/spec/18-state-item-characteristics.md's `StateItemCharacteristic`
! (still [SPECULATIVE], scoped to cross-graph mechanics this module does
! not need) - read either only as behavioral reference, never called.
!
! `build_transform` is a deferred method on `Characteristic` itself, not
! a separate provider registry: each concrete Characteristic subclass
! already knows how to build (or refuse to build) a Transform resolving
! a mismatch against a goal of the same kind - mirrors
! `StateItemAspect%make_transform` exactly (ordinary OO dispatch, not
! reinvented as a lookup table). A subclass with no real conversion yet
! (VerticalGridCharacteristic) implements this by failing explicitly
! (rc /= 0), which is what makes "Unregistered characteristic fails
! loudly" (spec) an ordinary deferred-method failure rather than a
! missing-registry-entry check.
!
! `CharacteristicMap` (CharacteristicId -> class(Characteristic)) is a
! real gFTL2 polymorphic map, generated the same way
! superstructure/generic/specs/StateItemAspect.F90 generates `AspectMap`
! for `StateItemAspect` (Key=CharacteristicId, T=Characteristic,
! T_polymorphic) - not a hand-written container.
!------------------------------------------------------------------------------
module mapl_Characteristic_mod
   use iso_fortran_env, only: INT64
   use mapl_CharacteristicId_mod, only: CharacteristicId, operator(<)
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform
   use mapl_ErrorHandling_mod

#define Key CharacteristicId
#define Key_LT(a,b) (a) < (b)
#define T Characteristic
#define T_polymorphic
#define Map CharacteristicMap
#define MapIterator CharacteristicMapIterator
#define Pair CharacteristicPair

#define USE_ALT_SET
#include "map/header.inc"
#include "map/public.inc"

   public :: Characteristic

   type, abstract :: Characteristic
   contains
      procedure(I_get_signature), deferred :: get_signature
      procedure(I_needs_extension_for), deferred :: needs_extension_for
      procedure(I_build_transform), deferred :: build_transform
      procedure(I_get_id), deferred, nopass :: get_id
   end type Characteristic

#include "map/specification.inc"

   abstract interface

      ! Human-readable, value-distinguishing signature (e.g.
      ! "units=m/s") - used to build a resource-index key unique to this
      ! Characteristic's concrete value, not just its kind (see
      ! mapl_ExtensionResolution_mod's reuse-search lookup).
      function I_get_signature(this) result(signature)
         import Characteristic
         class(Characteristic), intent(in) :: this
         character(:), allocatable :: signature
      end function I_get_signature

      ! Analogous to (never calls) StateItemAspect%needs_extension_for:
      ! does an item with this Characteristic need a transform step to
      ! satisfy an import declaring `goal` for the same kind? `goal` is
      ! guaranteed the same dynamic type as `this` by the caller (both
      ! come from the same CharacteristicMap-comparison entry point,
      ! mapl_ExtensionResolution_mod's find_mismatched_characteristics) -
      ! an implementation MAY still defensively `select type`/assert.
      logical function I_needs_extension_for(this, goal) result(needs_extension)
         import Characteristic
         class(Characteristic), intent(in) :: this
         class(Characteristic), intent(in) :: goal
      end function I_needs_extension_for

      ! Analogous to (never calls) StateItemAspect%make_transform: build
      ! a Transform (mapl_Transform_mod) resolving `this` (the export's
      ! value) into `goal` (the import's requirement), wired to read the
      ! bound input_node_id/output_node_id from `graph` at compute()
      ! time (TransformGraphNode%execute() -> Transform%compute() takes
      ! no arguments - a concrete Transform must hold whatever
      ! references it needs itself, set here at construction time). A
      ! subclass with no real conversion implemented yet MUST fail
      ! explicitly (rc /= 0) rather than allocate a no-op/stub Transform
      ! (spec "Unregistered characteristic fails loudly").
      subroutine I_build_transform(this, graph, input_node_id, output_node_id, goal, transformer, rc)
         import Characteristic, ComponentGraph, NodeId, Transform
         class(Characteristic), intent(in) :: this
         class(ComponentGraph), target, intent(in) :: graph
         type(NodeId), intent(in) :: input_node_id
         type(NodeId), intent(in) :: output_node_id
         class(Characteristic), intent(in) :: goal
         class(Transform), allocatable, intent(out) :: transformer
         integer, optional, intent(out) :: rc
      end subroutine I_build_transform

      ! NOPASS (mirrors StateItemAspect%get_aspect_id): which
      ! Characteristic kind this concrete type is - a fixed property of
      ! the type, not the instance, used as the CharacteristicMap key.
      function I_get_id() result(id)
         import CharacteristicId
         type(CharacteristicId) :: id
      end function I_get_id

   end interface

contains

#include "map/procedures.inc"
#include "map/tail.inc"

end module mapl_Characteristic_mod
