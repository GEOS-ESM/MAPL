## 1. Module scaffolding

- [x] 1.1 Create new files under `superstructure/generic/graph/`:
      `CharacteristicStatus.F90`, `StateItemCharacteristicKind.F90`,
      `StateItemCharacteristic.F90` (abstract base +
      `ValueCharacteristic`/`ReferenceCharacteristic` abstract
      intermediates), `PhysicalUnitsCharacteristic.F90`,
      `TypeKindCharacteristic.F90`, `GeometryCharacteristic.F90`,
      `SetSharedCharacteristic.F90` (design.md D7's mutator).
- [x] 1.2 Add the new source files to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.

## 2. CharacteristicStatus and StateItemCharacteristicKind

- [x] 2.1 Implement `mapl_CharacteristicStatus_mod`: enumeration type
      with `INVALID`/`SPECIFIED`/`MIRRORED`/`UNCHECKED`/`DEFERRED` named
      parameter values, `==`/`/=`, `to_string()` (design.md D1).
- [x] 2.2 Implement `mapl_StateItemCharacteristicKind_mod`, mirroring
      `CharacteristicId.F90`'s own shape exactly (wrapped integer, named
      parameter constants per registered concrete subclass -
      `PHYSICAL_UNITS_CHARACTERISTIC_KIND`/`TYPE_KIND_CHARACTERISTIC_KIND`/
      `GEOMETRY_CHARACTERISTIC_KIND`/`INVALID_CHARACTERISTIC_KIND`, a
      test-only mock constant, `==`/`/=`/`<`, `to_string()`) - deliberately
      independent of `CharacteristicId.F90` (design.md D2), no shared
      module dependency beyond both existing in the same directory.

## 3. StateItemCharacteristic hierarchy

- [x] 3.1 Implement `mapl_StateItemCharacteristic_mod`: abstract
      `StateItemCharacteristic` type with a `status`/`set_status` pair
      (backed by `CharacteristicStatus`), a nopass deferred `get_kind()`
      returning `StateItemCharacteristicKind`, a deferred
      `needs_extension_for(this, goal)` (mirrors
      `mapl_Characteristic_mod`'s own interface shape, independently
      declared - design.md D6, not reused), and abstract
      `ValueCharacteristic`/`ReferenceCharacteristic` extensions of it
      with no additional deferred methods yet (REQ-CHAR-002a is a pure
      subtype-shape requirement at this tier).
- [x] 3.2 Implement `ReferenceCharacteristic`'s `NodeId`-holding
      component and accessor (REQ-CHAR-002b); no concrete reference
      subclass extends `ReferenceCharacteristic` directly - concrete
      subclasses extend it (task 3.3's `GeometryCharacteristic`).
- [x] 3.3 Implement `PhysicalUnitsCharacteristic` and
      `TypeKindCharacteristic` (both extend `ValueCharacteristic`; hold
      their value inline - a units string and a type/kind tag,
      respectively) and `GeometryCharacteristic` (extends
      `ReferenceCharacteristic`; holds the referenced node's `NodeId`).
      Each implements `get_kind()`/`needs_extension_for()`;
      `needs_extension_for` is a plain value/identity comparison, no
      Transform-building logic at this layer (that is
      `graph/extension-reuse`'s existing, separate concern per
      design.md D6 - this hierarchy's job per REQ-CHAR-002 is
      description, not adaptation).

## 4. GraphStateItem.characteristics map

- [x] 4.1 Add `characteristics` component to `GraphStateItem`
      (`GraphStateItem.F90`): a map from `StateItemCharacteristicKind` to
      `StateItemCharacteristic` (gFTL2 polymorphic map, same
      `map/header.inc`+`public.inc`+`specification.inc`+`procedures.inc`+
      `tail.inc` instantiation pattern `mapl_Characteristic_mod` already
      uses for `CharacteristicMap`).
- [x] 4.2 Add accessor/mutator methods on `GraphStateItem`:
      `get_characteristic(kind)` (fails explicitly if absent - design.md
      D4's absent-key-is-canonical decision), `has_characteristic(kind)`,
      `set_characteristic(kind, characteristic)` (inserts or replaces the
      entry, never removes an existing key as a side effect of any other
      operation).
- [x] 4.3 Add `ordering(mismatched_kinds)` method to `GraphStateItem`
      returning the chaining order for a given set of mismatched kinds,
      delegating to the static per-variant table (task 4.4).
- [x] 4.4 Implement the static per-kind/variant ordering table
      (design.md D5): a module-level table keyed by `GraphStateItem`'s
      existing `variant()` classification
      (`mapl_StateItemVariantInfo_mod`), each entry an ordered list of
      `StateItemCharacteristicKind` values; a variant with no table entry
      falls back to the input list's own order (stable, non-crashing
      default per design.md D5).
- [x] 4.5 **Found in review (design.md D9):** `set_characteristic` now
      asserts `characteristic%get_kind() == kind` before inserting,
      failing explicitly rather than silently storing a mismatched
      pairing (REQ-CHAR-006/009's own same-key-implies-same-type
      assumption, otherwise unenforced). Each concrete
      `needs_extension_for`'s `class default` branch changed from
      `needs_extension = .true.` to `error stop` (defense in depth,
      matching `DependencyNetwork.F90`'s own precedent) - with 4.5's
      guard in place, that branch is reachable only by calling
      `needs_extension_for` directly, bypassing `GraphStateItem`. Added
      pFUnit case `test_set_characteristic_rejects_kind_mismatch`.

## 5. Detection algorithm (standalone, not wired into GraphBuilder)

- [x] 5.1 Implement
      `find_mismatched_state_item_characteristics(item_a, item_b)
      result(list of StateItemCharacteristicKind)`: iterates both items'
      `characteristics` maps through the common `StateItemCharacteristic`
      interface only (REQ-CHAR-009 - no branching on Value-vs-Reference
      kind), comparing entries present in both via
      `needs_extension_for`. A kind present in only one item's map is
      reported as mismatched (nothing to compare against). This is a new,
      standalone function - it does not call into or modify
      `GraphBuilder.F90`/`ExtensionResolution.F90` (design.md D6).
- [x] 5.2 Confirm (via task 7's tests) that calling `ordering()` (task
      4.3) on the result of 5.1 produces a sequence usable by a caller
      building an extension chain, without this change itself building
      that chain end-to-end (no real `TransformGraphNode` wiring is
      exercised here - `graph/extension-reuse` already owns that for its
      own, separate `Characteristic` family).

## 6. Sharing and the eager-structural/lazy-content mutator

- [x] 6.1 Confirm (via task 7's tests) that two `GraphStateItem`s' maps
      can each hold a `GeometryCharacteristic` referencing the same
      `NodeId`, with no new identity mechanism (REQ-CHAR-012/014) - this
      requires no new code beyond task 3.3, only a test demonstrating it.
- [x] 6.2 Implement `mapl_SetSharedCharacteristic_mod`'s
      `set_shared_characteristic(graph, node_id, new_value, rc)` (design.md
      D7): updates the referenced node's own `GraphStateItem` characteristic
      value; walks `graph`'s default `DependencyNetwork%get_successors`
      from `node_id`, filtering to `StateItemNode` successors (skip
      `TransformGraphNode` successors - REQ-CHAR-016's "left to the lazy
      path"); for each, performs a pure structural reset (deallocate/
      reset the ESMF payload's shape-bearing component without touching
      a `MethodGraphNode`) and resets that dependent's own revision to
      the invalid sentinel via its existing `set_revision(NodeRevision())`
      setter (design.md D7's mechanism note - a fresh default-constructed
      `NodeRevision` is already invalid by construction; no new
      `NodeRevision`/`StateItemNode` API is added); then advances
      `node_id`'s own revision via the existing `advance_revision()`
      (forward, never reset) last.
- [x] 6.3 Confirm `set_shared_characteristic` never looks up or invokes a
      `MethodGraphNode` anywhere in its call graph (REQ-CHAR-017) - code
      review checklist item, verified by task 7.5's test asserting no
      invocation side effect occurs.

## 7. Unit tests (synthetic nodes only, per Phase 1-2 exit-criterion precedent)

- [x] 7.1 Add pFUnit cases for `StateItemCharacteristicKind` (stable,
      distinct values per constant, `to_string()`) and
      `CharacteristicStatus` (default/invalid construction, each named
      value distinct, `to_string()`).
- [x] 7.2 Add pFUnit cases for the Value/Reference split: a
      `PhysicalUnitsCharacteristic`/`TypeKindCharacteristic` instance is a
      `ValueCharacteristic`; a `GeometryCharacteristic` instance is a
      `ReferenceCharacteristic` and holds a real `NodeId` (spec
      "A geometry characteristic is a reference characteristic").
- [x] 7.3 Add pFUnit cases for `GraphStateItem.characteristics`: starts
      empty; `set_characteristic`/`get_characteristic`/
      `has_characteristic` round-trip; absent key vs. present-with-
      `INVALID` are distinguishable (spec "Established characteristic is
      retrievable by its type tag", design.md D4).
- [x] 7.4 Add pFUnit cases for detection (task 5.1): identical
      characteristics on both items report no mismatch; a units-only
      difference and a units+type-kind difference both report the
      correct mismatched-kind set, with no branching behavior visible
      between value and reference entries in the same comparison (spec
      "Detection does not special-case kind" - use a
      `GeometryCharacteristic` mismatch alongside a
      `PhysicalUnitsCharacteristic` mismatch in one case to exercise
      both kinds together).
- [x] 7.5 Add pFUnit cases for ordering (task 4.3/4.4): two different
      `GraphStateItem` variants, each with a table entry, given the same
      two mismatched kinds, return each variant's own order (spec "Two
      different kinds may order the same pair of characteristics
      differently"); a variant with no table entry falls back to input
      order without failing.
- [x] 7.6 Add pFUnit cases for sharing (task 6.1): two `GraphStateItem`s
      each holding a `GeometryCharacteristic` referencing the same
      `NodeId` both resolve to that `NodeId` (spec "Two state items share
      one geometry reference").
- [x] 7.7 Add pFUnit cases for `set_shared_characteristic` (task 6.2):
      given a synthetic `ComponentGraph` with one shared geometry node and
      two `StateItemNode` successors, calling it resets both successors'
      status to `INVALID` and their revisions to invalid, advances the
      shared node's own revision, and performs no execution of any
      `TransformGraphNode`/`MethodGraphNode` reachable from the walk
      (spec "Dependents are reset within the same call", "Mutator never
      invokes a method node").
- [x] 7.8 Add a pFUnit case confirming content-side laziness (spec
      "Content is not recomputed by the mutator call itself" /
      "Next demand-driven request produces correct content"): after
      `set_shared_characteristic` resets a dependent, no producing
      Transform has executed yet; a subsequent demand-driven `update()`
      request on that dependent executes exactly the needed chain and
      returns correct content.
- [x] 7.9 **Correction (design.md Context/D8):** REQ-REV-011's
      pre-invocation pull is already shipped
      (`MethodInvocation.F90`'s `invoke_on_default_network`, Phase 4b) and
      already covered by `Test_MethodInvocation.pf` - no new isolated
      pFUnit case for "stale bound argument is refreshed before
      invocation" / "up-to-date bound argument triggers no extra work" is
      added here; that coverage already exists. This task is a checklist
      confirmation only: read `Test_MethodInvocation.pf` and confirm both
      scenarios are already exercised there.
- [x] 7.10 Add one new pFUnit case (in this change's own mutator test
      file, alongside 7.7/7.8) chaining 6.2's mutator with the *existing*
      `invoke_on_default_network`: build a synthetic `ComponentGraph` with
      a shared geometry node, a dependent `StateItemNode` bound as a
      `MethodGraphNode`'s IN argument, and a synthetic
      `MethodInvocationAdapter` test double; call
      `set_shared_characteristic` (mid-"Run" structural reset), then call
      `invoke_on_default_network` on the method node; confirm the
      already-shipped pre-invocation pull recognizes the staleness
      introduced by the new mutator and resolves it before the adapter's
      `invoke()` is called, with no special-cased interaction coded
      between the two (spec "Pre-invocation pull reflects a prior mid-Run
      structural reset") - this is the one piece of real new test code in
      this task group; no production code in `MethodInvocation.F90`/
      `MethodInvocationAdapter.F90`/its concrete subtypes is touched.

## 8. Build and verification

- [x] 8.1 Build MAPL with the NAG compiler (`module load nag-stack`
      before any `cmake`/`make`/`ctest` invocation, per project
      convention) and confirm a clean build with the new source files
      included.
- [x] 8.2 Run the new pFUnit suites (task group 7) directly with `-v` and
      confirm all pass.
- [x] 8.3 Run the full `MAPL.generic.*` `ctest` label set and confirm no
      regressions relative to the pre-change baseline (same known
      pre-existing failures only, if any).
- [x] 8.4 Run `openspec validate state-item-characteristics --strict` and
      confirm the change is valid.
