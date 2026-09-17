## 1. Module scaffolding

- [x] 1.1 Create new files under `superstructure/generic/graph/`:
      `Characteristic.F90`, `UnitsCharacteristic.F90`,
      `VerticalGridCharacteristic.F90`, `ExtensionProvider.F90`,
      `UnitsConvertTransform.F90`, `ExtensionResolution.F90` (design.md
      resolved Open Question).
- [x] 1.2 Add the new source files to
      `superstructure/generic/graph/CMakeLists.txt`'s `srcs` list.

## 2. Characteristic / CharacteristicMap

- [x] 2.1 Implement `mapl_Characteristic_mod`: abstract `Characteristic`
      type with deferred `get_name()` and
      `needs_extension_for(this, goal) result(logical)`.
- [x] 2.2 Implement `CharacteristicMap` (hand-written, linear-scan; not a
      gFTL polymorphic-map instantiation - design.md Decisions): `add`,
      `has`, `get`, `size`, iteration by name.
- [x] 2.3 Implement `UnitsCharacteristic` (holds a units string;
      `needs_extension_for` is a plain string inequality check).
- [x] 2.4 Implement `VerticalGridCharacteristic` (minimal
      presence/identity check - enough to detect a mismatch, not enough
      to regrid).
- [x] 2.5 Implement `build_characteristics(var_spec) result(CharacteristicMap)`.
      Implemented in `GraphBuilder.F90` rather than `ExtensionResolution.F90`
      (deviation from the original task text, discovered during
      implementation): it reads `VariableSpec` fields, which is legacy/
      `ComponentSpec`-facing data `GraphBuilder.F90` already depends on -
      keeping it there preserves `ExtensionResolution.F90`'s graph-neutral
      dependency footprint (design.md's own stated placement rationale for
      that module). Reads `units`/`vertical_grid` directly - no
      `StateItemSpec`/`AspectMap`/`make_StateitemSpec` involvement.
- [x] 2.6 Implement `find_mismatched_characteristics(export_map,
      import_map) result(list of names)` comparing two `CharacteristicMap`s
      via each entry's `needs_extension_for`.
- [x] 2.7 Wire mismatch detection into `resolve_one` (`GraphBuilder.F90`)
      ahead of the existing direct-wire branch: no mismatch keeps today's
      behavior (wire import directly to export/proxy node, spec "Matching
      export and import are wired"); any mismatch routes to the new
      extension-chain path (task group 4).

## 3. Reuse search

- [x] 3.1 Define the resource-index key shape for an extension chain:
      `"EXTCHAIN:" // export_node_id%to_string() // ":" //
      characteristic_name // "=" // characteristic_value_signature`
      (design.md Decisions - "Reuse search uses `ComponentGraph`'s
      resource index").
- [x] 3.2 Implement a lookup: given `this_graph`, export `NodeId`, and a
      goal `Characteristic`, check `get_resource_index` for an existing
      matching extension `NodeId`; return "no match" distinguishably when
      absent.
- [x] 3.3 On successful chain creation (task group 4), record the new
      extension `NodeId` via `add_resource_index` under the same key
      shape, so a later lookup for the same export + goal value reuses it.
- [x] 3.4 Cap the chain-building iteration count (mirroring legacy
      `extend()`'s `MAX_ITERATIONS` guard as a failure-mode reference, not
      shared code), failing explicitly rather than looping indefinitely.

## 4. Extension chain creation

- [x] 4.1 Implement `mapl_ExtensionProvider_mod`: an
      `I_build_transform(graph, input_node_id, output_node_id,
      export_characteristic, import_characteristic, transformer, rc)`
      abstract interface and a small provider-registry array type
      (name -> procedure pointer) with `lookup(name)` returning "no
      provider" distinguishably when absent.
- [x] 4.2 Implement `UnitsConvertTransform` (`mapl_Transform_mod`
      subclass): declares one input port and one output port; holds the
      owning `ComponentGraph` plus input/output `NodeId`s and the
      src/dst units strings; `compute()` reads both bound
      `GraphStateItem`s' `ESMF_Field`s (`get_field()`), gets data
      pointers via `mapl_FieldPointerUtilities_mod`'s `assign_fptr`
      (typekind-dispatched r4/r8), and applies a real conversion via
      `udunits2f`'s `Converter`/`get_converter`. No `ExtensionTransform`/
      `State`-wrapping involvement.
- [x] 4.3 Implement `build_units_transform` (matches the
      `I_build_transform` interface) and register it under the `units`
      key in the default production provider registry.
- [x] 4.4 In `ExtensionResolution.F90`, for each mismatched
      characteristic (task 2.6's output, in a fixed order): look up its
      provider (task 4.1). If found: create the extension `StateItemNode`
      (`next_node_id`/`register_node`, payload left unallocated per
      Context - matches `advertise_one`'s own "not yet realized"
      precedent), create one `TransformGraphNode` wrapping the provider's
      `Transform`, bind its ports (`bind_port`, REQ-XFORM-005), and add
      the dependency edges: export/proxy node -> transform node ->
      extension node. Chain multiple steps when more than one
      characteristic differs and has a provider (spec "Multi-step
      mismatch gets a chain"). If not found: fail the connection
      explicitly, identifying the unsupported characteristic (spec
      "Unregistered characteristic fails loudly") - do not wire directly,
      do not silently skip.
- [x] 4.5 Wire the final extension node to the import's `NodeId` with a
      dependency edge; confirm the import's own `StateItemNode` identity
      is unchanged (spec "Import identity is preserved") and that only
      its payload binding points at the chain's final extension node
      (spec "Import payload is not independently allocated").
- [x] 4.6 Confirm the no-mismatch path (task 2.7's direct-wire branch)
      creates no `TransformGraphNode`/extension node (spec "No-op case
      creates nothing").

## 5. GraphBuilder integration

- [x] 5.1 Update `graphbuilder_resolve_connections`/`resolve_match_connection`
      (`GraphBuilder.F90`) to call mismatch detection (task group 2)
      before choosing which node to wire an import to, delegating to the
      extension-chain path (task groups 3-4) on mismatch.
- [x] 5.2 Confirm `graphbuilder_resolve_connections`'s existing
      idempotency (no-op once the graph is frozen) still holds when a
      resolution attempt would otherwise create an extension chain.

## 6. Unit tests

- [x] 6.1 Add pFUnit cases for `CharacteristicMap`/mismatch detection:
      exact match reports no mismatch; a units-only difference and a
      units+vertical_grid difference both report mismatch (with the
      correct set of mismatched characteristic names identified).
- [x] 6.2 Add pFUnit cases for extension-chain creation: a units
      mismatch produces one `UnitsConverterTransform` + one extension node
      (spec "Single-step mismatch gets one transform"); a mismatch on two
      characteristics produces a chain (spec "Multi-step mismatch gets a
      chain") - `units` plus a test-only `FakeCharacteristic`
      (`MOCK_CHARACTERISTIC_ID`, see task 9.3 - superseded design: the
      original text here described a fake registry entry, replaced by a
      fake `Characteristic` after `build_transform` became a deferred
      method) so the chaining mechanism can be tested without needing a
      second real, executing characteristic.
- [x] 6.3 Add a pFUnit case for a mismatch on a characteristic with no
      real `build_transform` implementation (`vertical_grid`): the
      connection fails explicitly, identifying the characteristic, and
      no dependency edge is created (spec "Unregistered characteristic
      fails loudly").
- [x] 6.4 Add pFUnit cases for reuse search: a second import needing the
      same variant reuses the first extension, no duplicate created
      (spec "Second importer needing the same variant reuses the first
      extension"); a second import needing a different variant creates a
      new extension without disturbing the first (spec "Different
      variant still creates a new extension").
- [x] 6.5 Add a pFUnit case confirming the no-op case is unaffected (spec
      "No-op case creates nothing" / graph-builder spec "Matching export
      and import are wired").
- [x] 6.6 Add a pFUnit case for `UnitsConverterTransform` itself: given two
      `GraphStateItem`s with fields in different (compatible) units,
      `compute()` produces the numerically correct converted value.

## 7. Real-configuration equivalence

- [x] 7.1 Added a sibling fixture,
      `superstructure/generic/tests/scenarios/graphbuilder_equivalence_units/`
      (`cap.yaml`/`comp_src.yaml`/`comp_dst.yaml`) - identical to
      `graphbuilder_equivalence/` except `comp_src` exports `T` in `'m'`
      and `comp_dst` imports it in `'km'`, a genuine, convertible units
      mismatch (the one characteristic with a real, executing provider in
      this change).
- [x] 7.2 Added `test_units_mismatch_resolution_matches_legacy_coupler`
      to `Test_GraphBuilderEquivalence.pf`, run through the same real
      (DSO-backed, full `GENERIC_INIT_PHASE_SEQUENCE`) harness as the
      existing exact-match equivalence test. Legacy oracle: comp_dst's
      real ESMF import field for `T` still exists
      (`ESMF_FieldIsCreated`), proving `SimpleConnection%connect_sibling`
      -> `StateRegistry%extend` -> `ExtensionFamily`/`ConvertUnitsTransform`
      still resolves the mismatch. Graph oracle: unlike the exact-match
      case, `cap`'s own dependency network does NOT contain a direct
      edge between the export/import proxies (spec "Mismatched export
      and import are wired through an extension chain") - the export
      proxy has successors and the import proxy has predecessors instead,
      confirming a real `TransformGraphNode` is interposed. (A literal
      node-for-node comparison against `ExtensionFamily`'s internal
      family-list state - the spec's "Same reuse decision as the
      existing algorithm" scenario, worded generically - is not
      practical without exposing `ExtensionFamily`'s otherwise-private
      internals to a test; the chosen oracle instead checks both paths'
      *observable* outcome on the same real configuration, matching
      3b's own equivalence test's level of rigor.) Verified: full
      `MAPL.generic.*` ctest label set passes (7/7), full ctest run
      shows the same 7 pre-existing, unrelated failures only (ll-ll/
      cs-cs/cs-ll/ll-cs missing `LOCAL_REGRESSION_DATA_DIR`,
      `MAPL3G_Comp_Test_case02/11/23`).

## 8. Build and verification

- [x] 8.1 Build MAPL with the NAG compiler (`module load nag-stack`
      before any `cmake`/`make`/`ctest` invocation, per project
      convention) and confirm a clean build with the new source files
      included.
- [x] 8.2 Run the new pFUnit suites (task group 6) directly with `-v` and
      confirm all pass.
- [x] 8.3 Run the full `MAPL.generic.*` `ctest` label set and confirm no
      regressions relative to the pre-change baseline (same known
      pre-existing failures only, if any).
- [x] 8.4 Run `openspec validate extension-reuse --strict` and confirm
      the change is valid.

## 9. Post-implementation rework (reviewer feedback)

After task 8's initial verification pass, review of the as-built code
identified four design issues, all addressed and re-verified (full
rebuild + `MAPL.generic.*` ctest rerun, same 126/126 graph-suite pass,
same 7 pre-existing unrelated failures elsewhere):

- [x] 9.1 **Type-safe characteristic kind.** `Characteristic%get_kind()`
      (a plain string) replaced with `CharacteristicId` -
      `superstructure/generic/graph/CharacteristicId.F90`, mirroring
      `AspectId.F90`'s pattern exactly (wrapped integer, named parameter
      constants `UNITS_CHARACTERISTIC_ID`/`VERTICAL_GRID_CHARACTERISTIC_ID`/
      `MOCK_CHARACTERISTIC_ID`, `==`/`/=`/`<`, `to_string()`).
      `Characteristic%get_kind()` renamed to `get_id()` (nopass, mirrors
      `StateItemAspect%get_aspect_id`) returning `CharacteristicId`.
- [x] 9.2 **`CharacteristicMap` is a real gFTL2 polymorphic map**, not a
      hand-written linear-scan container - regenerated in
      `Characteristic.F90` using the exact `Key`/`Key_LT`/`T`/
      `T_polymorphic`/`Map`/`MapIterator`/`Pair` macro instantiation
      `StateItemAspect.F90` already uses for `AspectMap` (same
      `map/header.inc`+`public.inc`+`specification.inc`+`procedures.inc`+
      `tail.inc` include sequence). Callers use gFTL's own
      `insert`/`at`/`count`/`ftn_begin`/`ftn_end` API instead of the
      removed hand-written `add`/`has`/`get`/`size`/`kind_at`. Required
      `use iso_fortran_env, only: INT64` and `operator(<)` on
      `CharacteristicId` in scope (both needed transitively by the gFTL
      map template - found via NAG compile errors, not obvious from the
      template usage alone).
- [x] 9.3 **`ExtensionProvider.F90` removed; `build_transform` is a
      deferred method on `Characteristic`** (mirrors
      `StateItemAspect%make_transform` - ordinary OO dispatch, not a
      registry indirection). `UnitsCharacteristic%build_transform`
      constructs the real `Transform`; `VerticalGridCharacteristic%build_transform`
      fails explicitly (spec "Unregistered characteristic fails loudly"
      falls directly out of this, with no registry lookup involved).
      `mapl_ExtensionResolution_mod`'s chain-building loop calls
      `export_characteristic%build_transform(...)` directly; no registry
      parameter threaded through `find_or_build_extension_chain`/
      `GraphBuilder.F90` anymore. `mismatched_kinds` changed from a
      `StringVector` of names to a plain `CharacteristicId` array.
      Test-only "second characteristic" coverage for the multi-step chain
      test (task 6.2) now uses a `FakeCharacteristic`
      (`MOCK_CHARACTERISTIC_ID`, mirroring `AspectId.F90`'s own
      `MOCK_ASPECT_ID` precedent) instead of a fake registry entry.
- [x] 9.4 **`UnitsConvertTransform` renamed to `UnitsConverterTransform`**
      (`mapl_UnitsConverterTransform_mod`,
      `UnitsConverterTransform.F90`) - `mapl_ConvertUnitsTransform_mod`/
      `ConvertUnitsTransform` is already taken by the legacy module this
      type is deliberately independent of, and module names share one
      global namespace. **Follow-up task (3c2 or later cleanup, per
      docs/graph/spec/20-implementation-roadmap.md): rename to
      `ConvertUnitsTransform`/`mapl_ConvertUnitsTransform_mod`, matching
      `RegridTransform`'s naming convention, once the legacy
      `superstructure/generic/transforms/ConvertUnitsTransform.F90`
      module is removed.**
