## Why

`GraphBuilder`'s connection-resolution path (3b/3c, archived
`graphbuilder-advertising-connections`/`extension-reuse`) re-derives
identity and mismatch-characteristics from `ComponentSpec%var_specs` on
every resolution call, instead of treating the `StateItemNode`/
`GraphStateItem` created once at advertise time as the source of truth
for everything after creation. Concretely, in
`GraphBuilder_ConnectionResolution_smod.F90`:

- `for_each_matching_import` scans `dst_spec%var_specs` to find candidate
  destination imports, rather than the component's own `ComponentGraph`
  (resource index / import ports), which already holds every advertised
  item's identity from advertise time.
- `build_characteristics` rebuilds an ephemeral `CharacteristicMap` from
  `var_spec%units`/`%typekind`/`%vertical_grid` on every call, rather
  than reading `GraphStateItem%characteristics` — a persistent,
  per-item characteristics map that already exists (landed in
  `state-item-characteristics`, 5a) but is never populated by
  `GraphBuilder`'s own advertise step.
- `is_inout_borrower`/`callback_interface_id` are read directly off
  `var_spec` inside `resolve_one`'s branching, with no graph-native
  equivalent on `StateItemNode` at all.
- `try_materialize_field_extension`/`materialize_extension_chain` reach
  `ungridded_dims`/`vertical_stagger` via `component_for(this,
  export_comp_name)` + `var_spec` — a single-level-only reach into a
  named child's own `ComponentSpec`.

This is a design flaw independent of any single feature change: a
`VariableSpec` is meant to describe how to *create* a state item
(advertise-time, `advertise_one`/`materialize_composite`) — once that
item exists as a real `StateItemNode`/`GraphStateItem`, nothing
downstream should need to walk back to the originating `VariableSpec`,
let alone to a *different* component's `VariableSpec` reached by name.
It surfaced concretely while scoping `3b2-cross-component-import-
propagation`: a propagated (multi-level) import has no `var_spec` at
all at the level resolution would need one, and `component_for`'s
single-level child reach cannot find a multi-level-distant owner's
`var_spec` either — so nothing in the existing resolution path has
anywhere to look up a propagated item's identity, characteristics, or
materialization inputs. Fixing this is a precondition for 3b2 (and for
any future resolution-path work), not an optional refactor.

## What Changes

- **BREAKING** (internal-only — no change to any spec-level scenario's
  observable outcome, see Impact): `StateItemNode` gains identity and
  resolution-mode metadata set once at advertise time
  (`advertise_one`/`materialize_composite`) and read, never re-derived,
  by every later resolution step:
  - `short_name` and `state_intent` (the identity `item_key` already
    encodes as a string key — now also directly queryable off the node
    itself, not only via the resource-index string lookup).
  - `is_inout_borrower` (logical) and `callback_interface_id`
    (`CallbackInterfaceId`), mirroring the two `VariableSpec` fields of
    the same name — set once, from the advertised `var_spec`, at
    advertise time; read via new `StateItemNode` accessors everywhere
    `resolve_one`/`check_match_connection_unsatisfied` currently branch
    on `var_spec%is_inout_borrower`/`var_spec%callback_interface_id`.
- `advertise_one` additionally populates `GraphStateItem%characteristics`
  (via the existing `set_characteristic`/`StateItemCharacteristicKind`
  machinery landed in `state-item-characteristics`) from the advertised
  `var_spec`'s `units`/`typekind`/`vertical_grid`/`state_item_variant`
  fields — the same fields `build_characteristics` reads today, moved
  to run once at advertise time instead of being rebuilt on every
  resolution call.
- `for_each_matching_import` is rewritten to enumerate candidate
  destination imports from the destination component's own
  `ComponentGraph` (resource index and/or import-port table), filtered
  by the connection's declared pattern against each node's own
  `short_name`/`state_intent`, instead of scanning
  `dst_spec%var_specs`.
- `build_characteristics` is removed; every call site reads
  `GraphStateItem%get_characteristics()` off the resolved `NodeId`'s
  own node instead.
- `resolve_one`'s callback-interface/inout-borrower branch checks move
  from `var_spec%callback_interface_id%is_valid()`/
  `var_spec%is_inout_borrower` to the corresponding `StateItemNode`
  accessors.
- `try_materialize_field_extension`/`materialize_extension_chain`'s
  `ungridded_dims`/`vertical_stagger` inputs move from
  `export_var_spec%...` (reached via `component_for` + a named child's
  `ComponentSpec`) to equivalent fields captured on the export's own
  `StateItemNode`/`GraphStateItem` at advertise time, read directly off
  the already-resolved export `NodeId` — removing the single-level
  `component_for`/`ComponentSpec` reach from the materialization path
  entirely.
- `find_export_var_spec`/`find_inout_borrower_var_spec` (both `var_spec`-
  scanning helpers) are removed; their callers resolve via the
  destination/source `ComponentGraph`'s own resource index instead.
- **Explicitly out of scope**: `GraphBuilder_CallbackWiring_smod.F90`'s
  own `var_spec` reads (namespace flattening via
  `build_qualified_export_namespace`, interface conformance via
  `export_conforms_to_interface`, composite-member access) — a
  separate capability with its own `var_spec` usage for a different
  purpose (interface conformance checking, not identity/characteristic
  resolution); tracked as a follow-up gap, same precedent this
  project already has for 3b/3b2 and 4b/4b2 (a review-discovered gap
  gets its own future change rather than silent scope creep here).
  `advertise_one`/`materialize_composite` (the legitimate "create a
  node from a `var_spec`" use) are unchanged.

## Capabilities

### Modified Capabilities
- `graph/graph-builder`: "Ordinary connections are resolved into
  dependency edges" and its destination-matching/mismatch-detection
  mechanics are restated to read identity and characteristics from the
  resolved `StateItemNode`/`GraphStateItem`, never from a
  `VariableSpec` reached by component name, once advertising has
  created the node. No change to any requirement's observable
  outcome — every existing scenario (matching/mismatch/unresolved/
  materialization behavior) continues to hold; this is an
  implementation-mechanism correction surfaced as a spec delta because
  the mechanism itself ("how is an item's identity/characteristics
  found during resolution") was previously unstated and defaulted to
  the wrong one.

## Impact

- **Affected code**: `StateItemNode.F90` (new fields/accessors),
  `GraphBuilder_Advertising_smod.F90` (`advertise_one` populates the
  new fields and characteristics map),
  `GraphBuilder_ConnectionResolution_smod.F90` (`for_each_matching_import`,
  `build_characteristics` removal, `resolve_one`'s branch conditions,
  `try_materialize_field_extension`/`materialize_extension_chain`'s
  `ungridded_dims`/`vertical_stagger` source, `find_export_var_spec`/
  `find_inout_borrower_var_spec` removal).
- **Existing code read, not modified**: `GraphStateItem%characteristics`/
  `set_characteristic`/`get_characteristics` (landed,
  `state-item-characteristics`) — this change is the first real
  producer that populates them from `GraphBuilder`'s own advertise
  step; `ComponentGraph`'s existing resource-index/import-port/
  export-port tables (landed, 3b) — this change is the first real
  consumer of them for destination-match discovery.
- **New Fortran types**: none — reuses `StateItemCharacteristicKind`/
  `UnitsCharacteristic`/`TypeKindCharacteristicAdapter`/
  `VerticalGridCharacteristic` (already defined, currently only
  instantiated inside the now-removed `build_characteristics`) and
  `CallbackInterfaceId` (already defined, `mapl_CallbackInterfaceId_mod`).
- **Tests**: every existing `Test_GraphBuilder.pf` case that exercises
  ordinary resolution, mismatch detection, inout-borrower resolution,
  callback-interface routing, or materialization must continue to pass
  unmodified in its assertions (same observable outcome, per the
  Modified Capability note above) — this change's own task list treats
  "no existing assertion changes" as a correctness check, not an
  optional nice-to-have. New coverage: a case where the two
  `var_spec`-free lookup paths (`for_each_matching_import`'s node-based
  scan, `GraphStateItem%characteristics`-based mismatch detection) are
  exercised with the originating `var_spec` already gone from scope
  (not reachable from the resolution call site at all), to prove the
  removal is real and not just "the two paths happen to agree because
  both are still present."
- **Dependencies**: depends on `state-item-characteristics` (5a,
  landed) for the characteristics map this change becomes the real
  producer/consumer of; does not require `3b2-cross-component-import-
  propagation` or any export-propagation mechanism — this change fixes
  single-level resolution's own data source, independent of how many
  levels an item's identity needs to travel before resolution sees it.
- **Out of scope**: `GraphBuilder_CallbackWiring_smod.F90`'s own
  `var_spec` reads (see "What Changes"); any new propagation mechanism
  (import or export) — this change only fixes what resolution reads
  once an item is visible to it, not how far an item's visibility
  travels.
