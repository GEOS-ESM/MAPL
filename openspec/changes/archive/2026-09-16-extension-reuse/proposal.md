## Why

Phase 3b/3b2 (`graphbuilder-advertising-connections`, archived) made
`GraphBuilder` wire an ordinary `MatchConnection` export/import pair
directly into a `DependencyNetwork` edge whenever both sides advertise the
same short name — but it never checks whether the export's payload
actually matches what the import needs (grid, units, precision, ...). It
silently assumes REQ-EXT-003's no-op case always applies. For any real
configuration where an export/import pair differs in one of those
respects, that assumption is simply wrong: the legacy imperative path
(`SimpleConnection%connect_sibling` -> `StateRegistry%extend`) inserts a
framework-created extension chain (`E -> T1 -> E1 -> ...`) in exactly this
situation, and `GraphBuilder` currently has no equivalent. Per
`docs/graph/spec/20-implementation-roadmap.md` §20.4.1, this is
sub-change 3c: give `GraphBuilder` the extension/mismatch-chain machinery
`09-extension-reuse.md` (REQ-EXT-001..005) specifies, built on 3b's wiring
and Phase 2's `TransformGraphNode`.

## What Changes

- Introduce a graph-native `Characteristic`/`CharacteristicMap` pair
  (`superstructure/generic/graph/`) — the graph's own analog of legacy's
  `StateItemAspect`/`AspectMap`, deliberately named and built
  independently so it is not constrained by the existing `StateItemAspect`
  subclass hierarchy or by `18-state-item-characteristics.md`'s still-
  speculative, cross-graph-scoped `StateItemCharacteristic` design.
  `GraphBuilder`'s ordinary connection resolution
  (`graphbuilder_resolve_connections`) builds each side's
  `CharacteristicMap` directly from `VariableSpec` fields and compares
  them before wiring an export directly to a matching import, to decide
  whether REQ-EXT-003's no-op case actually applies. Legacy's
  `StateItemSpec`/`AspectMap`/`StateItemAspect%needs_extension_for` stay a
  behavioral reference only (see design.md Decisions) — not called into.
- When a mismatch exists, create a `TransformGraphNode` + extension
  `GraphStateItem`/`StateItemNode` chain (REQ-EXT-001) in whichever graph
  is already resolving that connection (the declaring parent's own graph,
  per 3b's proxy-node design — see design.md Decisions), and wire the
  import to the chain's final extension node instead of directly to the
  original export (REQ-EXT-002: the import keeps its own distinct
  `StateItemNode`, but is not independently materialized).
- Restate the existing "extension-family search" (find whether a suitable
  extension already exists so it can be reused instead of duplicated) as a
  graph-native lookup keyed off the original export's `NodeId` and the
  goal `Characteristic` (REQ-EXT-005), using `ComponentGraph`'s existing
  resource-index mechanism (REQ-CG-001, the same mechanism 3b already
  uses for advertised-item and proxy-node identity) rather than a new,
  separate data structure. This is a direct restatement of the existing
  imperative `ExtensionFamily%find_closest_spec` algorithm; it MUST NOT
  change which extensions get reused as part of this migration.
- **REQ-EXT-004 (StateRegistry/OuterComponent visibility) is deferred to
  a follow-up sub-change**, discovered mid-implementation (see design.md
  Decisions): `StateItemSpec` has no lightweight construction path
  independent of the full `AspectMap`/aspect-subclass machinery this
  change deliberately avoids reusing, so satisfying REQ-EXT-004 without
  that reuse would mean building a second, independent aspect-map
  pipeline — a substantial undertaking in its own right, and one not
  worth investing in deeply given `StateRegistry` is expected to be
  retired once graph development is far enough along. Extension items
  created by this change exist correctly in the graph (real `NodeId`s,
  real dependency edges, real reuse semantics) but are not yet pushed
  into `StateRegistry` or any ESMF state.
- No-op case (REQ-EXT-003) is preserved and now actually verified rather
  than assumed: when aspects already match exactly, `GraphBuilder` wires
  the import directly to the export's own node, unchanged from 3b's
  current behavior.
- **Scope discovered mid-implementation**: `TransformGraphNode` executes
  a `Transform` with no arguments at all (no ESMF handle passed in) — a
  fundamentally different shape from legacy's `ExtensionTransform`
  interface, and no concrete `Transform` subclass existed anywhere before
  this change. Rather than bridging to legacy's interface (which would
  reintroduce the coupling this change's mismatch-detection decisions
  already reject), each `Characteristic` gets a deferred
  `build_transform` method (mirrors `StateItemAspect%make_transform`
  exactly — ordinary OO dispatch, not a separate provider registry, per
  reviewer direction) with exactly one real implementation in this
  change: `UnitsCharacteristic` (builds `UnitsConverterTransform`, a new,
  minimal `Transform` subclass operating directly on `ESMF_Field` data).
  Any other mismatched characteristic (vertical grid, geom, typekind,
  ...) has no real `build_transform` implementation yet, so it fails
  explicitly rather than being silently wired directly or ignored (spec:
  "Unregistered characteristic fails loudly"). Real implementations for
  the remaining characteristics are left to a follow-up sub-change,
  each a replacement of that one subclass's `build_transform` body,
  without touching this change's chain-building logic.
- **Explicitly out of scope**: REQ-EXT-002a (shared-data payload instead
  of alias) stays `[SPECULATIVE]`/`[OPEN]` per `09-extension-reuse.md`
  §9.1.1 — not addressed here. `SimpleConnection`/`ReexportConnection`'s
  other behaviors beyond extension-chain creation, wildcard/callback
  connection resolution (Phase 4), and compiled execution (Phase 5/Q9)
  remain out of scope, unchanged from 3b's own scope boundary.
- **Scope boundary carried over from 3b's proxy-node design** (see
  design.md Decisions): an edge can only be added between two `NodeId`s
  owned by the same graph, so an extension chain is created in whichever
  graph is already resolving the connection (the declaring parent's own
  graph, using 3b's existing proxy machinery for a cross-component
  export), not in some separate "the export's home component" graph.
  Reuse search is therefore scoped to that one resolving graph for this
  slice — two different parents each separately proxying the same
  grandchild export do not share a reused extension. This mirrors 3b's
  own "single-level only" simplification (`3b2` was the follow-up that
  generalized 3b's own propagation gap); a cross-graph generalization of
  extension reuse, if a real configuration needs it, is left as a future
  follow-up rather than solved speculatively here.

## Capabilities

### New Capabilities
- `graph/extension-reuse`: extension-chain creation for mismatched
  export/import pairs (REQ-EXT-001/002/003), the extension-family reuse
  search (REQ-EXT-005), and explicit failure for a characteristic with no
  registered extension provider. REQ-EXT-004 (registry visibility) is
  deliberately not part of this capability's delta yet — see "Explicitly
  out of scope" and design.md Decisions.

### Modified Capabilities
- `graph/graph-builder`: ordinary connection resolution
  (`graphbuilder_resolve_connections`) no longer assumes every matched
  export/import pair is exact; it now determines whether a mismatch
  exists and, if so, delegates to `graph/extension-reuse` instead of
  wiring the import directly to the raw export node.

## Impact

- **Affected code**: `superstructure/generic/GraphBuilder.F90`
  (`resolve_match_connection`/`resolve_one`, extended to detect mismatch
  before choosing which node to wire the import to); new module(s) for
  extension-chain creation and the reuse-search traversal (exact file
  location left to design.md/task breakdown, following the existing
  `superstructure/generic/graph/`-adjacent convention).
- **Existing code read as behavioral reference only, not called into**:
  `superstructure/generic/specs/` (`VariableSpec`, `StateItemSpec`,
  `StateItemAspect` subclasses, in particular each aspect's
  `needs_extension_for`/`make_extension`) and
  `superstructure/generic/registry/ExtensionFamily.F90`/
  `StateRegistry_Extensions_smod.F90` (`extend`) — these define what
  observable behavior this capability must reproduce (REQ-EXT-005), but
  this capability's own mismatch-comparison and registration logic is
  written fresh rather than calling into them, per explicit reviewer
  direction (design.md Decisions); some duplication is an accepted
  trade-off, not an oversight. Kept as-is and not removed.
- **Existing code read and reused as-is**:
  `superstructure/generic/transforms/` (`ExtensionTransform`,
  `ConvertUnitsTransform`, `VerticalRegridTransform`, ...) as the
  concrete transform behaviors an extension chain link wraps — reusing
  the actual numerical/regrid execution strategies is unaffected by this
  revision, which is scoped to mismatch-comparison and registration
  bookkeeping only.
- **New Fortran types**: `CharacteristicId` (type-safe identity, mirrors
  `AspectId`), `Characteristic` (abstract, with a deferred
  `build_transform` method - mirrors `StateItemAspect%make_transform`)
  and concrete `UnitsCharacteristic`/`VerticalGridCharacteristic`,
  `CharacteristicMap` (a real gFTL2 polymorphic map, generated the same
  way `AspectMap` is), and `UnitsConverterTransform` (the one real
  `mapl_Transform_mod` subclass, named to avoid colliding with legacy's
  `ConvertUnitsTransform` module — see design.md Decisions for the
  rename-after-legacy-removal follow-up). Extension-chain construction
  logic wires these into `GraphBuilder`'s existing
  `TransformGraphNode`/`GraphStateItem` (Phase 1–2) types with no changes
  to those types' public APIs.
- **Tests**: new pFUnit coverage for mismatch detection
  (`CharacteristicMap` comparison), extension-chain creation
  (`TransformGraphNode` + extension `StateItemNode` wired correctly,
  import aliased to the final extension), the no-op case remaining
  unaffected, the reuse-search lookup (same extension reused for two
  importers needing the same variant, no duplicate chain created), the
  unregistered-characteristic failure path, and `UnitsConverterTransform`'s
  own numerical correctness. `StateRegistry`/`OuterComponent` visibility
  is not tested here — deferred with REQ-EXT-004 (see "What Changes").
  Where practical, extend the existing `graphbuilder_equivalence`
  real-configuration fixture (or add a sibling fixture) with at least one
  genuine `units` mismatch case (the one characteristic with a real
  provider in this change) so the side-by-side equivalence check from 3b
  now also covers extension creation, not only the no-op case it happened
  to exercise.
- **Dependencies**: builds on 3b's `GraphBuilder`/`DependencyNetwork`
  wiring and Phase 2's `TransformGraphNode`/port-binding machinery; no new
  external dependencies.
- **Out of scope**: REQ-EXT-002a (shared-data payload), wildcard/callback
  connection resolution (Phase 4), compiled execution (Phase 5/Q9),
  removal of the legacy imperative extension path (kept as the
  equivalence oracle, same posture as 3b), real (executing) extension
  providers for any characteristic other than `units` (vertical grid,
  geom, typekind, ungridded dims, ...) — detected but explicitly
  unsupported in this change, left to a follow-up sub-change per
  design.md Decisions — and REQ-EXT-004 (`StateRegistry`/`OuterComponent`
  visibility), also left to a follow-up sub-change.
