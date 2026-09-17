## Context

3b/3b2 (`graphbuilder-advertising-connections`, archived) gave
`GraphBuilder` a `resolve_match_connection`/`resolve_one` pair
(`superstructure/generic/GraphBuilder.F90`) that, for each ordinary
`MatchConnection` pair, looks up the import's and export's `NodeId`
(creating a parent-local proxy via `get_or_make_local_node_id` when the
real item lives on a named child, per REQ-HIER-006) and, if both exist,
adds a dependency edge between them in `this_graph`'s default network —
`this_graph` being whichever `OuterMetaComponent`'s `ComponentGraph` owns
the declared connection (typically the parent coordinating two children,
per `ComponentSpec%connections`). It never inspects whether the export's
and import's declared characteristics actually match; every found pair is
wired directly. This is REQ-EXT-003's no-op case, but currently applied
unconditionally rather than after a check. Advertised items' payloads are
left unallocated at this point in the lifecycle (`advertise_one`'s own
comment: "this item is advertised, not yet realized - REALIZE happens in
a later init phase this slice does not touch") — so nothing in this
change can assume a live `ESMF_Field` exists at connection-resolution
time either; extension chains built here are real graph structure, not
yet materialized ESMF payloads.

Legacy's actual behavior (`SimpleConnection%connect_sibling`, calling
`StateRegistry%extend` — `superstructure/generic/registry/
StateRegistry_Extensions_smod.F90`) is invoked for *every* matched pair,
including exact-name `MatchConnection` ones (`MatchConnection%connect`
delegates each match to a `SimpleConnection`). `extend()` uses
`ExtensionFamily%find_closest_spec` (`ExtensionFamily.F90`) to find the
already-created extension in the export's family that comes closest to
the import's required aspects, then repeatedly calls
`closest_extension%make_extension(goal_spec)` — implemented per-aspect
via `StateItemAspect%needs_extension_for`/`make_transform`
(`superstructure/generic/specs/*Aspect.F90`) — until no further extension
is needed. Each step registers a new extension `StateItemSpec` in the
export's own `StateRegistry` subregistry family and a coupler
(`GenericCoupler`/`CouplerMetaComponent`) that executes the concrete
`ExtensionTransform` (`ConvertUnitsTransform`, `VerticalRegridTransform`,
...) at run time. `StateItemSpec` has no construction path independent of
this machinery — every operation that matters (`create`, `allocate`,
`connect`, `add_to_state`) works through its `AspectMap`, which is built
by `VariableSpec%make_StateitemSpec`/`make_aspects`.

Phase 2 already provides `TransformGraphNode` (named ports, external
port-binding table keyed by `(DependencyNetworkId, NodeId)`,
`superstructure/generic/graph/` — see `10-transforms-and-ports.md`) as the
graph-level representation of a computation node, and `DependencyNetwork`
already exposes `get_successors`/`get_predecessors`/
`contains_dependency` (`DependencyNetwork.F90`) for structural queries.
No concrete `mapl_Transform_mod` subclass existed anywhere before this
change - only pFUnit test doubles (`Test_TransformGraphNode.pf`).
`TransformGraphNode%execute()` calls `Transform%compute(this, rc)` with
no other arguments; the generic demand-driven update algorithm
(`ComponentGraph_DemandDrivenUpdate.F90`) resolves port bindings to
`NodeId`s/revisions only and never hands a `Transform` any ESMF handle or
`NodeId` - a concrete `Transform` must hold whatever references it needs
itself, set at construction time.

The per-aspect/family mechanics described above (`ExtensionFamily`,
`StateItemAspect%needs_extension_for`, `StateRegistry_Extensions_smod`,
`StateItemSpec`) are described here only as background for what
observable behavior this capability must reproduce. See Decisions below
for why this change does not call into them, and does not attempt a
from-scratch equivalent of the `StateItemSpec`/`AspectMap` construction
pipeline either.

See proposal.md for motivation and the full scope boundary.

## Goals / Non-Goals

**Goals:**
- Introduce `Characteristic`/`CharacteristicId`/`CharacteristicMap` — a
  graph-native, independently-designed analog of legacy's
  `StateItemAspect`/`AspectId`/`AspectMap` (see Decisions) — and use it to
  make `graphbuilder_resolve_connections`'s existing no-op wiring
  conditional on an actual match check, rather than calling into legacy's
  `StateItemSpec`/`AspectMap` machinery.
- When a mismatch is found, create a `TransformGraphNode` + extension
  `StateItemNode` chain in the same graph that is already resolving the
  connection (`this_graph`, as established by 3b), wired from the
  export's (or its proxy's) `NodeId` to the import's `NodeId` through the
  chain. One chain link per mismatched characteristic, built by that
  characteristic's own `build_transform` deferred method (see Decisions)
  - a real one for `units`; any other mismatched characteristic fails the
  connection explicitly rather than being silently skipped or wired
  directly.
- Restate the reuse search (REQ-EXT-005) as a graph-native lookup, using
  `ComponentGraph`'s existing resource-index mechanism (REQ-CG-001, the
  same one 3b already uses for advertised-item and proxy-node identity)
  keyed by the export's `NodeId` and the goal characteristic, rather than
  a new, separate data structure.

**Non-Goals:**
- REQ-EXT-002a (shared-data payload) — stays speculative, not attempted.
- Generalizing extension reuse across two different resolving graphs
  (e.g. two separate parents each proxying the same grandchild export) —
  scoped out for this slice, same posture as 3b's single-level
  simplification (see proposal.md "Scope boundary carried over from 3b's
  proxy-node design").
- Replacing or removing the legacy `ExtensionFamily`/`extend()`/
  `GenericCoupler` path, or changing how an extension chain actually
  executes at run time through that path (still the existing
  `ExtensionTransform`/coupler machinery, invoked the same way it is
  today) — this change only adds the graph-level representation and
  detection/reuse logic alongside it, matching 3b's "kept as the
  equivalence oracle" posture.
- Any change to `TransformGraphNode`/`DependencyNetwork`/`GraphStateItem`
  public APIs from Phase 1–2.
- Real (executing) extension support for every declared characteristic.
  Discovered mid-implementation (see Decisions, "build_transform is a
  deferred method on Characteristic"): only `units` gets a real,
  executing `Transform` in this change; every other characteristic
  (vertical grid, geom, typekind, ...) is detected as a mismatch but its
  own `build_transform` has no real implementation yet, so chain-creation
  for it fails explicitly rather than executing. A follow-up sub-change
  is expected to give the remaining `Characteristic` subclasses a real
  `build_transform` implementation.
- REQ-EXT-004 (`StateRegistry`/`OuterComponent` visibility of created
  extension items). Discovered mid-implementation (see Decisions,
  "REQ-EXT-004 is deferred to a follow-up sub-change"): building it now
  would mean an independent, from-scratch `AspectMap`-equivalent
  construction pipeline, since `StateItemSpec` has no lighter-weight
  path. Extension items created by this change are real graph structure
  (real `NodeId`s, real dependency edges, real reuse semantics) but are
  not yet pushed into `StateRegistry` or any ESMF state.
- Reusing `VariableSpec%make_StateitemSpec`/`StateItemAspect`
  polymorphism, or calling into `ExtensionFamily`/`extend()`'s
  implementation, or `StateRegistry`'s existing methods, for this
  capability's own comparison logic — a deliberate choice (see
  Decisions), not an oversight; all remain in scope only as behavioral
  reference.

## Decisions

**"Characteristic" is the graph's own name for what legacy calls an
"Aspect," and is a deliberately independent design, not a reskin.**
Introduces `mapl_CharacteristicId_mod`
(`superstructure/generic/graph/CharacteristicId.F90`) - a type-safe
identity (wrapped integer, named parameter constants
`UNITS_CHARACTERISTIC_ID`/`VERTICAL_GRID_CHARACTERISTIC_ID`, `==`/`/=`/
`<`, `to_string()`) mirroring `AspectId.F90`'s own pattern exactly, and
`mapl_Characteristic_mod` (`Characteristic.F90`): an abstract
`Characteristic` type with deferred `get_id()` (nopass - a fixed property
of the concrete type, mirrors `StateItemAspect%get_aspect_id`),
`get_signature()`, `needs_extension_for(this, goal)`, and
`build_transform(...)` (see next decision), plus `CharacteristicMap`
(`CharacteristicId -> class(Characteristic)`) - a **real gFTL2
polymorphic map**, generated with the exact same
`Key`/`Key_LT`/`T`/`T_polymorphic`/`Map` macro instantiation
`StateItemAspect.F90` already uses to generate `AspectMap`, not a
hand-written container (an earlier draft of this decision used a
hand-written linear-scan map instead, reasoning that a gFTL
template-instantiation was an avoidable risk; reviewer direction: use the
established gFTL pattern, matching the rest of the codebase's own
convention for exactly this shape of container). Concrete characteristics
for this change: `UnitsCharacteristic` (holds a units string;
`needs_extension_for` is a plain string inequality check, mirroring - not
calling - `UnitsAspect%matches`'s own logic) and
`VerticalGridCharacteristic` (a minimal presence/identity check, enough
to detect a mismatch - not enough to regrid; see the next decision for
why that is fine for this change's scope). Deliberately independent of
both `StateItemAspect` (legacy, `AspectMap`-based, built for
`StateRegistry`'s coupler-state bookkeeping) and
`18-state-item-characteristics.md`'s `StateItemCharacteristic` (still
`[SPECULATIVE]`, scoped to cross-graph mechanics/Q12/Q13 this change does
not need) - reviewer direction: name and design this fresh so the graph's
own characteristic model is never constrained by either. `GraphBuilder`
builds each side's `CharacteristicMap` directly from `VariableSpec`
fields (`units`, `vertical_grid`); `ExtensionFamily%find_closest_spec`/
each `*Aspect%needs_extension_for` remain the **behavioral reference**
for which characteristics matter and what "needs extension" means - read
for understanding, never called. Some duplication is accepted
deliberately here in exchange for an implementation not constrained by,
or coupled to, the legacy per-aspect object model.

**`build_transform` is a deferred method on `Characteristic` itself, not
a separate provider registry - mirrors `StateItemAspect%make_transform`
exactly.** An earlier draft of this decision introduced a standalone
`mapl_ExtensionProvider_mod` (a procedure-pointer table mapping
characteristic name -> "build a `Transform`" procedure) precisely to
avoid the legacy-coupling risk of bridging to `ExtensionTransform`'s
interface. Reviewer direction: that registry re-invented what ordinary
OO dispatch already gives for free - legacy's own `StateItemAspect`
already puts `make_transform` directly on the aspect, not in a lookup
table, and `Characteristic` should follow that shape (not `StateItemAspect`'s
implementation) for the same reason. `ExtensionProvider.F90` is removed;
`Characteristic` gains a deferred `build_transform(this, graph,
input_node_id, output_node_id, goal, transformer, rc)` method. Discovered
mid-implementation (Context): no concrete `Transform` subclass existed
anywhere, and its execution shape (`compute(this, rc)`, no arguments) is
fundamentally different from legacy's `ExtensionTransform` interface, so
`build_transform` cannot bridge to `ExtensionTransform%update()` without
reintroducing the `import[1]`/`export[1]`/`State`-wrapping coupling the
`Characteristic` decision above already rejects. `UnitsCharacteristic%build_transform`
is the one real implementation in this change: it constructs
`UnitsConverterTransform` (a new `mapl_Transform_mod` subclass,
`UnitsConverterTransform.F90` - named `UnitsConverterTransform`, not
`ConvertUnitsTransform`, because `mapl_ConvertUnitsTransform_mod`/
`ConvertUnitsTransform` is already taken by the legacy module this type
is deliberately independent of, and module names share one global
namespace; rename to `ConvertUnitsTransform`, matching
`RegridTransform`'s naming convention, once the legacy module is removed
- tracked as a task in the 3c2 follow-up) holding the owning
`ComponentGraph` plus the input/output `NodeId`s; at `compute()` time it
pulls both `GraphStateItem`s' `ESMF_Field`s directly (`get_field()`,
`assign_fptr` from the general-purpose `mapl_FieldPointerUtilities_mod` -
infrastructure, not legacy-coupling) and applies a real unit conversion
via `udunits2f`'s `Converter` (the same general-purpose library legacy's
own `ConvertUnitsTransform` uses - reusing it is not the kind of coupling
this change avoids; it is a correctness-critical external library, not
`StateRegistry`/aspect bookkeeping). `VerticalGridCharacteristic%build_transform`
always fails explicitly (spec: "Unregistered characteristic fails
loudly") - there is no registry entry to be "missing"; the failure is
just this subclass's own deferred method reporting it has no real
conversion yet. A follow-up sub-change gives it (and other
characteristics) a real implementation by replacing that one method body
- no chain-building-loop changes needed, exactly the extensibility the
rejected registry design was trying to buy a different way.

**Extension chain lives in the connection-resolving graph, not a
separate "export's home" graph.** `ComponentGraph%add_dependency` only
accepts two `NodeId`s owned by the same graph (REQ-HIER-005); 3b already
established that a cross-component export is represented, inside the
resolving graph, by a proxy `StateItemNode` rather than by reaching into
the child's own graph directly. This change follows the same rule:
`TransformGraphNode`s and extension `StateItemNode`s created for a
mismatch are registered in `this_graph` (whichever graph is already
resolving that connection). Rejected alternative: creating the chain in
the exporting component's own `ComponentGraph` (closer to legacy's
`src_registry` placement) - rejected because `ComponentGraph`/
`DependencyNetwork` (Phase 1–2) has no cross-graph edge concept at all
(REQ-CG-002 forbids it structurally, and introducing one would be a
Phase 1–2 API change this slice's Non-Goals explicitly exclude).
Consequence, called out in proposal.md: reuse search (next decision) is
scoped per resolving graph, not globally per export.

**Reuse search uses `ComponentGraph`'s resource index, keyed by export
`NodeId` + characteristic, not a live `get_successors` walk - corrected
mid-implementation.** The original draft of this decision proposed
walking `DependencyNetwork%get_successors` from the export's `NodeId` to
find existing chains and inspect their final extension's characteristics.
That does not work with Phase 1–2's actual public API: `TransformGraphNode`
holds its bound `Transform` in a *private* component with no accessor
exposing it, so a structural walk can find candidate successor nodes but
cannot recover *which characteristic value* an existing chain's output
represents without reaching into that private state - information the
graph's edges alone do not carry (contrary to this decision's original
assumption). Reusing `ComponentGraph%add_resource_index`/
`get_resource_index` (REQ-CG-001) - the exact same mechanism 3b already
established for `item_key`/`proxy_key` identity lookups - is not a new,
separate data structure; it is the graph's own existing indexing
mechanism, applied to one more key shape:
`"EXTCHAIN:" // export_node_id%to_string() // ":" //
characteristic_value_signature` (one `Characteristic%get_signature()`
segment per mismatched kind, e.g. `"units=km"`) -> the resulting
extension `NodeId`. A second import needing the same variant looks up
the same key
and reuses the `NodeId` found; a different goal value produces a
different key and a new entry. This still satisfies REQ-EXT-005's "search
over the region associated with the original export" at the *observable*
level (same reuse/no-reuse decisions as `ExtensionFamily%find_closest_spec`,
per the required equivalence test) without depending on introspecting a
Phase 1–2 type's private state.

**REQ-EXT-004 is deferred to a follow-up sub-change.** `StateItemSpec`
has no construction path independent of its full `AspectMap` (Context) -
satisfying REQ-EXT-004 without reusing `StateRegistry`'s existing
methods would mean building a second, independent aspect-map-equivalent
construction pipeline, a substantially larger undertaking than a "one
small new method" and disproportionate given `StateRegistry` is expected
to be retired once graph development is far enough along (reviewer
direction). This change stops at real, correct graph structure; making
extension items visible outside the graph is named as its own follow-up
sub-change once that investment is actually justified. Rejected
alternative: a minimal `ESMF_StateAdd`-only bridge with no
`StateItemSpec` involvement at all - considered and explicitly not
chosen (reviewer direction), to avoid a partial, easily-stale bridge that
still has to be redone properly later.

## Risks / Trade-offs

- **[Risk] Duplicated mismatch-comparison logic diverging from legacy's
  `needs_extension_for` over time** (two independent implementations of
  "does this characteristic need a transform," one new in `Characteristic`,
  one in `StateItemAspect`) → **Accepted deliberately** (reviewer
  direction: avoid coupling this capability to the legacy per-aspect
  object model even at the cost of some duplication) — not something
  code-level reuse is used to prevent. **Mitigation:** keep each
  `Characteristic` subclass small and focused so it is easy to audit
  against the legacy reference by inspection; the reuse-search
  equivalence scenario (spec: "Same reuse decision as the existing
  algorithm") is the required regression check that *observable*
  behavior still matches, run against at least one real mismatched
  configuration, same posture as 3b's equivalence test.
- **[Risk] Extension items are graph-internal only until REQ-EXT-004's
  follow-up lands** — nothing outside the graph can see them, so no real
  component's ESMF-visible behavior actually changes end-to-end from this
  change alone → **Accepted, documented explicitly** (Non-Goals,
  proposal.md), not silently absorbed. Consistent with 3b's own posture:
  the graph representation is additive/non-load-bearing until a later
  phase's integration work makes it the source of truth.
- **[Risk] Creating a graph-side chain for every mismatch adds bookkeeping
  cost during this transitional phase** → **Mitigation:** accepted for
  this phase, same trade-off 3b already accepted for connection
  resolution generally; the legacy path remains the ESMF-visible source
  of truth until a later phase decides to retire it.
- **[Risk] Per-resolving-graph reuse scoping** (Decisions, "Extension
  chain lives in the connection-resolving graph") means the same
  grandchild export extended identically by two different parents is NOT
  deduplicated across those two parents' graphs → **Mitigation:**
  documented explicitly in proposal.md as a scope boundary rather than
  silently assumed; revisit only if a real configuration surfaces it,
  mirroring exactly how 3b's own single-level gap was discovered and
  became 3b2 rather than being solved speculatively up front.
- **[Risk] Chain-building non-convergence** (a characteristic chain that
  never resolves) → **Mitigation:** give the chain-building loop its own
  iteration cap and explicit failure, modeled on legacy `extend()`'s
  `MAX_ITERATIONS` guard as a reference for the failure mode to guard
  against — not shared code — rather than assuming a graph-based loop
  cannot loop forever.
- **[Risk] Only `units` executes for real; a real configuration mismatched
  on any other characteristic (vertical grid, geom, typekind, ...) fails
  this change's connection resolution outright** → **Accepted narrowing,
  documented explicitly** (see Goals/Non-Goals, proposal.md), not silently
  absorbed. **Mitigation:** the failure is explicit and identifies the
  unsupported characteristic (spec: "Unregistered characteristic fails
  loudly"), never a silent direct-wire or no-op; adding a real provider
  for another characteristic is a follow-up sub-change replacing that one
  subclass's `build_transform` body, not a chain-building-loop rewrite.

## Open Questions

None. On-disk locations: `superstructure/generic/graph/` for
`CharacteristicId.F90`, `Characteristic.F90`, `UnitsCharacteristic.F90`,
`VerticalGridCharacteristic.F90`, `UnitsConverterTransform.F90`, and
`ExtensionResolution.F90` (mismatch detection, chain creation, reuse
lookup) - this code depends only on graph-neutral types (`Transform`,
`TransformGraphNode`, `DependencyNetwork`, `GraphStateItem`,
`ComponentGraph`), not on `OuterMetaComponent`/`StateRegistry`, so it
belongs with the rest of the graph-neutral core rather than with
`GraphBuilder.F90`'s upward-facing integration code (mirrors 3b's own
placement rationale for `GraphBuilder.F90`, applied in the opposite
direction).
