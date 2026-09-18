## Context

See proposal.md for motivation. An earlier draft of this design proposed
satisfying REQ-EXT-004 by reading the real, already-allocated extension
`StateItemSpec` that legacy's own `SimpleConnection%connect_sibling` ->
`StateRegistry%extend()` already creates for every mismatch, and linking
the graph's own extension node to that same payload. **Rejected by
reviewer direction**: `StateRegistry` is slated for removal — a solution
that depends on reading its live objects at runtime, even read-only,
would need to be redone the moment `StateRegistry` is retired, which
defeats the point of doing this migration work in graph-native form.
This capability's implementation MUST be exclusive to the graph layer
(`superstructure/generic/graph/`, including `GraphStateItem`) — no
runtime dependency on `StateRegistry`, `StateItemSpec`, `VariableSpec`'s
`make_StateItemSpec`/`make_aspects`, or any `ClassAspect` subclass.
`StateRegistry`/`StateItemSpec`/`ClassAspect` remain useful only as
**read-only background reference** for understanding what a correct
payload looks like (Findings 1 and 4 below record what that reading
found) — never as a called-into dependency.

**Finding 1 — `StateItemSpec` has no lightweight constructor, and its
`FieldClassAspect%create()`/`allocate()` machinery is tightly coupled to
`AspectMap`/`ESMF_Info` bookkeeping** (`superstructure/generic/specs/
StateItemSpec.F90:106-116`, `FieldClassAspect.F90:164-217`) — confirms
this capability cannot reuse that machinery directly even read-only in
any lightweight way; it is a reference for *what a correct field needs*
(geom, typekind, ungridded dims, vertical grid, units), not a dependency
to call.

**Finding 2 — a general-purpose, `StateRegistry`-independent field
factory already exists and is the right tool for this job:**
`mapl_FieldCreate_mod::FieldCreate` (`infrastructure/field/
FieldCreate.F90:48-95`) builds a real, fully allocated `ESMF_Field` from
plain arguments — `geom`, `typekind`, `ungridded_dims`, `vgrid`
(`mapl_VerticalGrid`), `vert_staggerloc`, `units`, `standard_name`,
`long_name` — with no `StateRegistry`/`StateItemSpec`/`AspectMap`
involvement at all. It lives in `infrastructure/field/`, the same
dependency tier as `mapl_FieldPointerUtilities_mod`
(`infrastructure/esmf/FieldPointerUtilities.F90`), which 3c's own
`UnitsConverterTransform.F90:43` already depends on directly — i.e.,
already an accepted class of dependency for graph-layer code (general
infrastructure, not legacy/`StateRegistry` coupling).

**Finding 3 — the parameters `FieldCreate` needs are exactly the plain
`VariableSpec` fields `GraphBuilder` already reads today, without
touching `StateItemSpec`/`AspectMap`.** `VariableSpec`
(`superstructure/generic/specs/VariableSpec.F90:66-144`) is a plain
derived type with public, non-encapsulated components — `geom`/`geom_id`,
`typekind`, `ungridded_dims`, `vertical_grid`, `units`, `itemType`. 3c's
own `build_characteristics(var_spec)` (`GraphBuilder.F90`, per 3c
tasks.md 2.5) already reads `units`/`vertical_grid` directly from this
same object, deliberately avoiding `StateItemSpec`/`AspectMap` — this
capability extends that same, already-established pattern to the rest
of the fields `FieldCreate` needs.

**Finding 4 — a `units` mismatch is reachable through more than a plain
`Field`-typed item.** Per domain-expert review: a `Vector` item can
genuinely mismatch on `units` (e.g. a wind-vector pair with differing
unit conventions), and its real-world payload
(`VectorClassAspect%get_payload`, `superstructure/generic/specs/
VectorClassAspect.F90:349-363`) is an `ESMF_FieldBundle`, not a `Field`.
`Bracket`/`VectorBracket` items do not reach a `units` mismatch directly
— `TimeInterpolationTransform` already converts a bracket to a plain
`Field` before units handling would apply. A `State`-payload item
(`StateClassAspect`) is opaque to legacy's `AspectMap` model entirely;
reachability for a `units`-style mismatch on a field or vector nested
inside a `State` is a genuinely recursive question legacy's model never
answers — per domain-expert direction, proper recursive treatment of
`State` contents is the actual long-term motivation for the graph work
generally, not a gap this one capability is expected to close.
**Consequence for this capability:** since there is no general-purpose,
`StateRegistry`-independent factory equivalent to `FieldCreate` for a
`FieldBundle` or a `State` available to reuse, and building one is a
substantially larger undertaking (real `Vector`/`State` support in the
graph, not a small addition), this capability's own materialization
logic is scoped to plain `Field`-typed items only (`VariableSpec%itemType
== MAPL_STATEITEM_FIELD`) — see Non-Goals.

**Finding 5 — a concrete `geom`/`vertical_grid` is generally *not*
present on a `VariableSpec` at all** (confirmed by domain-expert review):
per `VariableSpec%make_GeomAspect`'s own resolution order
(`superstructure/generic/specs/VariableSpec.F90:552-580`, comment at
554-557: "If geom is allocated in var spec then it is prioritized over
the component-wide geom... If not specified either way, then it
indicates that the geom is mirrored and will be determined by a
connection"), the *common* case is that `VariableSpec%geom` is unset and
the concrete value instead comes from the owning `OuterMetaComponent`'s
own component-wide default (`this%geom`/`this%geom_id`/
`this%vertical_grid`, passed as `component_geom`/`component_geom_id`/
`vertical_grid` into `make_StateItemSpec`/`make_aspects` at every
`advertise_variable` call site, `OuterMetaComponent/
advertise_variable.F90:23-24`). `VariableSpec%geom` being explicitly
allocated (the `ASPECT_STATUS_SPECIFIED` branch) is the *exception* —
per domain-expert direction, this is typical of `HistoryCollection`,
where an explicit target geom/vertical grid is often given directly in
the resource file (YAML). The remaining, rarer case — neither
`VariableSpec` nor the component default has a concrete value — is
legacy's true cross-component "mirror" propagation
(`GeomAspect%connect_to_export`, `superstructure/generic/specs/
GeomAspect.F90:261-283`, copies the export's resolved geom into the
import at `connect()` time) — **explicitly out of scope for this
capability** (see Decisions/Non-Goals): replicating that natively would
mean building genuine cross-component geom-propagation in the graph
layer, a substantially larger undertaking than this capability's narrow
job.
`OuterMetaComponent` already exposes the component-wide default as
plain, public, non-`StateRegistry` accessors —
`has_geom()`/`get_geom(rc)`/`get_vertical_grid()`
(`OuterMetaComponent.F90:83-84,150-151`). **Revised during
implementation:** the export's *owning* component is not always the
`this` (`OuterMetaComponent`) argument `run_connect_hook` receives — for
a cross-component (parent/child) `MatchConnection`, the export's own
`VariableSpec` belongs to a named *child*, not the calling `this`
(`resolve_one`'s `export_var_spec` comes from `component_spec_for`'s
same child reach). Reaching that child's own `has_geom()`/`get_geom()`/
`get_vertical_grid()` needed a new, small, additive
`OuterMetaComponent` accessor, `get_child_outer_meta` — mirroring the
already-existing `get_child_component_spec`/`get_child_component_graph`
framework-internal reach (REQ-GB-002) rather than re-deriving the
`get_child()`->`get_gridcomp()`->`get_outer_meta()` pattern inline in
`GraphBuilder.F90`. This is additive only (no existing accessor
changes) - see proposal.md's Impact revision note.

**Finding 6 — `GraphBuilder`'s hooks already run unconditionally in every
real production run today, with no existing on/off switch.**
`graphbuilder_run_advertise_hook`/`run_activate_hook`/`run_connect_hook`
(`superstructure/generic/GraphBuilder.F90:798-849`) are called directly
from `OuterMetaComponent`'s real lifecycle submodules
(`initialize_advertise.F90:54,66`, `initialize_accept_transfer.F90:46`)
— not from test-only code — and `report_if_failed` turns any internal
failure into a log warning rather than gating whether the hook's own
work happens. Through 3c this was safe *by construction*: every
graph-side operation was cheap structural bookkeeping (`NodeId`s, edges,
resource-index entries) with zero real ESMF allocation, so "additive,
non-load-bearing shadow" (3a/3b/3c's own stated posture) was true
automatically, with nothing enforcing it. **This capability is the first
point that performs real ESMF work** (`FieldCreate`) from inside that
same unconditional call chain — without an explicit gate, every
`units`-mismatched, `Field`-typed connection in every real run would
have its field allocated *twice*: once by legacy's own
`connect_sibling`/`extend()` (unaffected, unaware of this capability),
once by this capability — a genuine, silent, unconditional resource cost
in production, not merely a testing concern. Raised by domain-expert
review; addressed in Goals/Decisions below rather than left as an
accepted risk, since (unlike the `Vector`/`State` scope boundary) this
is a cost paid on every affected connection in every run, not a
capability gap that simply fails loudly when hit.

## Goals / Non-Goals

**Goals:**
- Preserve the "additive, non-load-bearing shadow" invariant every prior
  sub-change (3a-3c) has maintained (Finding 6): this capability's real
  `FieldCreate` work MUST be off by default in every real run, and MUST
  require an explicit, deliberate action to enable — never fire simply
  because a component happens to run today.
- For a `units`-mismatched, `Field`-typed export/import pair (3c's only
  characteristic with a real, executing `build_transform`), materialize
  a real, fully allocated `ESMF_Field` for the extension chain's final
  item — entirely within the graph layer, using `mapl_FieldCreate_mod::
  FieldCreate` (Finding 2) parameterized from the export's own
  `VariableSpec` fields (Finding 3) plus the import's required `units`
  value — so `UnitsConverterTransform%compute()`
  (`superstructure/generic/graph/UnitsConverterTransform.F90:99-147`,
  which already assumes `output_item%get_field(rc)` succeeds) can
  actually run.
- Make that materialized item discoverable the same way 3c's own
  reuse-search (REQ-EXT-005) already makes any extension chain
  discoverable — via `ComponentGraph`'s existing resource-index
  mechanism — with no new, separate discoverability mechanism and no
  dependency on `StateRegistry`.
- Zero new runtime dependency on `StateRegistry`, `StateItemSpec`,
  `VariableSpec%make_StateItemSpec`/`make_aspects`, or any `ClassAspect`
  subclass — those are read-only background reference only (Context).

**Non-Goals:**
- A per-component, resource-file-configurable switch (e.g. a
  `gcm.rc`/`ComponentSpec` option, mirroring `activate_all_exports`/
  `activate_all_imports`) — this stage only needs a global, internal
  development/testing gate (Decisions); promoting it to a real,
  user-facing, per-component config option is future work, once this
  capability (and the graph path generally) is closer to production
  readiness.
- Reading or linking to any `StateItemSpec`/extension `StateRegistry`
  already created for the same connection via legacy's imperative path
  — explicitly rejected (Context); this capability's materialization is
  fully independent of whatever legacy does or does not do for the same
  connection.
- Calling `StateRegistry%add_extension`/`add_spec`, or any other
  `StateRegistry`-mutating call, for a graph-created extension — this
  capability never touches `StateRegistry` at all, read or write.
- Real payload materialization for a `Vector`/`FieldBundle`-typed item's
  `units` mismatch, or for anything reachable only through a `State`
  payload (Finding 4) — no `StateRegistry`-independent factory exists
  for either today; both are explicit, distinguishable failures from
  this capability (see Decisions), not silently-produced wrong payloads.
  Real support for either is a substantially larger follow-up capability
  in its own right, not a small addition to this one.
- Real (executing) visibility support for any characteristic besides
  `units` — unchanged 3c scope boundary; any other mismatch still fails
  explicitly before reaching this capability's materialization step.
- Any bridge into a real `OuterComponent`'s ESMF import/export state
  (literal `ESMF_StateAdd`) — "visible" for this capability means "has a
  real payload, discoverable via `ComponentGraph`'s own existing query/
  resource-index API," matching 3c's own REQ-EXT-005 precedent of
  restating legacy behavior in graph-native terms rather than calling
  into legacy. Making a graph-native item visible to a real running
  ESMF component is separate, later integration work (once the graph is
  ready to take over the responsibility `StateRegistry`'s
  `add_to_states` currently has) — not attempted here.
- Any change to `TransformGraphNode`/`DependencyNetwork`/`ComponentGraph`
  public APIs from Phase 1–2. `GraphStateItem%set(field)`/`get_field`
  and `StateItemNode%set_payload` are already public
  (`GraphStateItem.F90:69-77`, `StateItemNode.F90:26-27,50-62`) and
  sufficient as-is.

## Decisions

**Materialize a real `Field` natively via `mapl_FieldCreate_mod::
FieldCreate`; never read or link to anything `StateRegistry` creates.**
When `GraphBuilder`'s chain-building logic (3c, `ExtensionResolution.F90`)
builds (or reuses) an extension for a `units`-mismatched, `Field`-typed
pair, this capability calls `FieldCreate(geom=..., typekind=...,
ungridded_dims=..., vgrid=..., units=<import's required units>, rc=...)`
(`infrastructure/field/FieldCreate.F90`) with `typekind`/`ungridded_dims`
taken from the export's own `VariableSpec` (Finding 3), `units` taken
from the import's own `VariableSpec` (the resolved goal value), and
`geom`/`vgrid` resolved via the order established by Decision "Geom/vgrid
resolution order" below — producing a real, fully allocated `ESMF_Field`
with no `StateRegistry`/`StateItemSpec`/`AspectMap` involvement anywhere
in the call chain. The resulting field is assigned to the graph's
extension `StateItemNode`'s payload via the existing public
`GraphStateItem%set(field)` / `StateItemNode%set_payload` calls — no new
type API. *Rejected alternative (this design's first draft):* find and
read the `StateItemSpec` legacy's own `connect_sibling`/`extend()`
already created for the same import, via `StateRegistry`'s existing
`get_extension_family`/`find_closest_spec`/`get_payload` methods.
Rejected per explicit reviewer direction: `StateRegistry` is slated for
removal, so any solution — even read-only — that depends on it at
runtime is a dead end that must be redone later; this capability's own
logic must replicate what is needed natively in the graph layer instead.

**Geom/vgrid resolution order: `VariableSpec`'s own value, else the
owning `OuterMetaComponent`'s component-wide default, else fail
explicitly — no cross-component mirror propagation.** Confirmed by
domain-expert direction: (1) if the export's `VariableSpec%geom`/
`vertical_grid` is explicitly allocated, use it directly (the
`HistoryCollection`-style explicit-override case, Finding 5); (2)
otherwise, use the owning `OuterMetaComponent`'s own component-wide
default via its existing public `has_geom()`/`get_geom(rc)`/
`get_vertical_grid()` accessors (Finding 5) — the same fallback
`advertise_variable.F90` already applies to every `VariableSpec`, and the
common case per domain-expert direction; (3) if neither source has a
concrete value, this capability's materialization step fails explicitly
and distinguishably, the same "fails loudly" posture as everything else
this change scopes out, rather than attempting to replicate legacy's true
cross-component mirror propagation (`GeomAspect%connect_to_export`,
Finding 5) — that remains a distinct, substantially larger follow-up
capability (graph-native geom/vgrid propagation across component
boundaries), not attempted here. *Rejected alternative:* also replicate
cross-component mirror propagation now, so this capability has a
correct payload for every configuration legacy already handles. Rejected
per explicit direction: disproportionate scope for a capability whose
job is narrowly "give `UnitsConverterTransform` a real field," and
already-decided as a distinct future capability rather than folded into
this one.

**Scope limited to `Field`-typed items; anything else fails explicitly.**
`VariableSpec%itemType` (`VariableSpec.F90:68`, default
`MAPL_STATEITEM_FIELD`) tells this capability whether the export/import
pair is a plain field. If it is not (`Vector`, `Bracket`,
`VectorBracket`, `Service`, `Expression`, `State`, ...), this
capability's materialization step reports an explicit, distinguishable
failure identifying the unsupported item class — extending, not
replacing, 3c's existing "characteristic mismatch with no registered
provider fails loudly" posture to also cover "item class this capability
cannot yet materialize a payload for." *Rejected alternative:* attempt a
`Vector`/bundle materialization too, e.g. building an ad hoc
`ESMF_FieldBundleCreate` call. Rejected: no established,
`StateRegistry`-independent factory for this exists yet (Finding 4);
inventing one is a real capability in its own right (real `Vector`
support in the graph), disproportionate to this change's narrow job of
giving `UnitsConverterTransform` (`Field`-only) a real field to compute
into.

**Discoverability stays exactly what 3c already built (REQ-EXT-005);
no new mechanism.** 3c's reuse-search already keys a resolved extension
`NodeId` into `ComponentGraph`'s resource index
(`"EXTCHAIN:" // export_node_id%to_string() // ...`, 3c design.md
Decisions). Once this capability gives that `NodeId`'s `StateItemNode` a
real payload, it is already exactly as discoverable as it was
structurally before — this capability adds a real payload behind an
already-discoverable node, not a new discoverability path.

**Gate real materialization behind a simple, global, default-off,
internal switch — not a `VariableSpec` variant, and not a per-component
config option yet.** A single module-level flag (e.g. in
`ExtensionResolution.F90` or a small adjacent module), `logical, save ::
materialize_extensions = .false.` with a public getter and a public
setter (`set_materialize_extensions(enabled)`), guards only the
`FieldCreate` call this capability adds (task group 1) — chain
*structure* (`NodeId`s, edges, resource-index entries, already
established by 3c) is unaffected and continues to run unconditionally
exactly as it does today. When the flag is `.false.` (the default, and
the state of every real production run unless something explicitly
calls the setter), an extension item keeps exactly the unallocated
placeholder payload 3c already left it with — no behavior change from
today at all. Only this capability's own pFUnit tests (and any future
equivalence-fixture harness) call the setter; no production
initialization code (`initialize_advertise.F90`, `initialize_accept_
transfer.F90`, or anywhere in `OuterMetaComponent`) is touched or calls
it. *Rejected alternative 1:* a `VariableSpec` variant, or a per-item
conditional threaded through `VariableSpec` itself. Rejected:
`VariableSpec` is plain data read identically by both the legacy and
graph paths; nothing about *it* needs to vary. The actual seam needing
control is "does the graph's extension-resolution step perform real ESMF
work" — a `GraphBuilder`/`ExtensionResolution`-level runtime decision,
not a data-representation concern. *Rejected alternative 2:* a
`component_spec%misc`-style per-component config flag (mirroring
`activate_all_exports`/`activate_all_imports`) from the start. Rejected
as premature for this stage: that would present this as a real,
supported per-component user choice today, which it is not — a global
internal switch is sufficient while this capability is
development/testing-only, and can be upgraded to a config-driven,
per-component flag later if/when the graph path is closer to production
readiness (see Non-Goals).

**No per-connection correlation with legacy's own connect step is
needed** (unlike this design's first draft) — since materialization no
longer reads anything legacy creates, `GraphBuilder`'s existing
whole-phase connect hook (`gb%run_connect_hook(this)`,
`initialize_accept_transfer.F90:46`, unchanged since 3b/3c) is
sufficient; no change to that call site or to `Connection`'s interface.

## Risks / Trade-offs

- **[Risk] A configuration whose geom/vgrid resolution genuinely requires
  cross-component mirror propagation (Finding 5's rarer case) gets an
  explicit failure from this capability instead of a real payload** —
  same accepted-narrowing posture as the `Vector`/`State` scope
  boundary below, but for geom/vgrid resolution specifically.
  **Mitigation:** explicit, distinguishable failure (Decisions), not a
  silent skip or a wrongly-shaped field; real cross-component
  propagation is a distinct, future graph-native capability.
- **[Risk] Reading the export's owning component's component-wide
  default geom requires `GraphBuilder`'s connect-hook code to call
  `has_geom()`/`get_geom()`/`get_vertical_grid()`, and the owning
  component is sometimes a named child, not the `this`
  (`OuterMetaComponent`) argument the hook directly receives** — a
  slightly broader read surface than pure `VariableSpec` reading (3c's
  own established pattern), and (revised during implementation) a small
  new `OuterMetaComponent` accessor (`get_child_outer_meta`) rather than
  purely existing ones. **Mitigation:** `has_geom()`/`get_geom()`/
  `get_vertical_grid()` themselves are already-public, already-existing
  accessors (`OuterMetaComponent.F90:83-84,150-151`) used for exactly
  this purpose elsewhere (`advertise_variable.F90`); `get_child_outer_meta`
  mirrors the already-existing `get_child_component_spec`/
  `get_child_component_graph` framework-internal reach (REQ-GB-002) -
  additive only, not a `StateRegistry` dependency, and no change to any
  existing `OuterMetaComponent` accessor.
- **[Risk] `FieldCreate`'s plain-argument construction does not
  reproduce every metadata detail legacy's aspect-based `create()`/
  `allocate()` sets** (e.g. `attributes` aspect, `restart_mode`,
  `fill_value` — `FieldClassAspect.F90`'s own `create`/`allocate` set
  more than geom/typekind/units). **Accepted narrowing:** this
  capability's job is "give `UnitsConverterTransform` a real field to
  compute a unit conversion into," not full metadata parity with a
  legacy-created extension; documented explicitly rather than silently
  assumed equivalent.
- **[Risk] `Vector`/`FieldBundle` and `State`-payload `units` mismatches
  get no payload at all under this change** (Non-Goals) — a real
  configuration exercising one of those today would still fail this
  connection's resolution, same as if no provider were registered at
  all. **Mitigation:** explicit, distinguishable failure (Decisions),
  never a silent skip or a fabricated payload; same accepted-narrowing
  posture 3c itself used for "real providers beyond `units`."
- **[Risk] A global, module-level flag is coarse — all-or-nothing across
  every component in a run, not a per-component choice.**
  **Mitigation:** acceptable for this transitional, development/testing
  stage (Non-Goals); a finer-grained, per-component config option can be
  added later if/when this graph path approaches production readiness —
  not needed for this capability's own pFUnit/equivalence-fixture use.
- **[Risk] Module-level mutable state (the gate flag) can leak across
  pFUnit tests if not reset** — a test that enables materialization and
  fails before disabling it again could leave the flag on for later
  tests in the same process. **Mitigation:** this capability's own tests
  must reset the flag in `setUp`/`tearDown` (or scope enabling it to the
  narrowest block possible), the same discipline any global-mutable-test
  state requires; not a new category of risk this codebase doesn't
  already manage elsewhere.
- **[Risk] This capability's materialized field may not exactly match
  what legacy's own `connect_sibling`/`extend()` produces for the same
  connection** (no shared code path anymore) — the 3c/3b equivalence-test
  posture ("same reuse/no-op decisions as the existing algorithm") still
  applies to *whether* an extension is created/reused, but this
  capability does not promise the *materialized field itself* is
  identical to legacy's, only that it is real, correctly-shaped, and
  usable by `UnitsConverterTransform`. **Mitigation:** documented
  explicitly; equivalence tests should assert *decision* parity (same
  reuse/creation choices) and this capability's own tests should assert
  *correctness* of the graph-native field (right geom/typekind/units,
  successful conversion) — not byte-for-byte parity with a legacy field
  that no longer exists in this code path.

## Open Questions

None. Geom/vgrid resolution order was resolved during planning (Finding
5, Decisions) rather than left open.
