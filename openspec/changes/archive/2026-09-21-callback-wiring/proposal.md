## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.3 identifies Phase 4d
as the next Phase 4 sub-change: `GraphBuilder` wildcard/regex callback
wiring (`15-callbacks.md` §15.9-15.10) — wildcard/regex expansion against
the flattened qualified-export namespace (REQ-CB-016), per-method
`DependencyNetwork`s for get/put argument flow (REQ-CB-018), and the
invoke-once-after-all-args-ready discipline (REQ-CB-020). Phase 4c
(`callback-data-model-registry`, landed) already provides `CallbackInterface`/
`CallbackInterfaceRegistry`/`CallbackStateBinding`, but deliberately
scoped them to caller-supplied `NodeId`s with no `GraphBuilder` wiring
(its own explicit deferral). Without this change, a declared
`CallbackInterface` has no path from a real advertised component
hierarchy to a real, validated `CallbackStateBinding`, and an Invoker
component has no way to receive a flattened, wildcard-matched collection
of Handler callback states (`15-callbacks.md`'s own `DYN` tracer-callback
example) — the callback data model exists but is inert.

This is greenfield `GraphBuilder` work on top of the completed Phase
1-3 foundation, Phase 4a's `MethodGraphNode`/`AccessSpec`/
`StateMethodInvocation`, Phase 4c's callback data model, and the landed
`composite-state-spec` (member-`NodeId`-addressable composite states,
required for `CallbackStateBinding`'s argument members to resolve to
real advertised State members rather than caller-supplied stand-ins).

**Revised approach, superseding this change's first draft.** The first
draft introduced a new `CallbackConnection` `Connection` subtype
carrying its own regex source pattern and expected `CallbackInterfaceId`
directly. Design review rejected that: `SimpleConnection`/
`MatchConnection` exist specifically so a component author only ever
declares *where* two items are (source/destination locations, with
`MatchConnection`'s own `VirtualConnectionPt` already regex-capable for
exactly this "match a pattern" job, `connect_all`'s own established
`short_name='^.*$'` idiom) — a callback-specific `Connection` subtype
would be a second, parallel "special connection" idiom the framework
does not otherwise have, duplicating machinery (regex matching) that
`VirtualConnectionPt%matches()` already provides. The framework's actual
precedent for "this item needs special handling beyond plain source/
destination matching" is a marker on the *item* (`VariableSpec%itemType
== MAPL_STATEITEM_SERVICE` + `ServiceClassAspect`), not on the
connection. This change now follows that precedent: an ordinary ~~
`MatchConnection`~~ (source pattern, destination item, both already
expressible today) declares the wiring; the destination `VariableSpec`
itself declares that it expects to satisfy a `CallbackInterface`, and
`GraphBuilder`'s existing `MatchConnection` resolution branches on that
declaration. See design.md Decision 0 for the full rationale and
rejected alternative.

## What Changes

- **`VariableSpec` gains a `callback_interface_id` field**
  (`superstructure/generic/specs/VariableSpec.F90`, REQ-CB-017): marks an
  import as a callback consumer expecting to satisfy the given
  `CallbackInterfaceId` (default: invalid/unset, `is_valid() == .false.`,
  meaning "an ordinary, non-callback item" — zero behavior change for
  every existing declaration). No new `Connection` subtype, no new
  `ComponentSpec` field — the connection wiring itself stays an ordinary
  `MatchConnection`/`SimpleConnection`, exactly `15-callbacks.md`'s own
  REQ-CB-017 example ("source: PHYS export virtual connection point,
  pattern = tracers or */tracers; destination: DYN import item, name =
  tracers" — already an ordinary connection shape).
- **Flattened qualified-export namespace query** (REQ-CB-013/014): a
  `GraphBuilder`-level capability that, rooted at a named component
  (resolved the same way ordinary connection resolution already resolves
  `<self>`/a named child, `component_for`/`component_spec_for`), walks
  that component's own descendant `ComponentSpec`s recursively (via
  `get_child_component_spec`/`get_child_outer_meta`, generalized from
  ordinary resolution's single-hop child lookup to arbitrary depth) and
  builds `component/item`-qualified export names — reusing the existing
  `comp_name // '/' // short_name` convention
  `VirtualConnectionPt%get_full_name()`
  (`superstructure/generic/connection/VirtualConnectionPt.F90`) and the
  existing hierarchy-bubbling shape
  `StateRegistry_Propagation_smod.F90`'s `propagate_exports_virtual_pt`
  already established for History diagnostics — not a new, independently
  invented naming or bubbling scheme (REQ-CB-014's explicit requirement).
- **Pattern matching reuses `VirtualConnectionPt%matches()` directly**
  (REQ-CB-016), the same POSIX-regex-backed method `MatchConnection`
  already calls for its own ordinary wildcard resolution
  (`VirtualConnectionPt.F90`, itself backed by `utils/regex`) — each
  flattened namespace entry is compared via a synthetic, `comp_name`-less
  `VirtualConnectionPt(EXPORT, qualified_name)` against the connection's
  own declared source `VirtualConnectionPt`. No new regex helper, no new
  dependency on `utils/regex` directly: this change calls the one
  existing method every wildcard connection already goes through.
- **`GraphBuilder`'s existing `MatchConnection` resolution branches on
  the destination's declared `callback_interface_id`** (REQ-CB-016):
  inside the same `resolve_match_connection`/
  `check_match_connection_unsatisfied` dispatch ordinary connections
  already use, a matched destination import whose `VariableSpec`
  declares a `callback_interface_id` is resolved by expanding the
  connection's source pattern against the flattened qualified-export
  namespace (rooted at the connection's declared source component),
  validating every match implements the expected `CallbackInterface`
  (via Phase 4c's `CallbackInterfaceRegistry`/`CallbackStateBinding`),
  and materializing a flat callback collection for the Invoker: a
  composite `GraphStateItem` (reusing `composite-state-spec`'s existing
  `state_members` map, built directly rather than through
  `CompositeStateMaterialization`'s `VariableSpec`-tree walk — see
  design.md Decision 5) whose members are the matched callback states'
  `NodeId`s. An ordinary (non-callback) destination is completely
  unaffected — this is an additive branch, not a change to existing
  exact-match behavior.
- **New `CallbackMethodBinding`** (REQ-CB-019): the method-level binding
  record — a `MethodGraphNode`'s `NodeId`, the get/put
  `DependencyNetworkId`s, and per-direction argument source/target
  `NodeId` maps — completing `15-callbacks.md` §15.2's metadata-concept
  list (`CallbackMethodAttachment` was already resolved by 4c as "reuse
  `StateMethodInvocation`, no new type"; this change adds the one
  concept 4c explicitly left to 4d).
- **Per-method `DependencyNetwork`s** (REQ-CB-018): for an `INOUT`
  callback argument, a `get` network (provider representation ->
  transforms -> callback representation) and a `put` network (callback
  representation -> transforms -> provider representation), each created
  via `ComponentGraph%create_network()` (already provides multiple named
  networks per graph) and each independently acyclic (REQ-DEP-004),
  reusing Phase 2's `TransformGraphNode`/port-binding machinery for any
  interposed transform exactly like ordinary connection resolution
  already does for mismatched characteristics (3c's
  `find_or_build_extension_chain`).
- **Invoke-once-after-all-args-ready discipline** (REQ-CB-020): a new
  `invoke_callback_method` procedure mirroring Phase 4a/4b's own
  `invoke_on_default_network` (`MethodInvocation.F90`, REQ-REV-011's
  pull-before/invoke/advance-after discipline) but parameterized by the
  get/put networks a `CallbackMethodBinding` names instead of hardcoding
  the default network — ensures every get-network argument is current
  (`ComponentGraph%update()`), invokes the bound `MethodGraphNode`
  exactly once, then advances every put-network argument only on
  success. Never registers the callback method node into demand-driven
  update's own dispatch (`method-graph-node`'s "never scheduled by
  demand-driven update" requirement is preserved).

**Explicit deferrals** (later Phase 4 sub-changes' jobs, or explicitly
open questions, per `20-implementation-roadmap.md` §20.4.3 and
`15-callbacks.md`):

- **No `ComponentSpecParser`/YAML or `SetServices`-convenience entry
  point for `callback_interface_id`.** A real component's YAML config
  or Fortran `SetServices` code has no way today to actually declare an
  import as a callback consumer - `callback_interface_id` is set only
  by direct field assignment on an already-constructed `VariableSpec`
  (e.g. test fixtures bypassing `ComponentSpecParser` entirely, the
  same established convention `Test_GraphBuilder.pf`'s own header
  already documents for every other field). This mirrors
  `composite-state-spec`'s own identical, explicitly-stated deferral
  for `members` ("a config-driven builder... is not built here"); a
  `make_VariableSpec()` keyword and/or a `ComponentSpecParser` YAML key
  are natural, anticipated future entry points, not built by this
  change.
- Physical ESMF placement convention (import vs. export State) for
  callback States (§15.8, `[OPEN]`, REQ-CB-011/012) — not addressed by
  this change; the flattened qualified-export namespace and the
  materialized callback collection are both agnostic to where in the
  graph a matched callback state's `NodeId` ultimately sits.
- Handler/Invoker terminology finalization (§15.5, `[OPEN]`, Q6) — used
  only in comments/docs here, not settled by this change.
- Real `ESMF_MethodAdd`/`ESMF_MethodExecute`-backed `StateMethodInvoker`
  implementation — Phase 4c's own deferral, still not built here; this
  change continues to exercise `StateMethodInvocation` with the existing
  synthetic test-double invoker in tests, and (for real configurations)
  leaves attaching a real invoker to a `GridComp`'s callback method as a
  separate, later concern.
- Recursive/member-level connection resolution for non-callback
  composite items (`composite-state-spec`'s own explicit deferral) —
  unaffected by this change; callback wiring here only ever resolves the
  callback state's *own* member `NodeId`s (already declared via
  `declare_member`), never an ordinary composite-to-composite match.
- Geometry/route-handle work (Phase 4e-4g) — unrelated, not touched.

## Capabilities

### New Capabilities
- `graph/callback-wiring`: the flattened qualified-export namespace
  query, `GraphBuilder`'s callback-branch resolution (pattern matching
  via `VirtualConnectionPt%matches()`, interface conformance,
  callback-collection materialization), `CallbackMethodBinding`,
  per-method get/put `DependencyNetwork` construction, and the
  invoke-once-after-all-args-ready discipline.

### Modified Capabilities
- `graph/graph-builder`: gains callback-specific wiring behavior as a
  branch inside its existing `MatchConnection` resolution, triggered by
  the destination `VariableSpec`'s own declared `callback_interface_id`
  — an externally observable addition to what `GraphBuilder` does, not
  merely an internal implementation change, so it is called out as a
  modified capability. Every existing `graph-builder` requirement and
  scenario for a non-callback destination is unchanged (the branch is
  additive and gated entirely on a field that defaults to unset).

## Impact

- **Affected/new code**:
  - `superstructure/generic/specs/VariableSpec.F90`: new
    `callback_interface_id : type(CallbackInterfaceId)` field (default
    invalid), additive only — no existing field, method, or aspect
    construction path changes.
  - `superstructure/generic/graph/CallbackMethodBinding.F90`
    (`mapl_CallbackMethodBinding_mod`): new type — `MethodGraphNode`
    `NodeId`, get/put `DependencyNetworkId`s, per-direction argument
    `NodeId` maps.
  - `superstructure/generic/GraphBuilder.F90`: new procedures for
    flattened qualified-export namespace construction, the callback
    branch inside `resolve_match_connection`/
    `check_match_connection_unsatisfied`, callback-collection
    materialization, `CallbackStateBinding` construction from real
    advertised composite members, per-method network construction, and
    `invoke_callback_method`. No change to the exact-match code path any
    existing (non-callback) connection already exercises.
  - Corresponding `CMakeLists.txt` entries.
- **Dependencies**: no new external dependency — `VirtualConnectionPt`
  (already a `GraphBuilder.F90` dependency) supplies pattern matching;
  no change to `ComponentGraph`, `DependencyNetwork`, `MethodGraphNode`,
  or the Phase 4c callback data model types themselves, all consumed
  unchanged.
- **Tests**: new unit/synthetic-graph tests reproducing
  `15-callbacks.md`'s own `DYN` tracer-callback example end-to-end
  (multiple descendant components each advertising a `PassiveTracer`
  callback state, a `DYN`-like Invoker declaring a `tracers` import with
  `callback_interface_id` set and a `MatchConnection` whose source
  pattern is `*/tracers`-style, `GraphBuilder` resolving it into a
  materialized callback collection with the correct members); confirming
  a destination with no `callback_interface_id` set is completely
  unaffected (ordinary exact-match behavior, byte-for-byte unchanged);
  per-method get/put network construction and acyclicity; invoke-once-
  after-all-ready behavior (including a not-yet-ready argument correctly
  deferring invocation, and a successful invocation never firing twice
  for one logical call). Where a real `OuterMetaComponent`/
  `ESMF_GridComp` hierarchy is needed to exercise the descendant-
  traversal namespace construction (mirroring 3b's own real-hierarchy
  requirement for ordinary connections), reuse the existing component-
  hierarchy test fixtures already used by
  `graphbuilder-advertising-connections`/`extension-reuse`'s own test
  suites rather than inventing new ones.
- **Out of scope**: everything under "Explicit deferrals" above.
