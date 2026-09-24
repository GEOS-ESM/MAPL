## Context

Phase 4a (`method-graph-node`, archived) shipped `MethodGraphNode`,
`MethodInvocationAdapter`, and two concrete adapters
(`GridCompMethodInvocation`, `StateMethodInvocation`), each delegating
to a small injected abstract interface
(`GridCompPhaseInvoker`/`StateMethodInvoker`) with only synthetic
test-double implementations. `GridCompMethodInvocation` already carries
a `driver_key : character` field (REQ-MTH-009's stable-identifier shape)
and a `phase_name : character` field.

Phase 4a's own design.md flagged, in its own Risks section (found during
that change's review, not fixed there — explicitly left for this
sub-change), that `phase_name` does not in fact match the real call this
adapter must eventually make: `GriddedComponentDriver%run`/`initialize`/
`finalize` (`superstructure/component/GriddedComponentDriver.F90`) take
`phase_idx : integer`, never a name, and take no `arguments`/`bindings`
parameters at all. The existing precedent for "invoke a named phase on a
named driver" is `OuterMetaComponent%run_child_by_name`
(`superstructure/generic/OuterMetaComponent/run_child_by_name.F90`):
it resolves a driver via `this%get_child(child_name)` (an ordinary
lookup on the *parent `OuterMetaComponent`'s own child map*, a
`GriddedComponentDriverMap` named `children` — not any
`ComponentGraph`/`GraphBuilder`-level registry), translates a
`phase_name` to `phase_idx` via `get_phase_index(child_meta%get_phases(...),
phase_name=...)` (`MethodPhasesMap.F90`), and calls
`child%run(phase_idx=phase_idx, ...)`. `OuterMetaComponent` already owns
exactly the driver shape REQ-MTH-008 specifies: `user_gc_driver` (its
own driver) and `children : GriddedComponentDriverMap` (one per child),
both declared in `superstructure/generic/OuterMetaComponent.F90` before
any graph-native code existed.

Separately, `GraphBuilder.F90` (Phase 3b/3c) already has graph-native
hooks for two of REQ-MTH-011 step (c)'s three phases:
`graphbuilder_run_advertise_hook`/`graphbuilder_run_activate_hook`
(called from `initialize_advertise.F90`, GENERIC_INIT_ADVERTISE) and
`graphbuilder_run_connect_hook` (called from
`initialize_accept_transfer.F90`, GENERIC_INIT_ACCEPT_TRANSFER — real
proxies/edges, extension materialization gated by
`materialize_extensions_enabled()`, then `freeze()`). There are **no**
graph-native hooks at all for `GENERIC_INIT_REALIZE_PROVIDED`/
`GENERIC_INIT_REALIZE_ACCEPTED` today — those phases
(`initialize_realize_provided.F90`/`initialize_realize_accepted.F90`)
remain entirely legacy `StateRegistry`-driven
(`this%registry%allocate()`), untouched by `GraphBuilder`.

Critically, `enums/GenericPhases.F90`'s `GENERIC_INIT_PHASE_SEQUENCE`
shows that "cycle the three phases until convergence" is not, in the
real MAPL cap driver, a dynamic loop at all: it is a **fixed, hard-coded
two-pass schedule** — `REALIZE_PROVIDED`/`ACCEPT_TRANSFER`/
`REALIZE_ACCEPTED` are each registered as ESMF phases and appear
literally twice in the sequence array ("Second pass" comment inline).
`graphbuilder_run_connect_hook`'s own header comment already documents
handling this: ESMF can invoke `GENERIC_INIT_ACCEPT_TRANSFER` more than
once per real `Initialize` call, so `graphbuilder_resolve_connections`
guards at the graph level — once `graph%is_frozen()`, a repeat call is a
documented no-op (`GraphBuilder.F90:469-473`) rather than an attempt to
mutate a frozen graph. `check_unsatisfied_imports`
(`GraphBuilder.F90:319-352`) exists and is called once, but only from
`graphbuilder_run_activate_hook` at GENERIC_INIT_ADVERTISE time — i.e.
*before* either real ACCEPT_TRANSFER pass has run — and its result is
only ever logged as a `lgr%warning`, never escalated to an error.

## Goals / Non-Goals

**Goals:**

- Supply a real, `GriddedComponentDriver`-backed `GridCompPhaseInvoker`
  and the real `driver_key -> driver` resolution mechanism REQ-MTH-009
  requires, fixing the `phase_name`/`phase_idx` and
  `arguments`/`bindings`-unused mismatches Phase 4a's own design.md
  flagged, while keeping `superstructure/generic/graph/`'s existing
  one-directional dependency discipline (that tier does not depend on
  `OuterMetaComponent`) intact.
- Supply a graph-neutral, unit-testable implementation of REQ-MTH-003a's
  trigger/advance discipline, callable by the `OuterMetaComponent` layer
  per REQ-REV-011a, without requiring a real ESMF component to exercise
  it.
- Bound REQ-MTH-011 step (c)'s convergence question to an explicit,
  concrete decision *for this sub-change's own scope* (below), matching
  the schedule MAPL's cap driver actually uses today
  (`GENERIC_INIT_PHASE_SEQUENCE`'s fixed two-pass shape), rather than
  inventing a new dynamic loop the rest of the codebase does not use
  yet. This is explicitly an interim scoping decision, not a claim that
  a fixed two-pass schedule is sufficient in general — see Open
  Questions below for why, and for what must eventually replace it.
- Close the specific "warning instead of hard error, too early to be
  meaningful" gap in `check_unsatisfied_imports`'s current only call
  site.

**Non-Goals:**

- **`GraphBuilder` constructing a real `MethodGraphNode` per registered
  GridComp phase.** Ordinary GridComp phases have no fixed named-argument
  signature the way an attached-State callback method does (REQ-CB-004);
  what a phase's `ArgumentSpecMap`/bindings should even contain (every
  advertised item? none, with REQ-MTH-004's "conceptual" argument-set
  view left purely conceptual?) is an open architectural question this
  sub-change does not resolve, and resolving it here would silently
  expand "driver integration" into a second, unbounded design problem —
  exactly what the roadmap's own 3a/3b/3c/3b2/3c2 sub-sequencing exists
  to prevent (§20.4.1). This is flagged as a real gap in Risks below,
  not quietly assumed solved.
- **Graph-native hooks for `GENERIC_INIT_REALIZE_PROVIDED`/
  `GENERIC_INIT_REALIZE_ACCEPTED`.** These phases have no `GraphBuilder`
  involvement today (Context above) and adding it is a `GraphBuilder`
  materialization concern, not a driver-integration concern; out of
  scope here.
- **A dynamic, counter-based convergence loop keyed on
  `CharacteristicStatus` reaching `SPECIFIED`.** `CharacteristicStatus`
  remains `[SPECULATIVE]` (`18-state-item-characteristics.md`, Phase 5)
  and is not introduced by this change.
- **Callback methods, `StateMethodInvoker`'s real
  `ESMF_MethodExecute`-backed implementation, wildcard/regex wiring** —
  Phase 4c/4d, unaffected by anything here.

## Decisions

**A new `DriverResolver` abstraction decouples the real
`GridCompPhaseInvoker` from `OuterMetaComponent`.** Rather than putting
the real `GridCompPhaseInvoker` implementation inside
`mapl_OuterMetaComponent_mod` itself (which would work, but would pull
`OuterMetaComponent`'s full module into `superstructure/generic/graph/`'s
otherwise `OuterComponent`-independent tier), this change adds one small
abstract type, `DriverResolver` (one deferred function
`resolve(this, driver_key, rc) result(driver)` returning
`class(GriddedComponentDriver), pointer`), declared alongside
`GridCompPhaseInvoker`/`StateMethodInvoker`
(`MethodInvocationAdapter.F90`, which already declares `GriddedComponentDriver`-
adjacent injection points and is a natural single home for this one
too). The real `GridCompPhaseInvoker` concrete type (new file,
`superstructure/generic/graph/`) holds a `class(DriverResolver),
allocatable` and does exactly: resolve the driver, translate
`phase_idx` (see next decision), call `driver%run`/`initialize`/
`finalize` per which ESMF method the node models, propagate `rc`.
Nothing else — satisfies REQ-MTH-003 by construction, same posture Phase
4a used for the adapter/invoker split itself.

*Alternative considered:* implement `GridCompPhaseInvoker` directly
inside `mapl_OuterMetaComponent_mod` (mirroring how `run_self()` is
already implemented as an `OuterMetaComponent`-side submodule
specifically to avoid a circular dependency the *other* direction,
per `GriddedComponentDriver.F90`'s own comment). Rejected: it would make
`superstructure/generic/graph/` (today independent of
`OuterMetaComponent`) depend on it transitively through
`GridCompMethodInvocation`'s own construction path, which every prior
Phase 1-4a sub-change has deliberately avoided (REQ-CG-002-style
layering). The `DriverResolver` seam keeps that boundary intact: the
concrete resolver implementation (next decision) is the only new code
that touches `OuterMetaComponent` internals.

**The concrete `DriverResolver` implementation lives in
`superstructure/generic/`, alongside `GraphBuilder.F90`** (the existing
integration tier that already depends on both
`mapl_OuterMetaComponent_mod` and the graph types) — new file, e.g.
`OuterMetaComponentDriverResolver.F90`. It holds
`class(OuterMetaComponent), pointer :: owner` and resolves a
`driver_key` as follows, reusing the existing `"<self>"` sentinel
convention `GraphBuilder.F90` already established
(`SELF_COMPONENT_NAME = '<self>'`, `GraphBuilder.F90:153`) rather than
inventing a second one:

- `driver_key == '<self>'` (or empty — both accepted, `'<self>'`
  canonical): return `owner%get_user_gc_driver()` — already an existing
  accessor returning `type(GriddedComponentDriver), pointer` with no
  copy (`get_user_gc_driver.F90`).
- otherwise: look up `owner%children` (the existing
  `GriddedComponentDriverMap`) directly by pointer, the same underlying
  lookup `get_child_by_name.F90` performs
  (`this%children%at(child_name)`) — but return that pointer directly
  instead of `get_child_by_name`'s own value-copy result. This is a
  strictly narrower promise than REQ-MTH-009 requires ("never a raw
  pointer to the driver object" applies to what `MethodGraphNode`/the
  adapter *retains* across calls — the pointer here is used
  transiently, inside one `resolve()` call, then handed to
  `GridCompPhaseInvoker`, which itself never stores it past the one
  `run`/`initialize`/`finalize` call it makes with it) and avoids
  `get_child_by_name`'s own documented "deep copy of shallow ESMF
  objects - be careful using result" caveat (`get_child_by_name.F90:9`)
  entirely, for this call site.
- an unresolvable key (neither `<self>` nor a known child name) is an
  explicit `_ASSERT` failure, not a silent null return.

*Alternative considered:* resolve exclusively through
`ComponentGraph`'s existing `resource_index` (`add_resource_index`/
`get_resource_index`, `key -> NodeId`), matching REQ-MTH-009's literal
wording ("resolvable within the owning `ComponentGraph`/`GraphBuilder`
context"). Rejected: `resource_index` maps to `NodeId`s (graph-native
identities), not to driver objects — drivers are not, and per
REQ-MTH-009 must not become, graph nodes or graph-visible values, so
routing driver lookup through the resource index would mean either
inventing a driver-`NodeId` fiction (contradicting REQ-MTH-005/012's
core "the component is not a graph node" stance) or a second,
`NodeId`-keyed table doing nothing `OuterMetaComponent%children`
does not already do. `OuterMetaComponent` — which already owns both the
drivers (REQ-MTH-008) and the `local_graph` (`07-component-graph.md`
REQ-CG-002/004) — is the resolution context REQ-MTH-009 actually needs;
the spec wording is satisfied because `OuterMetaComponent` *is* "the
owning component" the driver key is resolved within, one layer above
where `ComponentGraph` itself lives, not inside `ComponentGraph`'s own
data.

**`GridCompMethodInvocation` gains `phase_idx : integer` in place of
`phase_name : character`.** Directly fixes the mismatch Phase 4a's
design.md flagged: `GriddedComponentDriver%run`/`initialize`/`finalize`
take `phase_idx`, never a name. `get_phase_name()` becomes
`get_phase_idx()`; `new_GridCompMethodInvocation`'s second positional
argument changes accordingly. Translating a human-readable phase name to
`phase_idx` (via `MethodPhasesMap%get_phase_index`, as
`run_child_by_name.F90` already does) is the job of whatever constructs
a `GridCompMethodInvocation` for a real phase — deferred, per Non-Goals
above, to whichever later sub-change actually builds real
`MethodGraphNode`s for GridComp phases. This sub-change's own tests
supply `phase_idx` directly.

*Alternative considered:* keep `phase_name`, translate it to
`phase_idx` inside the new real `GridCompPhaseInvoker` itself (using
`DriverResolver`-obtained driver plus a second lookup for
`ESMF_Method_Flag`/`get_phases()`). Rejected: which `ESMF_Method_Flag`
(`INITIALIZE` vs `RUN` vs `FINALIZE`) applies is not otherwise available
to the invoker at all (`GriddedComponentDriver%run`/`initialize`/
`finalize` are already three separate calls, so the invoker already
knows which one it is about to make) — but `MethodPhasesMap` is keyed
by `ESMF_Method_Flag`, so translating name->index correctly still needs
that flag threaded in as a *new* parameter, making the interface more
complex for zero behavioral gain over resolving the index once, earlier
(at construction time, next to phase registration, where
`ESMF_Method_Flag` is already naturally in scope, exactly as
`run_child_by_name.F90` does today for its own analogous case).

**REQ-MTH-003a's trigger/advance discipline is one new graph-neutral
procedure, not an `OuterMetaComponent` method.** New file,
`superstructure/generic/graph/MethodInvocation.F90` (distinct from
`MethodInvocationAdapter.F90`), exporting one subroutine —
`invoke_on_default_network(graph, node_id, clock, rc)` — that:

1. Looks up the `MethodGraphNode` for `node_id` in `graph`.
2. For each of its bound `IN`/`INOUT` arguments, calls
   `graph%update(bound_node_id, default_network_id, rc)`
   (REQ-REV-006).
3. Calls the node's own `invoke(rc, clock)`.
4. Only if step 3 succeeds, calls `advance()`
   (`NodeRevision%advance`) on each bound `OUT`/`INOUT` argument's
   revision.

This needs only `ComponentGraph`/`MethodGraphNode`/`NodeRevision` —
exactly Phase 1-3's existing synthetic-graph-testable shape, no
`OuterMetaComponent`, no ESMF component. REQ-REV-011a's "controlled by
the `OuterMetaComponent` layer" is satisfied by *who calls* this
procedure (an `OuterMetaComponent` call site, when one exists — see
Risks below for why this sub-change does not yet add that call site
into `initialize_realize_provided.F90`-style real lifecycle code), not
by putting the mechanism itself inside `OuterMetaComponent`.

*Alternative considered:* an `OuterMetaComponent` type-bound procedure
(`this%invoke_method_node(...)`). Rejected: would require a real
`OuterMetaComponent`/ESMF component in every test of this discipline,
regressing from Phase 4a's synthetic-only testability for a piece of
logic that has no actual dependency on `OuterMetaComponent` state beyond
"which graph, which network" — both already plain arguments.

*Two narrower alternatives also considered, both rejected for the same
underlying reason:* a `MethodGraphNode` type-bound procedure, and a
`ComponentGraph` type-bound procedure.
- Not a `MethodGraphNode` method: `MethodGraphNode` deliberately holds
  no back-reference to its own owning `ComponentGraph`
  (`MethodGraphNode.F90`'s own comment on this — a Phase 4a decision,
  restated here rather than revisited), so a method needing both the
  node and its graph cannot live on the node without violating that.
- Not a `ComponentGraph` method (e.g. `graph%invoke_method(node_id,
  rc, clock)`, alongside the existing `graph%update()`): would mean
  editing `ComponentGraph.F90` itself - a Phase 1 "settled" foundational
  file every prior sub-change, 4a included, has deliberately kept
  additive-only (new sibling files, never edits to the core node/graph
  types themselves).
- The precedent this module actually follows is `GraphBuilder.F90`:
  free-standing procedures operating externally on two existing types
  (`OuterMetaComponent`+`ComponentGraph` there; `ComponentGraph`+
  `MethodGraphNode` here) rather than adding methods to either one.
  `MethodInvocation.F90` is one layer further in than `GraphBuilder.F90`
  (graph-neutral - no `OuterMetaComponent`/ESMF at all), which is also
  why it is its own file rather than folded into `GraphBuilder.F90`:
  putting graph-neutral logic inside the file whose own header
  describes it as "the integration layer between the graph-neutral
  core and the rest of MAPL" would misstate what this procedure
  actually depends on.

**REQ-MTH-011 step (c) convergence: match the existing fixed two-pass
schedule; add one missing hard-error check; do not add a new dynamic
loop.** Concretely:

- The "iteration" REQ-MTH-011 describes already exists and is already
  bounded: `GENERIC_INIT_PHASE_SEQUENCE`'s two-pass schedule, fixed by
  ESMF phase registration (`enums/GenericPhases.F90`). No new counter or
  configurable iteration-limit constant is introduced — the limit is
  already exactly 2, set by code this change does not touch.
- "No further progress possible" is not measured via
  `CharacteristicStatus` (Non-Goals above); it is a structural fact
  already true today for the one graph-native hook that exists at an
  ACCEPT_TRANSFER-cycled phase: `graphbuilder_run_connect_hook`'s
  `graph%is_frozen()` guard (`GraphBuilder.F90:469-473`) makes a second
  invocation a provable no-op. This sub-change adds no new guard here —
  it already exists and already satisfies "an iteration that makes no
  further progress terminates the cycle" for this hook.
- **New work:** promote `check_unsatisfied_imports`'s result from a
  warning to a hard error, and move the check to run *after* the second
  (final) ACCEPT_TRANSFER pass instead of solely at
  GENERIC_INIT_ADVERTISE/activate time. Today
  `graphbuilder_run_activate_hook` calls
  `check_unsatisfied_imports`/logs warnings once, at GENERIC_INIT_ADVERTISE
  time — *before* `graphbuilder_run_connect_hook` has run even once, so
  its report reflects nothing about whether resolution actually
  succeeded. This sub-change adds a second, hard-error-raising call
  to the same existing `check_unsatisfied_imports` procedure (no
  signature change needed — it already returns
  `unresolved_imports`/optionally), invoked from
  `graphbuilder_run_connect_hook` itself, gated on `graph%is_frozen()`
  being newly true after `graphbuilder_freeze` succeeds: if any required
  import is still unresolved once the graph is frozen (i.e., once no
  further pass can ever change anything), that is
  `_ASSERT`-level failure, not a logged warning.
- `GENERIC_INIT_REALIZE_PROVIDED`/`GENERIC_INIT_REALIZE_ACCEPTED` have no
  graph-native hook to apply this pattern to yet (Non-Goals) — this
  decision's scope is limited to the one graph-native hook
  (`run_connect_hook`) REQ-MTH-011 step (c) already touches today.
- **This is explicitly not a general solution and is not expected to
  remain adequate.** Matching legacy's fixed two-pass schedule is only
  correct because legacy itself never needed more than two passes for
  the configurations it supports — that is an empirical property of
  today's supported use cases, not a property of the underlying
  problem. It is straightforward to construct a hybrid NUOPC coupling
  configuration (mixed graph-native and externally-driven
  Advertise/Realize negotiation across component boundaries) where two
  passes provably do not reach a stable resolution. This sub-change
  deliberately does not attempt that general case — seeing real
  progress-detection requires exactly the kind of per-item resolved/
  unresolved signal `CharacteristicStatus` (`18-state-item-
  characteristics.md`, `[SPECULATIVE]`, Phase 5) is meant to provide,
  which does not exist yet, so building a real N-pass convergence loop
  now would mean guessing at that signal's shape rather than using it.
  The hard-error check added above is deliberately the fallback for
  exactly this gap: a configuration that would need a third pass fails
  loudly, at the point the fixed schedule runs out, instead of silently
  proceeding with an under-resolved graph. See Open Questions.

*Alternative considered:* a general-purpose, reusable "run these N
sub-steps until an iteration changes nothing, else error after a
configurable cap" utility, applicable uniformly to all three step-(c)
phases regardless of whether they have graph-native hooks yet.
Rejected as premature: with only one of the three phases having any
graph-native hook at all, and that hook already single-pass-idempotent
by construction (frozen-graph guard), a general iteration utility has
no second real call site to justify its own abstraction yet; building
one now risks guessing wrong about what `realize_provided`/
`realize_accepted`'s eventual graph-native hooks will actually need to
treat as "progress" once they exist.

## Risks / Trade-offs

- **[Risk]** No real `MethodGraphNode` is constructed for any real
  GridComp phase by this sub-change (Non-Goals) — the new real
  `GridCompPhaseInvoker`/`DriverResolver`/`invoke_on_default_network`
  are each independently correct and independently tested, but nothing
  in a real component's actual `Initialize`/`Run` path calls any of
  them yet. **Mitigation:** each is tested to the depth its own
  contract requires (`DriverResolver`+real `GridCompPhaseInvoker`
  against a real `GriddedComponentDriver` wrapping a minimal real ESMF
  test `GridComp`, manually constructing the `MethodGraphNode`/
  `GridCompMethodInvocation` rather than via `GraphBuilder`;
  `invoke_on_default_network` against synthetic `ComponentGraph` data,
  Phase 1-3 style) — this is the same "supply the mechanism, defer real
  construction-site wiring" posture Phase 4a itself used successfully.
  Whichever later sub-change decides how `GraphBuilder` constructs real
  `MethodGraphNode`s for phases (flagged as a genuine, currently
  unassigned roadmap gap — recommend the user add an explicit roadmap
  entry for it, since it belongs to neither 4c/4d's callback scope nor
  this sub-change's driver-integration scope) can then wire these three
  pieces together with no further rework of any of them.
- **[Risk]** The hard-error promotion in `check_unsatisfied_imports`
  (Decisions above) changes existing, already-shipped
  `graphbuilder_run_connect_hook` behavior from "log a warning, continue"
  to "fail" for any real configuration that currently relies on an
  import staying unresolved past freeze without erroring.
  **Mitigation:** `report_if_failed`'s existing convention
  (`GraphBuilder.F90:1173-1186`) already catches any `GraphBuilder`-
  internal failure at these three wrapper call sites and reports it
  through the component's logger rather than propagating it into real
  MAPL component initialization (`design.md` Decision from
  `graphbuilder-advertising-connections`, restated at
  `GraphBuilder.F90:1092-1102`) — so this change's new hard error is
  still caught there and downgraded to a logged failure at the
  `run_connect_hook` boundary, matching every other `GraphBuilder`
  failure mode's existing safety net. It is a hard error only *within*
  `graphbuilder_resolve_connections`'s own directly-tested API, not a
  new way for `GraphBuilder` defects to break real component
  initialization.
- **[Risk]** The fixed two-pass schedule this change relies on
  (Decisions above) is known-inadequate for hybrid NUOPC coupling
  configurations that genuinely need more than two realize/accept/
  realize passes to converge — not a hypothetical, per project
  experience with mixed graph-native/externally-driven negotiation.
  **Mitigation:** none, within this sub-change's own scope — not
  claimed to be solved here. The hard-error check (task 5) at least
  converts "silently under-resolved" into "fails loudly at
  initialization," which is strictly safer than today's warning-only
  behavior even though it does not make a genuinely-hybrid
  configuration work. Real generalization is recorded as an Open
  Question below, with an explicit trigger for when it must be
  revisited.
- **[Trade-off]** `DriverResolver`'s pointer-returning `resolve()`
  (Decisions above) is a narrower, more careful promise than
  `get_child_by_name`'s existing value-copy convention used elsewhere in
  `OuterMetaComponent` — two different access patterns to the same
  `children` map now coexist. Accepted: `get_child_by_name`'s copy
  exists for its own callers' reasons (documented as deliberately
  cautious, "be careful using result"); `DriverResolver`'s pointer is
  scoped to one transient call inside `GridCompPhaseInvoker`, which
  never retains it, so the two conventions do not conflict, only
  coexist for different, already-documented reasons.

## Open Questions

- **General, progress-based convergence for the realize/accept/realize
  cycle** (not resolved here, deliberately — see Decisions/Risks
  above). The hard-coded two-pass schedule this sub-change relies on is
  a property of today's supported configurations, not a property of
  the underlying negotiation problem; hybrid NUOPC coupling
  configurations can require more than two passes to reach a stable
  resolution, and no amount of matching legacy's existing schedule
  changes that. Deferring this is safe *for now* because: (a) no
  currently-supported real configuration is known to need more than
  two passes, (b) this sub-change's new hard-error check (task 5)
  ensures a configuration that does need more fails loudly rather than
  silently under-resolving, and (c) a real fix plausibly wants to build
  on `CharacteristicStatus`/`SPECIFIED` (`18-state-item-
  characteristics.md`), which is itself still `[SPECULATIVE]` and
  scoped to Phase 5 — designing a bespoke, parallel progress signal now
  risks conflicting with that later, more general mechanism. **This
  does not change the specs, this sub-change's own approach, or its
  task breakdown** — it is recorded here as a known, named limitation
  of the interim decision above, not as an unresolved ambiguity within
  this sub-change's own scope.
  - **Revisit when:** (1) a real configuration is found that needs a
    third realize/accept/realize pass and therefore hits this
    sub-change's new hard error where it previously (silently) worked,
    or (2) `CharacteristicStatus`/`18-state-item-characteristics.md` is
    promoted out of `[SPECULATIVE]`, whichever comes first. Either
    event should prompt a dedicated roadmap entry/sub-change for
    general convergence detection — not a quiet extension of this
    sub-change's own fixed-schedule assumption.

## Migration Plan

Additive: new `DriverResolver` abstract type + real `GridCompPhaseInvoker`
+ `OuterMetaComponentDriverResolver` + `invoke_on_default_network`
(`superstructure/generic/graph/MethodInvocation.F90`) are all new files/
types. One existing type (`GridCompMethodInvocation`) has its
`phase_name`/`get_phase_name` field/accessor renamed to
`phase_idx`/`get_phase_idx` — a source-compatible-within-this-repo
change since Phase 4a shipped no external callers of the old accessor
(archived change, additive-only, no consumers yet). One existing
procedure (`graphbuilder_run_connect_hook`) gains one new internal call
to the already-existing `check_unsatisfied_imports`, raising a new
`_ASSERT` failure path that `report_if_failed` already catches and
downgrades to a logged, non-propagating failure — no change to that
wrapper's own external contract (still logs and continues on any
internal failure, per its existing documented behavior). Rollback is a
plain revert of the new files plus the two small edits
(`GridCompMethodInvocation.F90`'s field rename,
`graphbuilder_run_connect_hook`'s new internal check call) and their
`CMakeLists.txt` entries.
