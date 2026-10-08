## Context

`docs/graph/spec/18-state-item-characteristics.md` is `[SETTLED]` on
direction (Value/Reference split, agnostic detection iteration,
eager-structural/lazy-content propagation split) but leaves four points
explicitly `[OPEN]`: `CharacteristicStatus` naming (§18.3),
`CharacteristicType` naming (§18.4), the ordering-delegation mechanism
(§18.6, `17-open-questions.md` Q13), and absent-key-vs-`INVALID`
representation (§18.5). Per this sub-change's own roadmap entry
(§20.4.4), these MUST be resolved here, up front, before implementation —
the same discipline Phase 4's 4b followed for REQ-MTH-011(c).

The codebase already has an unrelated, pre-existing `Characteristic`
family (`superstructure/generic/graph/Characteristic.F90`,
`CharacteristicId.F90`, `UnitsCharacteristic.F90`,
`VerticalGridCharacteristic.F90`, `GeomCharacteristic.F90`), built for
`graph/extension-reuse` (3c) to compare `VariableSpec`-derived metadata at
connection-resolution time. Its own header comment already anticipates
this change and declares itself deliberately independent of
`18-state-item-characteristics.md`'s `StateItemCharacteristic` — "read
...only as behavioral reference, never called." This existing family is
not renamed, merged, or modified by this change. One of its type names
(`GeomCharacteristic`) collides verbatim with a name
`18-state-item-characteristics.md`'s own table suggests for the new
hierarchy, which forces a naming decision below (same situation already
resolved once in this codebase for `UnitsConverterTransform` vs. legacy
`ConvertUnitsTransform`).

`GraphStateItem` (`superstructure/generic/graph/GraphStateItem.F90`) is a
concrete, non-polymorphic type with three allocatable ESMF-handle
components (field/field-bundle/state), two membership maps, and a single
`set`-choke-point mutation discipline (REQ-SI-004/006). It has no
characteristics-map component today.

`ComponentGraph`'s `DependencyNetwork` already exposes
`get_successors`/`get_predecessors`/`contains_dependency`
(`graph/dependency-network`), sufficient for the single-graph structural
walk this change needs (REQ-CHAR-016's first bullet). Cross-graph
structural walking (REQ-CHAR-016's second bullet) requires
parent-driver-mediated recursion into child `ComponentGraph`s and is
explicitly noted in the spec as "not fully worked out" — this change does
not attempt it.

**Correction found during implementation (recorded here, not just in
proposal.md, since it changes what this document's D8/Goals originally
claimed):** REQ-CHAR-018's content-side propagation story depends on a
`MethodGraphNode` invocation pulling its bound IN/INOUT arguments before
invoking. This document originally planned that pull as new work
(REQ-REV-011, D8 below) on the mistaken assumption that
`MethodInvocationAdapter%invoke()` was the only existing invocation entry
point. It already exists, one layer up: `docs/graph/spec/11-revision-and-
update.md` §11.4a already specifies REQ-REV-011/REQ-REV-011a (its own
"Note" already cross-references this capability's REQ-CHAR-018),
`openspec/specs/graph/method-graph-node/spec.md` already documents the
matching requirement/scenarios, and
`superstructure/generic/graph/MethodInvocation.F90`'s
`invoke_on_default_network()` already implements it end-to-end
(`pull_bound_inputs()`/`advance_bound_outputs()`), shipped with Phase 4b
(`griddedcomponentdriver-integration-lifecycle`), with existing coverage
in `Test_MethodInvocation.pf`. D8 below is corrected accordingly: no new
code, no new spec delta for `graph/node-revision-and-update`.

## Goals / Non-Goals

**Goals:**
- Resolve the four spec-mandated open points (naming x2, ordering
  mechanism, map representation) as concrete, final decisions.
- Implement REQ-CHAR-001..018 as new, independently unit-testable types
  and a mutator, exercised with synthetic nodes only (no ESMF
  component/`GridComp`/`StateRegistry`), matching Phase 1–2's exit
  criterion even though this lands after Phase 4.
- Add the `characteristics` map and ordering-query method to
  `GraphStateItem` without changing its existing allocation invariant or
  public API shape (REQ-SI-004/006 unaffected).
- Demonstrate, with one new test, that the already-shipped REQ-REV-011
  mechanism (`MethodInvocation.F90`'s `invoke_on_default_network`)
  correctly makes REQ-CHAR-018's content-side story concrete for a
  structural reset produced by this change's new mutator — no new
  production code in that mechanism itself (see Context's correction
  note and D8 below).

**Non-Goals:**
- Adding or modifying REQ-REV-011/`invoke_on_default_network` or any
  `MethodInvocationAdapter` subtype. That mechanism already exists and
  already satisfies what this change needs from it (see Context's
  correction note) — this change only adds a test confirming the
  interaction, never a code change there.
- Reconciling or merging the new `StateItemCharacteristic` hierarchy with
  `graph/extension-reuse`'s existing `Characteristic` family. They remain
  two deliberately independent models, per that module's own precedent.
- Rewiring `GraphBuilder`'s real connection-resolution path
  (`resolve_one`/`ExtensionResolution.F90`) to consult
  `GraphStateItem.characteristics`. That is real, separate integration
  work left to a follow-up.
- Real `build_transform` implementations for `TypeKindCharacteristic` or
  `GeometryCharacteristic`. Only `units`-equivalent plumbing is
  demonstrated to be wireable; the other two fail explicitly, mirroring
  `graph/extension-reuse`'s own current state for non-`units`
  characteristics.
- Cross-`ComponentGraph` structural-dependent walking (REQ-CHAR-016's
  second bullet).
- Any change to legacy `StateRegistry`/`ExtensionFamily`/`ClassAspect`.

## Decisions

### D1. `CharacteristicStatus` keeps its working name
No alternative name was proposed anywhere in `17-open-questions.md`, it
does not collide with any existing type, and it reads clearly next to
`StateItemCharacteristic`. Finalized as-is: `CharacteristicStatus`, with
exactly the five values from REQ-CHAR-003
(`INVALID`/`SPECIFIED`/`MIRRORED`/`UNCHECKED`/`DEFERRED`) and no
additional `ERROR`/`CONFLICT` value — REQ-CHAR-003's own `[OPEN]` note
flags that as a possible future addition, not a blocking requirement for
this change; a reconciliation failure the mutator or detection algorithm
cannot resolve is reported as an ordinary `rc`/`_FAIL` error at the call
site, not encoded as a sixth status value. Module:
`mapl_CharacteristicStatus_mod`.

**Alternative considered:** a richer status set including `CONFLICT`.
Rejected for this change — no concrete scenario in §18.3-§18.8 requires
distinguishing "irreconcilable mismatch" from an ordinary call-site
failure; adding it speculatively would be exactly the kind of
unvalidated enum growth REQ-CHAR-003's own open note warns against.

### D2. The type-tag type is named `StateItemCharacteristicKind`, not `CharacteristicType`
`18-state-item-characteristics.md`'s own working name, `CharacteristicType`,
is rejected in favor of `StateItemCharacteristicKind` for one concrete
reason: the existing, unrelated `graph/extension-reuse` family already
defines `CharacteristicId` (`CharacteristicId.F90`) as "type-safe identity
for a Characteristic kind" — a per-kind identity type for a *different*
`Characteristic` hierarchy, living in the same directory. `CharacteristicId`
next to a new, unrelated `CharacteristicType` is a real, ongoing
readability hazard (two near-identical names, two unrelated purposes, no
textual cue which is which) — the same class of problem this codebase
already solved once for `UnitsConverterTransform` vs. legacy
`ConvertUnitsTransform` by picking a visibly different name rather than
relying on module-path disambiguation alone. `StateItemCharacteristicKind`
ties the name directly to the type it tags (`StateItemCharacteristic`,
matching REQ-CHAR-005's own description: "closer to a type-tag
enumeration than to `NodeId`") and shares no prefix collision with
`CharacteristicId`. Module: `mapl_StateItemCharacteristicKind_mod`,
following `CharacteristicId.F90`'s own shape (wrapped integer, named
parameter constants, `==`/`/=`/`<`, `to_string()`) — same pattern, new,
unambiguous name.

**Alternative considered:** keep `CharacteristicType` verbatim, relying on
module-qualified `use` statements to disambiguate from `CharacteristicId`
at every call site. Rejected: Fortran's `use, only:` imports the bare name
into scope, and two reasonable-sounding names for unrelated concepts in
the same subsystem is an avoidable maintenance cost, not merely a cosmetic
one.

### D3. Geometry subclass is named `GeometryCharacteristic`, not `GeomCharacteristic`
`18-state-item-characteristics.md`'s table literally proposes
`GeomCharacteristic` for the geometry `ReferenceCharacteristic` subclass —
an exact, unavoidable collision with the existing, unrelated
`mapl_GeomCharacteristic_mod`/`GeomCharacteristic` type already shipped
for `graph/extension-reuse`. Fortran module and type names share one
global namespace (the same constraint already documented for
`UnitsConverterTransform`/`ConvertUnitsTransform` in the extension-reuse
change's own task list, item 9.4), so the two cannot coexist under
identical names. Resolved by using `GeometryCharacteristic` (full word,
distinguishable at a glance, no abbreviation collision) for this change's
new type. Module: `mapl_GeometryCharacteristic_mod`.

`PhysicalUnitsCharacteristic` and `TypeKindCharacteristic` (the spec's
other two suggested names) do not collide with anything existing
(`graph/extension-reuse`'s analog is named `UnitsCharacteristic`, a
different name) and are kept as-is.

### D4. Absent key is canonical for "never established"; present-with-`INVALID` is reserved for "established but not yet resolved"
REQ-CHAR-008 leaves open whether absence of a type-tag key and presence
with `INVALID` status are used interchangeably or distinguished. This
change distinguishes them: a key is absent until something first
constructs a characteristic for that axis (e.g. `advertise`-time
declaration establishes a `PhysicalUnitsCharacteristic` with whatever
status is known at that point); once present, the entry is never removed
— it may transition through `INVALID`/`DEFERRED`/`MIRRORED`/`SPECIFIED`/
`UNCHECKED`, but the key stays in the map. This matches REQ-CHAR-004's
reset-to-`INVALID` language ("a newly-reallocated dependent... must have
its own status/revision reset to `INVALID`" — a reset of an existing
entry, not key removal) and gives callers a clean distinction: "is this
axis meaningful for this item at all" (key presence) vs. "is its value
currently known" (status).

**Alternative considered:** treat absence and `INVALID` as fully
interchangeable (map entries are lazily created on first touch,
never pre-populated as `INVALID`). Rejected: REQ-CHAR-004's reset
language only makes sense if there is an existing entry to reset; if
absence and `INVALID` meant the same thing, "reset to `INVALID`" would be
indistinguishable from "remove the key," which REQ-CHAR-004 does not say.

### D5. Ordering delegates to a static per-kind table (Q13's recommendation)
REQ-CHAR-011 requires delegating transform-insertion order to "something
that varies by the `GraphStateItem`'s active kind/subtype." This change
adopts Q13's own recommendation verbatim: a static table keyed by
`GraphStateItem`'s existing variant classification
(`graph/state-item`'s `variant()` query), each entry an ordered list of
`StateItemCharacteristicKind` values. `GraphStateItem%ordering()` looks up
its own variant in this table and returns the corresponding order,
filtered to only the kinds actually present and mismatched for a given
comparison. A kind/variant with no table entry gets a defined default
(declaration order of the mismatched set, stable but not meaningfully
prioritized) rather than failing — this change does not need every
variant to have a hand-tuned order to be useful, only to never crash on
one that doesn't.

**Alternative considered:** a method on one "primary" characteristic
(Q13's rejected alternative). Rejected for the same reason Q13 gives: it
requires picking an arbitrary primary characteristic for kinds that may
not have an obvious one, and makes one characteristic subclass respon-
sible for knowing about every other's relative priority — the opposite of
REQ-CHAR-001's "each characteristic is independently adaptable" framing.

### D6. New hierarchy stays independent of `graph/extension-reuse`; no `GraphBuilder` rewire
REQ-CHAR-009 describes an algorithm shaped like `09-extension-reuse.md`'s
existing one ("iterate all characteristics... build a reconciling
chain"), but this change implements it as a new, standalone algorithm
operating on `GraphStateItem.characteristics` (synthetic-node pFUnit
tests only) — it does not modify `GraphBuilder.F90`'s real
`resolve_one`/`ExtensionResolution.F90` connection-resolution path, and it
does not make the existing `Characteristic`/`CharacteristicMap`
(`graph/extension-reuse`) consult or populate `GraphStateItem`'s new map.

This follows directly from `Characteristic.F90`'s own header comment,
written during 3c, explicitly anticipating this exact moment: "...and
`StateItemCharacteristic` (still `[SPECULATIVE]`...) — read either only as
behavioral reference, never called." That comment is itself a design
decision already made by the project before this change existed; this
change honors it rather than re-litigating it. Unifying the two families
— so real connection resolution populates and queries
`GraphStateItem.characteristics` instead of (or alongside) its own
`VariableSpec`-derived `CharacteristicMap` — is real, separable
integration work (touches `GraphBuilder.F90`, a production code path with
its own equivalence-testing discipline against the legacy coupler) left
to an explicit follow-up, not attempted speculatively here.

**Alternative considered:** rewire `GraphBuilder`'s real connection
resolution to use the new `characteristics` map directly, retiring the
`graph/extension-reuse`-specific `Characteristic`/`CharacteristicMap` in
the same change. Rejected: this is a second large, separately-scoped
effort (its own `GraphBuilder.F90` surgery, its own equivalence-test
obligation against the legacy coupler, matching 3b/3c's own precedent for
anything touching real connection resolution) that would roughly double
this change's footprint and risk, for a benefit (one model instead of
two) that is real but not required by anything in §18.2–§18.8 itself.

### D7. Mutator and structural walk live in a new module, not on `GraphStateItem` or `ComponentGraph` directly
REQ-CHAR-016's mutator (`mapl_SetSharedCharacteristic_mod`, a
`MAPL_SetGeom`-style entry point in shape) is implemented as a standalone
procedure taking the owning `ComponentGraph`, the shared node's `NodeId`,
and the new value, rather than as a method on `GraphStateItem` (which has
no access to the owning graph's `DependencyNetwork` for the structural
walk) or a new general-purpose method on `ComponentGraph` (which has no
reason to know about `StateItemCharacteristic` specifically). It:
1. Resolves the shared node's `GraphStateItem` and updates its referenced
   value's own storage.
2. Walks `DependencyNetwork%get_successors` from that node, filters to
   `StateItemNode` successors (skipping `TransformGraphNode` successors,
   left to the lazy path per REQ-CHAR-016), and resets each to `INVALID`
   via a pure structural reset.
3. Advances the shared node's own `NodeRevision`.

No `MethodGraphNode` is ever looked up or invoked by this procedure
(REQ-CHAR-017) — it only calls `DependencyNetwork` query methods and each
affected `StateItemNode`/`GraphStateItem`'s own structural-reset/
revision methods.

**Mechanism note on "reset to INVALID" (REQ-CHAR-004):** `NodeRevision`'s
existing public API deliberately exposes no backward/invalidate operation
(`advance()` only moves forward — `NodeRevision.F90`'s own header:
"only advance, never arbitrary assignment"), and this change does not add
one — REQ-CHAR-004's "reset to INVALID" for each *dependent* is achieved
with zero new `NodeRevision`/`StateItemNode` API by calling
`StateItemNode`'s already-existing raw `set_revision()` setter with a
freshly default-constructed `NodeRevision()` (already the invalid sentinel
by construction, per that type's own default field initializer). This is
distinct from the *shared/referenced* node's own revision, which this
mutator advances forward via the existing `advance_revision()` (never
reset backward) — matching this capability's own spec text precisely
("resets every direct structural dependent... to the invalid state; and
advances the shared node's revision" — two different operations on two
different nodes, not the same operation applied twice). Using the raw
sentinel-valued default for dependents rather than inventing a new
"invalidate" concept avoids any risk to `NodeRevision`'s own
never-repeats-a-value invariant (REQ-REV-001): the `INVALID` sentinel is
guaranteed distinct from every value `advance()` can ever produce, so a
dependent compared against *any* prior recorded baseline by the
unmodified REQ-REV-006 machinery (`current_rev /= baseline_rev`,
`TransformGraphNode.F90`) is correctly detected as stale the moment it is
next demanded — which is exactly what REQ-CHAR-018/this capability's own
"Next demand-driven request produces correct content" scenario requires,
using the existing comparison mechanism completely unchanged.

**Scope note on "structural reset" itself.** REQ-CHAR-016 step 2 gives
`ESMF_FieldEmptyReset` as an illustrative example ("e.g.") of a structural
reset for a field-kind dependent - it is not a mandate that this
graph-neutral mutator itself call ESMF. Consistent with this change's own
Goals (synthetic-node-testable, no ESMF component/`GridComp`/
`StateRegistry` involvement, matching Phase 1-2's exit criterion even
though this change lands after Phase 4), `set_shared_characteristic()`
performs the graph-neutral half of "structural reset" only: resetting
each direct `StateItemNode` dependent's own revision to the invalid
sentinel (mechanism note above), and, for any dependent that itself holds
a `ReferenceCharacteristic` entry referencing the same shared `node_id`,
setting that entry's `CharacteristicStatus` to `INVALID` too (best-effort
- a dependent reached only through plain `DependencyNetwork` adjacency,
with no such characteristic entry of its own, is still revision-reset;
the status update is additional, not required, information). Actually
reallocating an ESMF payload to a new shape (`ESMF_FieldEmptyReset` or
equivalent) is real ESMF-aware work left to whichever future integration
layer wires this mutator into a real `MAPL_SetGeom`-style entry point -
not attempted here, same posture as this change's other already-stated
deferrals (real `build_transform` providers, cross-`ComponentGraph`
walking).

### D8. REQ-REV-011's pre-invocation pull already exists; this change adds a test, not code
Corrected during implementation (see Context's correction note): the
pre-invocation pull this change needs for REQ-CHAR-018's content-side
story is not new work. It already exists at
`superstructure/generic/graph/MethodInvocation.F90`'s
`invoke_on_default_network(graph, node_id, rc, clock)` — a procedure one
layer above `MethodGraphNode`/`MethodInvocationAdapter` that already has
the `ComponentGraph` reference those two types deliberately do not carry
(`MethodInvocationAdapter%invoke()`'s signature is
`(arguments, bindings, clock, rc)` — no graph, no network id, by design,
per that module's own header comment on staying decoupled from
`MethodGraphNode`). `invoke_on_default_network()` already: (1) calls
`graph%update(graph%get_default_network_id(), bound_id, rc)` for every
bound IN/INOUT argument before invoking (`pull_bound_inputs`), (2) invokes
the node, and (3) advances every bound OUT/INOUT argument's revision only
on success (`advance_bound_outputs`) — exactly REQ-REV-011/REQ-REV-011a,
already shipped with Phase 4b.

This change's only obligation here is to confirm the two mechanisms
compose correctly: a structural reset performed by this change's new
mutator (D7) on a bound IN/INOUT argument must be recognized as stale by
`pull_bound_inputs`'s existing `graph%update()` call the next time
`invoke_on_default_network` runs on a dependent `MethodGraphNode`. No
change to `MethodInvocation.F90`, `MethodInvocationAdapter.F90`,
`GridCompMethodInvocation.F90`, or `StateMethodInvocation.F90` is made or
needed — the existing mechanism's own staleness check
(`NodeRevision`-based, via `ComponentGraph%update()`/REQ-REV-006) already
has no special-case branch for *why* a bound argument is stale, so a
reset produced by the new mutator is indistinguishable, to that existing
code, from staleness produced by an ordinary `TransformGraphNode` chain.

**Alternative considered (this change's original, incorrect plan):** add
the pull as a new step inside `MethodInvocationAdapter%invoke()` or its
two concrete subtypes. Rejected on discovery that (a) those types
deliberately have no `ComponentGraph` reference to call `update()` with,
and (b) the capability this would have added already exists one layer up
and is already tested. Implementing it a second time at the wrong layer
would have been redundant, and would have been real, unnecessary scope.

### D9. `set_characteristic` enforces that a characteristic's own kind matches its map key
Found during code review (not caught by the original task breakdown):
REQ-CHAR-009's detection algorithm (task 5.1) pairs two `GraphStateItem`s'
characteristics by map *key* only (`find_mismatched_state_item_characteristics`
looks both sides up under the same `StateItemCharacteristicKind`, then calls
`needs_extension_for`), implicitly assuming both sides are therefore the
same concrete type. Nothing enforced that assumption: `set_characteristic`
accepted any `(kind, characteristic)` pair, so a caller could store e.g. a
`GeometryCharacteristic` under `PHYSICAL_UNITS_CHARACTERISTIC_KIND`. Each
concrete `needs_extension_for`'s `class default` branch (reached only on a
genuine cross-subclass comparison) silently returned `.true.` ("needs
extension") for this case — which does not actually catch the defect, it
just mislabels an impossible pairing as an ordinary, reconcilable mismatch
with no Transform that could ever resolve it (REQ-CHAR-002's own
one-characteristic-one-Transform-kind framing: there is no, and can never
be, a cross-kind Transform).

Fixed at two levels:
1. **Root cause** — `GraphStateItem%set_characteristic` now asserts
   `characteristic%get_kind() == kind` before inserting, failing
   explicitly (`rc`) the moment a mismatched pairing is attempted. This
   keeps the detection algorithm's own same-key-implies-same-type
   assumption actually true by construction, not merely by caller
   convention.
2. **Defense in depth, scoped to the real call chain** — each concrete
   `needs_extension_for`'s `class default` branch now uses `error stop`
   (unchanged signature, no `rc` added) instead of returning `.true.`.
   This was revisited once during review: an initial attempt added a
   catchable `rc`/`_FAIL` path instead, on the reasoning that
   `needs_extension_for` is a public, directly callable procedure and
   nothing *at the language level* stops some future caller from
   invoking it outside the sanctioned path. That reasoning proved too
   strict a bar — the question is not "can this one procedure be proven
   unreachable in isolation" but "can the actual call chain in this
   system reach it." For the real call chain, it provably cannot: the
   only production caller of `needs_extension_for` is
   `find_mismatched_state_item_characteristics`
   (`mapl_StateItemCharacteristicDetection_mod`), which looks both
   `char_a`/`char_b` up by the *same* `StateItemCharacteristicKind` key
   out of two `GraphStateItem.characteristics` maps; (1) is the only
   insertion path into that map and already guarantees every entry's
   `get_kind()` matches its own key. So any two entries retrieved by the
   same key are, in this system's actual call chain, provably the same
   concrete type - exactly this codebase's own existing `error stop`
   precedent for "should be checked by calling procedure"
   (`DependencyNetwork.F90`'s `node_reachable`/`has_cycle_from`,
   `ActualConnectionPt.F90`). The `rc`-threading attempt was reverted.

**Alternative considered:** leave `set_characteristic` unchanged and only
harden the `needs_extension_for` branches. Rejected: that would still let
a mismatched pairing be stored silently and only fail later, at whatever
point something happens to compare it - potentially far from the actual
mistake, and not at all if nothing ever compares that particular pairing.
Catching it at insertion time is strictly better and costs one `_ASSERT`.

## Risks / Trade-offs

- **Two parallel "characteristic" models (D6).** A future reader may
  reasonably ask why `graph/extension-reuse`'s `Characteristic` and this
  change's `StateItemCharacteristic` both exist and don't talk to each
  other → mitigated by this document's own D6 entry and by
  `Characteristic.F90`'s pre-existing header comment; both now point at
  each other and at this design doc section. Follow-up integration work
  is named explicitly (proposal.md, Impact) rather than left implicit.
- **Static per-kind ordering table (D5) may be insufficient for a real
  configuration not yet exercised** — Q13's own confidence is "medium,"
  with no concrete comparison use case worked through end-to-end →
  mitigated by keeping the table's default (stable declaration order)
  safe/non-crashing for any variant without a hand-tuned entry, so an
  under-specified case degrades gracefully rather than failing.
- **Single-ComponentGraph structural walk (non-goal) leaves a real gap**
  for geometry/vertical-grid sharing across a parent/child boundary → the
  spec itself marks this `[OPEN]`/"not fully worked out"; deferring it
  here does not regress anything (no code path today performs any
  cross-graph structural reset at all), and the single-graph case is
  still a genuine, independently useful increment.
- **New types land with no production call site** (D6's non-goal) — pure
  additive risk of "unused code" perception → mitigated by thorough
  synthetic-node pFUnit coverage (tasks.md) demonstrating the mechanism
  works correctly in isolation, matching this project's established
  precedent for additive, not-yet-wired-in graph-native capability (4e,
  4f, 4g all landed this way before any real init-path integration).
