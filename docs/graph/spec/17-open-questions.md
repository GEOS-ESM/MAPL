# 17. Open Design Questions

Status: `[OPEN]` — this entire document is analysis and recommendation, not
specification. Nothing here is binding until promoted into the numbered
sections above by an explicit decision.

Each question restates the original prompt, gives a recommendation, states
its confidence, and cross-references the affected spec sections.

---

## Q1 — Local ComponentGraphs + boundary nodes vs. one global graph, given MethodGraphNode

**Recommendation:** Keep local `ComponentGraph`s (REQ-HIER-004). Do not
introduce a global owning graph.

`MethodGraphNode` doesn't change the ownership argument — a parent's
`MethodGraphNode` for "invoke child phase X" still only needs the child's
*published port* `NodeId`s to bind arguments, exactly like a value
dependency does today. It does not need the child's internal topology.

**What does need re-examination:** whether "parent invokes child method"
should itself be represented as a dependency in the *parent's* network
(a `MethodGraphNode` with predecessor = parent-local proxy for a child
input port, successor = parent-local proxy for a child output port), or
left as an out-of-band driver call that merely happens to be sequenced by
existing GridComp run-loop ordering.

**Resolved by the spec author, against the original recommendation above:**
do NOT represent "parent invokes child method" as a dependency in the
parent's graph. Except for init phases, these calls generally happen in
the *middle* of a parent method's own execution — not at a graph-traversal
boundary the parent's `DependencyNetwork` could observe — and are
therefore out of scope for the parent's graph to model at all. This is the
same structural point later established independently in
`18-state-item-characteristics.md` §18.8 (mutation-and-immediate-use inside
one call has no traversal boundary for demand-driven pull to hook into):
a mid-`Run`-method child invocation is exactly such a case, so trying to
represent it as an ordinary dependency edge would repeat that same
mismatch. Init-phase child invocation is different in kind (it happens at
a real phase boundary, per `12-methods-and-drivers.md` REQ-MTH-011) and MAY
still be represented as an ordinary dependency if useful — this resolution
applies specifically to mid-method (`Run`-time) child invocation.

**Confidence:** high on "no global graph"; high (was medium) on "do not
represent mid-method child invocation as a parent-graph dependency,"
following the spec author's resolution above.

---

## Q2 — MethodGraphNode for GridComp phases and State callbacks without duplicating invocation logic

**Recommendation:** Invocation adapters (`GridCompMethodInvocation`,
`StateMethodInvocation`) should be thin translators that call
`GriddedComponentDriver` (for GridComp phases) or a callback-attachment
wrapper (for State methods) — never reimplement ESMF calling sequences
themselves.

Concretely: `MethodGraphNode` should hold (a) a reference to which
adapter kind it uses, (b) the stable driver identifier (REQ-MTH-009), and
(c) its named argument bindings. The adapter's `invoke()` should do
nothing but: gather bound arguments → call the one existing driver/
attachment entry point → return. If an adapter starts needing to know
ESMF-specific details beyond "here are my bound arguments, here is which
driver/attachment to call," that is a signal invocation logic is leaking
into the graph layer and should be pushed back into
`GriddedComponentDriver` or the callback attachment wrapper instead.

**Status:** the *shape* here is a recommendation; the two adapter names
were already proposed in the snapshot. Treat as `[OPEN]` until a driver
signature for "invoke with named bindings" is actually written down.

**Spec author:** agrees with the recommendation.

---

## Q3 — Transform port bindings: TransformGraphNode vs DependencyNetwork

**Recommendation:** store bindings externally, keyed by
`(DependencyNetworkId, node NodeId)` — i.e., generalize the callback
approach (`CallbackStateBinding`, already external) to ordinary
Transforms, rather than special-casing single-network transforms to store
bindings on-node.

**Why (contradiction flagged):** the snapshot proposes on-node storage
"if each transform node belongs to only one relevant network"
(`10-transforms-and-ports.md` §10.3) but *also* specifies callback method
bindings as external, per-network structures because callback methods
routinely belong to two networks (get/put). Having two different storage
strategies selected per-instance based on how many networks a given node
happens to participate in is a source of implementation inconsistency:
code that walks bindings would need to check "is this on-node or
external" per node. A uniform external-binding-table approach removes
that branch entirely and costs nothing for the single-network case (a
1-entry lookup). Recommend adopting it uniformly.

**Confidence:** medium-high. The single counter-argument is potential
lookup overhead at the ~10,000-node scale mentioned in
`11-revision-and-update.md` §11.5 — but that section already anticipates
compiling the frozen graph into direct wrappers for performance, so the
interpreted-graph binding lookup cost is not on the long-term hot path.

**Spec author:** agrees with the recommendation.

**Formalized.** Promoted into the numbered sections as
`10-transforms-and-ports.md` REQ-XFORM-005 and `06-dependency-network.md`
§6.6 (both now `[SETTLED]`). See also Q17 (visualization export
port-binding labels), which was blocked on this and is now resolved as a
consequence.

---

## Q4 — Multiple networks sharing StateItemNodes and revisions

This is mostly answered by existing rules, restated here for clarity:

- Revisions live on the `StateItemNode` (or eventually its ESMF Info),
  **not** per-network — REQ-REV-001/003. All networks referencing a given
  node see the same revision.
- Producer uniqueness (REQ-DEP-008) is scoped **per network**, so the same
  node can have different producers in different networks (get vs. put;
  forward vs. return).
- **Resolved by the spec author (was "unresolved," shared with Q3's
  neighbor, `16-inout-items.md` §16.3 — that document remains
  `[DEFERRED]` in full, but this specific sub-rule is settled
  independently of it):** there MUST NEVER be a case where, within a single
  update pass, the same node is written to more than once. This is now a
  hard constraint on graph construction, not just an execution-order
  hazard to avoid — `GraphBuilder`/`ComponentGraph` construction-time
  validation MUST reject any wiring that could result in the same
  `StateItemNode` being written by more than one producer within what
  would be a single update pass (this subsumes and sharpens REQ-DEP-008,
  which only constrained producer count *per network* — this adds a
  cross-network check for the same update-pass case). The callback get/put
  pattern remains fine specifically because REQ-CB-020 already guarantees
  the two writes are never part of the same pass (single invocation, after
  all args prepared, get and put are temporally distinct events). The
  general ordinary-inout case (`16-inout-items.md`) remains `[DEFERRED]`
  per that document's status and is not addressed by this constraint
  beyond ruling out the specific same-pass-double-write hazard.

**Confidence:** high (was medium), given the spec author's resolution
above converts this from an open design question into a stated
construction-time constraint.

---

## Q5 — Callback wildcard collection connections in GraphBuilder and public ports

**Recommendation:** model a wildcard callback connection as a distinct
connection-point kind (not a variant of an ordinary single-source
connection), because it has different arity (one destination, N sources)
and a validation step ordinary connections don't have (interface
conformance check per match, REQ-CB-016 step 2).

Concretely: `GraphBuilder` should (1) resolve the wildcard against the
flattened qualified-export namespace at wiring time (not before — the set
of matches can only be known once sibling/descendant advertisement is
complete), (2) validate each match against the named `CallbackInterface`,
(3) create the destination collection State + `NamedAlias` members, (4)
register the collection as a normal `StateItemNode`/`StateValue`, and (5)
wire ordinary per-member dependencies as if each match had been named
explicitly. From the `DependencyNetwork`'s point of view, nothing new is
needed once expansion happens — the special part is entirely in
`GraphBuilder`'s expansion/validation step, not in the graph core.

Public ports: the *collection* (not each individual matched callback)
should be the thing exposed as a public port, if this Invoker's callback
collection itself needs to be visible further up the hierarchy. Do not
expose N individual ports per match.

**Open sub-item, resolved by the spec author:** pattern syntax is
**regex**, not glob — already implemented this way in the legacy
application, so this is adopting existing precedent rather than choosing
fresh. `*/tracers`-style examples elsewhere in this spec should be read as
illustrative shorthand, not literally glob syntax; REQ-CB-016 step 1
implementations MUST use regex matching against the flattened qualified-
export namespace.

**Confidence:** medium-high on the GraphBuilder-only placement of
complexity; high (was low) on pattern syntax, now settled as regex per the
spec author.

---

## Q6 — Are Handler/Invoker the best terms?

**Recommendation:** keep them for now; they are mechanically accurate and
explicitly avoid the "service provider" ambiguity called out in the
snapshot (Advection: DYN invokes but does not provide/own tracer data;
Aerosol: handles callbacks *and* provides the science — these two examples
would break any single "provider" framing). No better pair identified
during this pass. This is a naming question with low technical risk either
way — do not block implementation on it; the *distinction* (REQ-CB-006) is
what matters and is settled regardless of the label chosen.

**Confidence:** low-stakes either way; recommend not spending more design
time here unless a stakeholder objects to the specific words.

**Spec author:** agrees with the recommendation.

---

## Q7 — Distinguishing ESMF placement from callback semantics without misleading StateIntent values

**Problem restated:** `StateIntent` (`IMPORT`/`EXPORT`) already means
something specific in MAPL (which state a component receives which data
through). Overloading it, or inventing parallel intent-like values, to
also express Handler/Invoker placement risks confusing two orthogonal
axes: *which ESMF State object physically holds this* vs. *who owns the
implementation / who calls it*.

**Recommendation:** do not add new `StateIntent` values. Instead, treat
Handler/Invoker placement (§15.8) purely as a `GraphBuilder`/accessor-layer
convention, expressed through a separate attribute — e.g. a
`CallbackRole` (`HANDLER` | `INVOKER`) attached to the `CallbackStateBinding`
(REQ-CB-007), which already exists as a real, per-binding structure. This
keeps `StateIntent` meaning exactly what it means today, and keeps the new
axis (role) as new, additively-introduced metadata rather than a
reinterpretation of an existing enum. Dedicated MAPL accessors
(REQ-CB-012) then read `CallbackRole` to decide/hide placement, rather than
inferring role from which physical State something was found in.

**Confidence:** medium-high — this avoids the identified risk cleanly, but
has not been checked against every call site that currently branches on
`StateIntent`.

**Spec author:** agrees with the recommendation.

---

## Q8 — Revisions for composite State and FieldBundle values

**Settled sliver:** advertised-granularity rule (REQ-REV-008) — a
container's revision advances when the *advertised* logical unit changes,
regardless of whether that's a Field or a whole FieldBundle.

**Open:** whether member-level changes ever need independent visibility
when a member is *also* separately graph-visible (recall
`FieldBundleValue`'s member map, REQ-VAL-003, covers only graph-visible
members — the exact case where double bookkeeping could arise). Two
candidate rules, neither adopted yet:

- **(a) Container-authoritative:** only the container's revision is ever
  compared by consumers of the container; member revisions exist only for
  producers *within* the component that owns the container's internal
  wiring. Consumers outside never look at member revisions directly.
- **(b) Independent tracking:** both container and member revisions are
  independently meaningful, and a Transform can depend on either
  granularity depending on what it actually reads.

**Recommendation:** (a), container-authoritative, as the default — it
matches REQ-REV-008's existing rule and avoids needing a rule for
"container revision advanced but no member did" or vice versa. Adopt (b)
only for a specific `GraphValue` subtype if a concrete Transform genuinely
needs member-granularity triggering (e.g., a transform that only cares
about one member of a large FieldBundle) — treat that as a documented
exception, not the default.

**Confidence:** medium. This has not been stress-tested against
`StateValue` (which, unlike `FieldBundleValue`, wraps something with much
more heterogeneous membership and its own ESMF semantics).

**Spec author:** agrees with the recommendation.

---

## Q9 — Compiling the graph-executed prototype to efficient direct ESMF/MAPL wrapper relationships

**Recommendation:** treat this as a distinct, later phase, gated on the
reference (interpreted) implementation being correct and validated first
(REQ-REV-009 already states this ordering). Compilation should:

1. Take a *frozen* `ComponentGraph` (freezing already guarantees no more
   structural change — the precondition compilation needs).
2. Walk each `DependencyNetwork` once, emitting a direct call sequence
   (or direct wrapper-to-wrapper linkage) equivalent to what the
   interpreted `update` traversal (REQ-REV-006) would do, exploiting the
   stated low fan-out (0–3) to avoid any dynamic dispatch/lookup at
   runtime.
3. Keep the interpreted path alive as a reference oracle
   (REQ-REV-009) — do not delete it once compilation exists; that
   contradicts the explicit reference-mode-testing requirement.

**Not addressed here:** the actual compiled representation (generated
Fortran? a resolved closure table? direct pointer wiring through existing
MAPL wrapper objects?) is a substantial separate design effort and is
intentionally left unspecified — flagging it as the least-ready item of
the ten for anyone hoping to start implementation immediately.

**Confidence:** low on mechanism, high on the phasing/ordering
recommendation.

**Spec author's response (confirmed):** agreed that compilation is a later
phase. Additional clarification: with the per-component-graph model
(REQ-HIER-004, one `ComponentGraph` per `OuterComponent`, no global graph),
the per-component compiled form **is** the compiled form referred to here
— there is no separate "compile the whole hierarchy" step to worry about
beyond compiling each local graph independently. The original hesitation
about compilation was specifically about a hypothetical *global* graph,
where compiling across component boundaries could inadvertently violate
the parent/child encapsulation boundary (REQ-HIER-003, REQ-HIER-005/006 —
a parent may not reach into a child's internal topology). That concern
does not apply to compiling a local `ComponentGraph` in isolation, which is
exactly what §2.4's "no global graph" position (Q1) already keeps this
proposal scoped to.

---

## Q10 — Incremental implementation while preserving existing MAPL behavior

**Recommendation — suggested implementation order**, chosen so each step
is independently testable against current MAPL behavior before the next
begins:

1. **Identities** (`05-identities.md`) — `NodeId` + FPP template + other
   ID types. Zero behavior risk; pure new code, nothing else depends on
   it yet being wired up.
2. **GraphValue + GraphNode hierarchies** (`03`, `04`) — new types, not yet
   connected to real components. Unit-testable in isolation
   (REQ-VAL-002/REQ-CG-002 layering makes this possible by construction).
3. **DependencyNetwork** (`06`) — adjacency, cycle rejection, validation.
   Fully unit-testable with synthetic nodes; no ESMF involvement needed.
4. **ComponentGraph** (`07`) — lifecycle (initialize/freeze), still with
   synthetic/test data; no `GraphBuilder` yet.
5. **GraphBuilder minimal slice**: advertising + ordinary connection
   resolution + extension reuse (`08`, `09`) — this is the first point
   where existing MAPL coupler behavior must be reproduced *exactly*
   (REQ-OBJ-002, REQ-EXT-001..005). Recommend running this slice
   side-by-side with the existing imperative coupler code on real
   configurations and diffing results before removing the old path.
6. **NodeRevision + demand-driven update** (`11`) — introduce lazy
   execution once ordinary wiring is proven equivalent to today's eager
   behavior.
7. **MethodGraphNode + GriddedComponentDriver integration** (`12`) — this
   is where phases/SetServices start flowing through the graph.
8. **Callbacks** (`15`) — new capability, no legacy behavior to match, so
   lower migration risk despite being conceptually the most elaborate
   piece; can proceed once (1)-(7) are stable.
9. **Geometry/vertical grids/route handles as GraphValues** (`13`, `14`)
   — recommend last among the "settled" features, since it likely
   requires touching the most existing special-cased code
   (REQ-GEO-001's stated goal of *replacing* existing geometry-inheritance
   logic), so should happen once the graph plumbing around it is
   well-exercised by (1)-(8).
10. **Inout items** (`16`) and **compilation** (`11` §11.5 / Q9) —
    deliberately last; both are explicitly speculative/unresolved in this
    snapshot and should not gate anything above.

**Confidence:** high on ordering logic (dependency-driven: each step only
needs what came before); the specific point at which "old imperative
coupler code is deleted" (end of step 5, or later, kept as a fallback
until step 6+ is trusted) is a judgment call for the implementation team,
not something this document can settle.

**Spec author:** OK with this ordering.

---

## Q11 — Does the GraphStateItem amendment supersede the polymorphic GraphValue hierarchy?

**Problem restated:** `04-graph-value-hierarchy.md` §4.6 proposes that
`StateItemNode`'s payload become one concrete `GraphStateItem` (allocatable
`esmf_field`/`esmf_field_bundle`/`esmf_state`/`esmf_route_handle`, at most
one allocated) instead of one polymorphic `GraphValue`. `GraphStateItem` has no
corresponding component for `GridValue`/`MeshValue`/`LocStreamValue`/
`VerticalGridValue` (§4.2), and no explicit component for the member-name →
`NodeId` maps currently specified on `FieldBundleValue`/`StateValue`
(REQ-VAL-003–007).

**Recommendation:** resolve this before implementing either §3.1a or §4.6.
Two sub-decisions are needed:

1. **Membership maps.** Add them as `GraphStateItem` components active only for
   the `field_bundle`/`state` kinds (simplest; keeps `GraphStateItem` as the one
   place all `StateItemNode` structure lives), rather than a side table
   keyed by `NodeId`. A side table reintroduces exactly the kind of
   external-bookkeeping-vs-on-object tension already flagged in Q3; prefer
   consistency with the "put it in the object unless it must be
   cross-network" precedent from that discussion — membership is not a
   cross-network concern, so it belongs on `GraphStateItem`. Still open.
2. **Geometry kinds — resolved by the component's author (Grid/Mesh/
   LocStream/XGrid sub-case).** Earlier drafts of this recommendation went
   through two readings — first "superseded by `GeomCharacteristic`-as-
   metadata" (geometry stops being a node at all), then walked back to
   "additive" (geometry stays a node, of some unspecified kind). The
   question of *which node kind* is now answered directly: geometry needed
   inside a `GraphStateItem`-bearing `StateItemNode` is carried as an incomplete
   `esmf_field` (`ESMF_FIELDSTATUS_GRIDSET`) — an ordinary `GraphStateItem` with
   its `esmf_field` component allocated but not yet data-complete. No new
   node kind, and no `GeomValue`-as-`GraphStateItem`-component, is needed for
   this case. `GeomCharacteristic` (`18-state-item-characteristics.md`
   §18.2.1) is a `ReferenceCharacteristic` holding that ordinary
   `StateItemNode`'s `NodeId`. `GeomValue` (`04-graph-value-hierarchy.md`
   §4.2a, corrected to one type covering `Grid`/`Mesh`/`LocStream`/`XGrid`)
   remains the right shape under the *original* (§4.1–§4.5) reading, and is
   the documented fallback if the `esmf_field` approach proves problematic.
   **Not yet separately confirmed:** whether `VerticalGridValue` collapses
   the same way — it already references three other `StateItemNode`s by
   `NodeId` (REQ-GEO-007), which is in the same spirit, but has not been
   explicitly addressed the way the Grid/Mesh/LocStream/XGrid case has.
   This still has not been checked line-by-line against
   `13-geometry-and-vertical-grids.md` REQ-GEO-002/008 or
   `14-route-handles.md`'s `RouteHandleKey` geometry references.

**Confidence:** high on sub-item 2's Grid/Mesh/LocStream/XGrid case (settled
by the spec author) and on sub-item 1 (membership maps, confirmed below);
the `VerticalGridValue` follow-on was spun off as Q14 and is itself
resolved (see below).

**Spec author's response (confirmed):**

- **Sub-decision 1 (membership maps):** agreed, as recommended.
- **Sub-decision 2 (`VerticalGridValue`):** does NOT collapse the same way
  as Grid/Mesh/LocStream/XGrid. Vertical grid is more complex — see the
  corrected model in `13-geometry-and-vertical-grids.md` §13.3 (a
  `VerticalGrid` holds multiple coordinate sets, each an independent
  `ESMF_Field`/`GraphStateItem`, keyed by physical dimension, not a fixed
  `PLE`/`ZLE` pair). This likely requires **generalizing the `GraphStateItem`
  derived-type concept** itself (beyond the current four fixed
  `esmf_field`/`esmf_field_bundle`/`esmf_state`/`esmf_route_handle`
  components) rather than fitting `VerticalGrid` into the existing shape
  unchanged. Not designed yet — tracked as **Q14** below rather than
  folded silently into the existing `GraphStateItem` structure.
- **General note (new, applies beyond just geometry):** additional
  metadata will in general be needed to distinguish use cases that share
  the same underlying ESMF representation — e.g. a `TimeInterpolationBracket`
  and a vector quantity are both plain `FieldBundle`s, disambiguated today
  only via `ESMF_Info` metadata, not by type. The same `Info`-based
  technique could disambiguate Geom roles (`Info` on the proxy Field,
  `13-geometry-and-vertical-grids.md` REQ-GEO-003) and possibly
  `VerticalGrid` itself — representing a `VerticalGrid` as an `ESMF_State`
  (members = physical-dimension coordinate-set Fields) rather than a
  bespoke payload type is now noted as a plausible alternative (see
  `13-geometry-and-vertical-grids.md` §13.3.2). Not decided.

**Formalized.** Q11 is now fully resolved: sub-item 2 (Grid/Mesh/LocStream/
XGrid) via `04-graph-value-hierarchy.md` §4.6.4; sub-item 1 (membership
maps) promoted into `04-graph-value-hierarchy.md` REQ-SI-006; the
`VerticalGridValue` follow-on resolved separately as Q14. No part of Q11
remains open.

**Further formalized (later pass).** The "general note" above (additional
metadata needed to distinguish use-cases sharing one ESMF representation,
e.g. Bracket-vs-Vector `FieldBundle`) is now a binding, typed mechanism
rather than an ad hoc `Info`-tagging idea: `04-graph-value-hierarchy.md`
REQ-SI-002b defines a two-tier `esmf_kind()`/`variant()` classification,
with `variant()` returning a new, extensible `MAPL_StateItem_Flag`. This
same pass also removed the `esmf_route_handle` component entirely
(REQ-SI-002/002a) — a RouteHandle is now just `esmf_state` in its
wrapper role, distinguished via `esmf_kind() ==
ESMF_STATEITEM_ROUTEHANDLE` by delegating to the wrapper's own sole
member's native ESMF classification, not via a `GraphStateItem`-specific tag.

---

## Q12 — Should StateItemCharacteristic instances be graph nodes?

**Resolved (partially), through three rounds of discussion — recorded here
so the reasoning isn't lost:**

**Round 1 (original recommendation, now superseded):** make
`StateItemCharacteristic` itself a graph node, using its
`DependencyNetwork` successors as a "listener" set, so change propagation
reuses `NodeRevision.advance()` + demand-driven update uniformly.

**Round 2 (counter-example that broke round 1):** a component's `Run`
phase may mutate a shared geometry characteristic and *immediately*, in
the same call, expect to populate correctly-shaped export Fields. There is
no graph-traversal boundary between mutation and use for demand-driven
(pull) update to hook into — control never returns to the framework
between them. A purely lazy/pull answer cannot satisfy this.

**Round 3 (rejected alternative):** require geometry mutation to happen
only in a dedicated, framework-recognized "ChangeGeom" phase, giving pull a
boundary to anchor to. Rejected for two concrete implementability reasons —
see `18-state-item-characteristics.md` §18.9 for the full record: (1) no
existing mechanism to mark a phase as special in `SetServices`
registration; (2) no way to constrain user code inside such a phase (it
could call a child's methods directly, defeating the invariant the phase
was supposed to provide).

**Adopted resolution:** split the question by *what kind* of
characteristic and *what kind* of consequence (`18-state-item-characteristics.md`
§18.2.1, §18.8):

- `ReferenceCharacteristic`s (Geom, VerticalGrid) DO reference real graph
  nodes with `NodeId`/`NodeRevision` identity — sharing is ordinary
  multiple-`GraphStateItem`s-hold-the-same-`NodeId`, no new identity mechanism.
  `ValueCharacteristic`s (Units, TypeKind) are never shared and need none
  of this.
- The `StateItemCharacteristic` object itself is NOT what gets
  notified/notifies — it is not a bespoke observer node. Structural
  consequences (Field reallocation) are handled by an eager, synchronous,
  non-reentrant mutator call (§18.8 REQ-CHAR-016/017) that never invokes
  user code, so it's safe to call from any context without the framework
  needing to recognize anything as special. Content consequences (Transform
  recompute) stay exactly as lazy/pull as before (REQ-REV-006), now
  correctly anchored via REQ-REV-011 (`11-revision-and-update.md` §11.4a).

**Round 4 (fills the node-kind gap this section originally left open):**
the earlier version of this section asked "if characteristics [or the
things they reference] are nodes, what `GraphNode` subclass are they — a
`StateItemNode` variant, or a new sibling?" Answer, confirmed by the spec
author while reviewing §4: no new node kind. A `ReferenceCharacteristic`
for geometry references an ordinary `StateItemNode` whose `esmf_field` is
a geometry proxy in `ESMF_FIELDSTATUS_GRIDSET` (`04-graph-value-hierarchy.md`
§4.6.4). This also removes the REQ-DEP-008 concern raised in earlier
drafts of this answer (producer-uniqueness accounting for a hypothetical
new node kind) — it's an ordinary `StateItemNode`, subject to the same
rules as any other.

**Still open:** the precise mechanism for identifying "direct structural
dependents" in REQ-CHAR-016 step 2 — within one `ComponentGraph`, dispatch
on `DependencyNetwork` successor node-kind is a natural fit; across
`ComponentGraph` boundaries, a parent-orchestrated recursive walk through
existing proxy/port wiring is sketched but not fully worked out
(`18-state-item-characteristics.md` §18.8, final `[OPEN]` block).

**Confidence:** high on the node-kind answer (Round 4, settled by the spec
author); medium-high on the overall adopted resolution's shape (eager
structural / lazy content split, no bespoke observer node); low-medium on
the remaining cross-`ComponentGraph` walk mechanics.

**Spec author:** OK with the overall resolution.

---

## Q13 — Characteristic transform-insertion-order delegation mechanism

**Problem restated:** `18-state-item-characteristics.md` §18.6 distinguishes
two concerns that were originally conflated: *detection completeness*
(iterate all characteristics, agnostic of `ValueCharacteristic`/
`ReferenceCharacteristic` kind — this part is now settled, REQ-CHAR-009)
and *transform-insertion order* (given two or more mismatches, what order
are their reconciling Transforms chained in). This question is about the
latter only. `GraphStateItem` must expose the insertion order, delegating the
decision to "something that varies by the `GraphStateItem`'s active
kind/subtype," but the delegation mechanism itself is not specified.

**Recommendation:** favor a small per-kind strategy lookup (`kind()` →
ordered list of `CharacteristicType`) over a method living on one
designated "primary" characteristic, because the latter implies one
characteristic subclass has to know about all others' relative priority,
which fights the "each characteristic is independently adaptable" framing
of REQ-CHAR-001. A static per-kind table is simpler to reason about, easier
to extend when a new `CharacteristicType` is registered (REQ-CHAR-006), and
does not require picking an arbitrary "primary" characteristic for kinds
that may not obviously have one (e.g., a bare `RouteHandle`-kind
`GraphStateItem`).

**Confidence:** medium. No concrete comparison use case has been worked
through end-to-end (e.g., "given this import `GraphStateItem` and this export
`GraphStateItem`, in what order do we discover mismatches and build the
extension chain") to validate that a static per-kind table is actually
sufficient.

**Spec author:** OK with the recommendation, but expects this will likely
need revisiting once a concrete implementation is attempted.

---

## Q14 — Generalizing `GraphStateItem` for `VerticalGrid`

**Problem restated:** `13-geometry-and-vertical-grids.md` §13.3 corrects
the model of a `VerticalGrid` to "one or more coordinate sets, each an
independent `ESMF_Field`/`GraphStateItem`, keyed by physical dimension." This
does not fit the current `GraphStateItem` (§4.6, REQ-SI-002) unchanged — the
four allocatable components (`esmf_field`/`esmf_field_bundle`/`esmf_state`/
`esmf_route_handle`, at most one allocated) have no place for "a set of
named `Field` references, one per physical dimension." Unlike the
Grid/Mesh/LocStream/XGrid case (resolved in Q11 via the geometry-proxy
`esmf_field`), the spec author confirmed `VerticalGrid` genuinely does not
collapse the same way.

**Candidate directions, not yet evaluated against each other:**

1. Represent `VerticalGrid` as an `ESMF_State` (§13.3.2) — members are the
   physical-dimension coordinate-set Fields, addressed by member name.
   This would let `VerticalGrid` use the *existing* `esmf_state` component
   of `GraphStateItem` as-is, with no `GraphStateItem` structural change at all —
   the "generalization" would then be entirely at the naming/metadata
   level (distinguishing a `VerticalGrid`-kind `esmf_state` from an
   ordinary nested state, likely via `ESMF_Info` per the
   Bracket-vs-Vector precedent noted in §13.3.2), not a type-structure
   change.
2. Add a fifth `GraphStateItem` component specifically for "map of physical
   dimension → `NodeId`" — closer to how `FieldBundleValue`/`StateValue`
   membership maps were being discussed for Q11's sub-item 1, but
   `GraphStateItem` was deliberately built as *at most one of four* allocated
   components (REQ-SI-002/004), so adding a fifth changes that invariant's
   shape, not just its count.

Direction 1 is attractive precisely because it requires no change to
`GraphStateItem` itself — worth ruling out first before considering any
structural change to `GraphStateItem`.

**Not yet decided.** Flagging as a genuinely new open question, distinct
from Q11 (which is about whether `GraphStateItem` needs new *node-kind*
representation for geometry; this is about whether `GraphStateItem` needs new
*component-structure* for vertical grids specifically).

**Resolved by the spec author: Direction 1.** `VerticalGrid` is represented
as an `ESMF_State` — its members are the physical-dimension coordinate-set
Fields, addressed by member name. This requires no structural change to
`GraphStateItem` at all: a `VerticalGrid`-kind item simply allocates the
existing `esmf_state` component (REQ-SI-002), distinguished from an
ordinary nested state via `ESMF_Info` metadata (per the
Bracket-vs-Vector-style disambiguation already noted in §13.3.2), not via
a new `GraphStateItem` component. Direction 2 (a fifth allocatable component) is
not adopted.

**Confidence:** high — settled by the spec author.

**Further formalized (later pass).** "`ESMF_Info` metadata" above is now
the typed `variant()` query, value `MAPL_STATEITEM_VERTICALGRID`
(`04-graph-value-hierarchy.md` REQ-SI-002b).

---

## Q15 — Visualization export: DOT/JSON schema

**Problem restated:** `19-visualization-export.md` REQ-VIZ-008/009 commit
to DOT as primary output and an equal-content JSON as secondary, but
REQ-VIZ-009a deliberately leaves exact attribute conventions (shape/color
per node kind, edge style per network) and exact JSON field names/nesting
unspecified.

**Recommendation:** do not fix the schema in the spec text. Let a
reference implementation define it, document it alongside the exporter
code, and version it. Rationale: node/edge metadata needs (§19.5) are
already contingent on two other open questions (payload representation,
Q11; port-binding storage, Q3) — freezing a schema now risks churn as soon
as either of those settles.

**Confidence:** medium-high on "don't fix it yet"; no recommendation
offered on the schema's actual content.

---

## Q16 — Visualization export: repeated/time-series snapshots

**Problem restated:** `19-visualization-export.md` REQ-VIZ-007 requires
export to work at least once after freeze (final structural topology).
REQ-VIZ-007a asks whether export should also be callable repeatedly across
runtime (post-freeze structure is fixed per REQ-CG-006, but revision/status
metadata, §19.7, changes over time) to support an animated or time-series
view of graph activity.

**Recommendation:** treat as speculative until a concrete use case
appears. The mechanism (REQ-VIZ-006) MUST NOT be designed in a way that
precludes repeated calls (e.g. no hidden single-shot assumption, no
resource that can only be acquired once), but building actual
time-series/animation tooling ahead of a real need would be speculative
work with no current consumer.

**Confidence:** medium. Low cost either way (the "don't preclude it" bar
is cheap to satisfy); the open part is purely whether to build more than
that now.

---

## Q17 — Visualization export: port-binding labels vs. port binding storage location — **RESOLVED**

**Problem as originally stated:** `19-visualization-export.md` REQ-VIZ-012
wants Transform port bindings shown as edge labels, but where those
bindings are stored was itself unresolved (`06-dependency-network.md` §6.6,
Q3). Building the port-label feature against on-node storage vs. an
externally-keyed table could have meant throwaway work if Q3 resolved the
other way.

**Resolved by Q3's resolution.** Port binding storage is now settled as an
external `(DependencyNetworkId, NodeId) → port name → NodeId` table
(`10-transforms-and-ports.md` REQ-XFORM-005). REQ-VIZ-012
(`19-visualization-export.md`) has been updated accordingly: port-binding
edge labels are a normal part of export now, with graceful degradation
(omit label, keep edge) retained only for the ordinary case of a
node/network pair genuinely absent from the table — not as a hedge
against an unresolved storage question.

**Confidence:** high.

---

## Q18 — Visualization export: hierarchy-wide (multi-ComponentGraph) export scope for v1.0

**Problem restated:** `19-visualization-export.md` REQ-VIZ-005 describes
composing per-`ComponentGraph` exports at proxy-node boundaries to produce
a hierarchy-wide view, consistent with `02-component-hierarchy.md`
REQ-HIER-005/006 (no reaching into a child's internal nodes/networks
directly). Whether this composition is needed for v1.0, or whether
per-component export alone is sufficient until the future global graph
view (`02-component-hierarchy.md` §2.4) materializes, is not settled.

**Recommendation:** per-component export alone is likely sufficient for
v1.0 — it already covers the debugging use case within one component's
own wiring (extension chains, misconnected ports), which is the more
common failure mode than "child ports don't line up with parent
proxies," and it requires no cross-graph stitching logic. Defer
hierarchy-wide composition until the global graph view (§2.4) is itself
undertaken, at which point the stitching logic likely serves both needs
at once rather than being built twice.

**Confidence:** medium. Depends on how often cross-component wiring bugs
turn out to be the actual pain point in practice — not yet known.

---

## Summary: settled vs. speculative vs. open, at a glance

| Area | Status |
|---|---|
| ESMF constraints | Settled (external facts) |
| One ComponentGraph per OuterComponent | Settled |
| GraphNode / GraphValue hierarchies | Settled |
| NodeId + ID template | Settled |
| No first-class edges, adjacency-list DependencyNetwork | Settled |
| ComponentGraph lifecycle (init/freeze) | Settled |
| GraphBuilder layering & responsibilities | Settled |
| Extension reuse / backward compatibility | Settled (compatibility requirement) |
| Transform node model (named ports) | Settled |
| Port-binding storage location | **Resolved (Q3)**: external table, `10-transforms-and-ports.md` REQ-XFORM-005 |
| NodeRevision type + update algorithm | Settled |
| Composite-value revision granularity | **Open (Q8)** |
| Revision storage relocation into ESMF Info | Speculative |
| Runtime interpretation | Settled (near-term) |
| Compilation to direct wrappers | Speculative (Q9) |
| MethodGraphNode concept | Settled |
| Invocation adapter design | Open (Q2) |
| GriddedComponentDriver | Settled |
| SetServices/init lifecycle ordering | Settled |
| Geometry/vertical grid as GraphValue | Settled concept; renewal-under-freeze open |
| RouteHandle sharing model | Settled |
| RouteHandle time-dependent renewal | Open |
| CallbackInterface / registry data model | Settled |
| Handler/Invoker distinction | Settled; term choice open (Q6) |
| Placement-vs-semantics (StateIntent) | Open (Q7) |
| Wildcard callback aggregation mechanism | Settled (reuses existing flattening); pattern syntax Settled as regex (Q5) |
| Global graph view | Speculative, out of scope v1.0 |
| Ordinary inout items | **Deferred** (§16, explicitly set aside) |
| Parent-invokes-child-method as parent-graph dependency (Q1) | Settled: NOT represented, for mid-Run invocation; init-phase invocation may still be |
| Same-node double-write within one update pass | Settled: forbidden across all networks (REQ-DEP-008a) |
| Compilation scope under per-component-graph model (Q9) | Settled: per-component compiled form is the compiled form; no separate global-graph compilation step |
| ValueGraphNode → StateItemNode rename | Settled (nominal) |
| BaseGraphNode stores own NodeId | Settled (REQ-NODE-002a, reversed from original position) |
| BaseGraphNode adjacency caching | Settled: still excluded, reconsidered and kept as-is |
| GraphStateItem as concrete StateItemNode payload | Speculative container decision; **Q11 resolved** (Grid/Mesh/LocStream/XGrid sub-item high confidence; membership maps now REQ-SI-006; VerticalGrid follow-on spun off as Q14, itself resolved) |
| GeomValue (replaces GridValue/MeshValue/LocStreamValue, adds XGrid) | Settled |
| VerticalGrid representation | Settled: represented as ESMF_State, no GraphStateItem change needed (Q14) |
| RouteHandle representation (`esmf_state` in wrapper role, no separate component) | Settled: persistent wrapper ESMF_State, not a bare handle (REQ-CG-010/REQ-SI-002a/002b) |
| StateItemCharacteristic hierarchy, incl. Value/Reference split | Settled direction (speculative pending Q11) |
| CharacteristicStatus / CharacteristicType naming | Open (naming only, §18.3/§18.4) |
| StateItemCharacteristic as graph node | Resolved: no bespoke observer node; eager structural / lazy content split instead (Q12); cross-graph walk mechanics still open |
| MethodGraphNode invocation triggers argument update() | Settled (REQ-REV-011), with concrete default-network/callback-network trigger points specified |
| Dedicated "ChangeGeom" phase | Rejected (§18.9) |
| Characteristic transform-insertion order | **Open (Q13)** |
| Characteristic sharing + structural propagation | Settled direction (eager mutator, no listener list); cross-graph mechanics open |
| Shared-data (non-alias) import payload for NUOPC | Speculative extension (REQ-EXT-002a) |
| GraphStateItem RouteHandle-wrapper (esmf_state role) shallow-copy safety | Settled (REQ-CG-009/010) |
| ComponentGraph structure constructor | Settled: MAY be used (reverses original position, REQ-CG-003) |
| MAPL init phase list (advertise/modify_advertised/realize-accept-realize cycle/read_restart/user_specific) | Settled (REQ-MTH-011), convergence algorithm still open |
| Visualization export: DOT/JSON schema | **Open (Q15)** — deliberately deferred to reference implementation |
| Visualization export: repeated/time-series snapshots | **Open (Q16)** — speculative pending use case; mechanism must not preclude it |
| Visualization export: port-binding labels vs. port-binding storage location | **Resolved (Q17)**: Q3's resolution settles this — port-binding labels implementable now |
| Visualization export: hierarchy-wide (multi-ComponentGraph) export scope | **Open (Q18)** — leaning per-component-only sufficient for v1.0 |
