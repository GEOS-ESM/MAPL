# 18. StateItemCharacteristic

Status: `[SPECULATIVE]` overall direction; `[SETTLED]` (informally — see below)
the Value/Reference split (§18.2), agnostic extension-building iteration
(§18.6), and eager-structural/lazy-content split for shared characteristics
(§18.8); `[OPEN]` naming of `CharacteristicStatus`/`CharacteristicType`
(§18.3, §18.4), transform-insertion order (§18.6, Q13), and some mechanics
of §18.8. This entire document depends on the `GraphStateItem` amendment in
`04-graph-value-hierarchy.md` §4.6, which is itself `[OPEN]` (Q11). Do not
implement ahead of that resolution.

## 18.1 Motivation

A `GraphStateItem` (§4.6) can mismatch its connection partner along several
independent, atomic axes — physical units, type/kind (precision), geometry,
vertical grid, and so on. Today (pre-graph and in the polymorphic-`GraphValue`
reading of this spec) these mismatches are handled by ad hoc extension
chains (`09-extension-reuse.md`). `StateItemCharacteristic` proposes making
each such axis a first-class, inspectable, independently-adaptable object
attached to a `GraphStateItem`, so that:

- mismatch detection is a per-characteristic comparison, not a monolithic
  "does this GraphStateItem match that one" check
- the `Transform` needed to reconcile a mismatch is determined by *which*
  characteristic differs, not by a bespoke case analysis
- characteristics that are legitimately shared across `GraphStateItem`s (Geom,
  VerticalGrid — §18.7) have one place to carry that sharing semantics

## 18.2 StateItemCharacteristic hierarchy

**REQ-CHAR-001.** `StateItemCharacteristic` MUST be an abstract type. Known
concrete subclasses (minimum set; not exhaustive):

| Subclass | Adapted by (Transform) | Notes |
|---|---|---|
| `PhysicalUnitsCharacteristic` | `ConvertUnitsTransform` | e.g. `kg/kg` → `1` |
| `TypeKindCharacteristic` | `CopyTransform` | precision/type-kind conversion; `CopyTransform` is a placeholder name — flagged poor, needs a better one before implementation |
| `GeomCharacteristic` | `RegridTransform` | uses a shared `RouteHandle`, see `14-route-handles.md` |

**REQ-CHAR-002.** Each `StateItemCharacteristic` subclass MUST be adaptable
by exactly one associated `TransformGraphNode` subclass (the table above),
consistent with the existing Transform model (`10-transforms-and-ports.md`).
`StateItemCharacteristic` itself MUST NOT perform the adaptation; it
describes the characteristic's current value/status, and the associated
Transform performs the reconciliation.

**`[OPEN]`** This minimum set of three is almost certainly incomplete
(precision aside, there is no listed characteristic for e.g. staggering,
halo width, or time-averaging convention). Treat the list as a starting
point, not a closed set. Adding a new characteristic subclass MUST NOT
require changes to `GraphStateItem` or `CharacteristicType`'s consumers beyond
registering a new `CharacteristicType` value (§18.4).

**`[OPEN]`** `CopyTransform` is called out by name as a poor placeholder.
Do not treat the name as final.

### 18.2.1 Value characteristics vs. reference characteristics

**REQ-CHAR-002a.** `StateItemCharacteristic` MUST branch into two abstract
intermediate kinds, distinguished by whether the characteristic owns its
value or points at a shared one:

- **`ValueCharacteristic`** — holds its value inline; not shared; no
  identity beyond the owning `GraphStateItem`. `PhysicalUnitsCharacteristic` and
  `TypeKindCharacteristic` are `ValueCharacteristic`s.
- **`ReferenceCharacteristic`** — holds a `NodeId` referencing a shared
  `StateItemNode`-or-equivalent elsewhere in the graph, rather than a value
  of its own. `GeomCharacteristic` (and, prospectively, an unnamed
  vertical-grid characteristic — `13-geometry-and-vertical-grids.md`) are
  `ReferenceCharacteristic`s.

**Rationale:** these two kinds have genuinely different runtime needs —
`ValueCharacteristic`s never need sharing, identity, or change propagation
beyond their own `GraphStateItem`; `ReferenceCharacteristic`s always do (§18.7,
§18.8). Modeling this as a real subtype split, rather than leaving it
implicit per-subclass behavior, keeps that difference visible in the type
system instead of buried in documentation.

**REQ-CHAR-002b.** A `ReferenceCharacteristic`'s referenced `NodeId` MUST
point at an ordinary graph node with its own identity and `NodeRevision` —
the `ReferenceCharacteristic` itself does not carry the payload, only the
reference plus its own `CharacteristicStatus`. For `GeomCharacteristic`
specifically, this is resolved (`04-graph-value-hierarchy.md` §4.6.4): the
referenced node is an ordinary `StateItemNode` whose `esmf_field` component
is a geometry proxy field (REQ-GEO-003) held in
`ESMF_FIELDSTATUS_GRIDSET` — grid/mesh/locstream/xgrid associated, data
array not yet allocated. No new node kind is introduced for this purpose.

## 18.3 CharacteristicStatus `[OPEN — name and exact semantics]`

**REQ-CHAR-003.** Every `StateItemCharacteristic` MUST carry a status value
from a parameterized enumeration (working name `CharacteristicStatus`; name
not settled) with at least these values:

| Value | Meaning |
|---|---|
| `INVALID` | No meaningful value yet; equivalent in spirit to `NodeRevision`'s invalid state (`11-revision-and-update.md` §11.1). |
| `SPECIFIED` | Fully resolved; the characteristic's value is authoritative and usable as-is. |
| `MIRRORED` | Not yet resolved; will be made to match the corresponding characteristic of a source once connected. Transitions to `SPECIFIED` at that point. |
| `UNCHECKED` | Connection is allowed without a reconciling Transform even though the characteristic is not confirmed to match. Permits deliberately sloppy use cases where some metadata is unknown. **Dangerous** — MUST be clearly distinguished from `SPECIFIED`/`MIRRORED` in diagnostics, since it is an explicit opt-out of validation. |
| `DEFERRED` | Further processing cannot happen yet (e.g., needed information is not available at this initialization phase). A later init phase is expected to update the status. Distinct from `INVALID`: `DEFERRED` implies a known reason and an expected future resolution point; `INVALID` implies "not yet touched." |

**REQ-CHAR-004.** A `ReferenceCharacteristic`'s status transition away from
`SPECIFIED` (e.g. its referenced node's value changing) MUST be reflected
by the propagation mechanics in §18.8, not silently. A newly-reallocated
dependent (§18.8) MUST have its own status/revision reset to `INVALID`
until repopulated — this reuses the ordinary invalid-state concept
(REQ-REV-001) rather than introducing a new one.

**`[OPEN]`** Exact name (`CharacteristicStatus` is a working name only) and
whether additional values are needed (e.g. an explicit `ERROR`/`CONFLICT`
status distinct from `INVALID`) are not settled.

## 18.4 CharacteristicType `[OPEN — name]`

**REQ-CHAR-005.** A type (working name `CharacteristicType`; name not
settled) MUST provide one unique, stable value per concrete
`StateItemCharacteristic` subclass. It is the key type for the map in
§18.5 and MUST follow the same encapsulated-identity pattern used elsewhere
in this spec (cf. `05-identities.md` REQ-ID-001, though `CharacteristicType`
identifies a *subclass*, not a per-instance identity — it is closer to a
type-tag enumeration than to `NodeId`).

**REQ-CHAR-006.** Introducing a new `StateItemCharacteristic` subclass
(§18.2) MUST correspond to registering exactly one new `CharacteristicType`
value. `CharacteristicType` values MUST be stable once assigned (same
"never renumber" discipline as `REQ-<AREA>-<NNN>` IDs, README Conventions).

## 18.5 GraphStateItem.characteristics map

**REQ-CHAR-007.** `GraphStateItem` (§4.6) MUST contain a component:

```
characteristics : map<CharacteristicType, StateItemCharacteristic>
```

**REQ-CHAR-008.** This map MUST be sparse: a `GraphStateItem` need only contain
entries for characteristics that are meaningful for its allocated kind
(REQ-SI-003) and that have actually been established or deferred. Absence
of a `CharacteristicType` key MUST be distinguished from presence with
status `INVALID` — `[OPEN]` which of these two representations
(absent-key vs. present-with-`INVALID`) is canonical; do not assume they
are used interchangeably without deciding.

## 18.6 Extension-building algorithm and comparison order

**REQ-CHAR-009.** The algorithm that detects mismatches and builds a
reconciling extension chain (`09-extension-reuse.md`) MUST iterate over
*all* entries in a `GraphStateItem`'s `characteristics` map through the common
`StateItemCharacteristic` interface only. It MUST NOT branch on whether a
given characteristic is a `ValueCharacteristic` or `ReferenceCharacteristic`
(§18.2.1) — that distinction is an implementation detail of each subclass,
invisible to the detection/extension-building algorithm. This is settled:
the A/B split is a subtyping convenience, not a license for the algorithm
to special-case by kind.

**REQ-CHAR-010.** `GraphStateItem` MUST expose a method returning the order in
which mismatching characteristics should have their reconciling Transforms
inserted into the extension chain. This is a distinct concern from
REQ-CHAR-009: REQ-CHAR-009 is about *detection completeness* (don't skip
any characteristic); this is about *sequencing* (given two or more
mismatches, in what order are `T1`, `T2`, ... chained per
`09-extension-reuse.md` REQ-EXT-001) — e.g. whether unit conversion happens
before or after regridding, or before or after a precision change, may
matter for correctness or efficiency even though detection itself doesn't
care about the order characteristics are found in.

**REQ-CHAR-011.** This ordering method MUST delegate to something that
varies by the `GraphStateItem`'s active kind/subtype (working description: "the
subtype characteristic" — exact mechanism not settled).

**`[OPEN]`** The precise delegation mechanism (a method on a per-kind
strategy object? a static table keyed by `kind()`?) is not specified. Do
not implement a specific mechanism without resolving this first — see
`17-open-questions.md` Q13.

## 18.7 Sharing across GraphStateItems

**REQ-CHAR-012.** A `ReferenceCharacteristic` (§18.2.1) MAY be shared —
i.e., multiple `GraphStateItem`s' `characteristics` maps hold a
`ReferenceCharacteristic` referencing the *same* underlying `NodeId`. This
is expected to be common for `GeomCharacteristic` and a vertical-grid
characteristic, because many `GraphStateItem`s legitimately share the same
geometry or vertical grid, including in time-dependent cases
(`13-geometry-and-vertical-grids.md` §13.4, `14-route-handles.md` §14.4).

**REQ-CHAR-013.** `ValueCharacteristic`s are never shared this way — sharing
is a `ReferenceCharacteristic`-only concern (§18.2.1's rationale).

**REQ-CHAR-014.** A `ReferenceCharacteristic` references an ordinary graph
node that itself has real `NodeId`/`NodeRevision` identity (reaffirming,
via `17-open-questions.md` Q11's resolution, that geometry keeps node
identity rather than becoming pure `GraphStateItem` metadata with none). For
`GeomCharacteristic` specifically, that referenced node is an ordinary
`StateItemNode` whose `esmf_field` is a `GRIDSET`-only geometry proxy field
(`04-graph-value-hierarchy.md` §4.6.4) — not a separate `GeomValue` node
kind. A `GeomCharacteristic` is a thin `NodeId`-holding reference to that
node, not a redefinition of what geometry *is*. Sharing is then nothing more than
several `GraphStateItem`s' map entries holding the same `NodeId` — no new
identity mechanism required (`05-identities.md`).

## 18.8 Propagating a shared-characteristic change: structural (eager) vs. content (lazy)

**REQ-CHAR-015.** A change to a shared `ReferenceCharacteristic`'s
underlying value has two independent consequences, with two independent
timing disciplines, and this document settles that they MUST be kept
separate:

- **Structural correctness** — a dependent `GraphStateItem`'s ESMF payload (e.g.
  an `esmf_field`) has storage matching the new characteristic (e.g. the
  right grid/shape). This MUST be resolved *eagerly*, synchronously, as
  part of the operation that changes the characteristic.
- **Content correctness** — a dependent `GraphStateItem`'s *values* are
  up to date (e.g. a regridded field's actual numbers). This MAY remain
  *lazy*, resolved by the existing demand-driven update algorithm
  (`11-revision-and-update.md` REQ-REV-006) the next time it's actually
  requested.

**Rationale — why structural correctness cannot be lazy:** a user
GridComp's `Run` phase may change a shared geometry characteristic and then
*immediately*, within the same call, expect to populate its own export
Fields with a correctly-shaped data array. There is no graph-traversal
boundary between the mutation and that immediate use for the demand-driven
algorithm to hook into — control never returns to the framework between
them. Anchoring structural correctness to some later "requested output"
event (as REQ-REV-006 implicitly assumes for ordinary Transform-produced
values) does not work for this case.

**REQ-CHAR-016.** Structural correctness MUST be handled by a dedicated
mutator entry point (e.g. a `MAPL_SetGeom`-style wrapper) that, in one
synchronous call:

1. Updates the shared characteristic's value.
2. Walks its direct structural dependents (mechanism for identifying these
   is `[OPEN]` — see below) and, for each, performs a pure structural reset
   (e.g. `ESMF_FieldEmptyReset` for an `esmf_field`), resetting that
   dependent's own status/revision to `INVALID` (REQ-CHAR-004).
3. Advances the shared characteristic's own `NodeRevision`, so that
   content-side dependents (Transforms, per REQ-REV-006) are recognized as
   stale the next time they are demanded.

**REQ-CHAR-017.** The walk in REQ-CHAR-016 step 2 MUST NOT invoke any
`MethodGraphNode` (no user code, no callback dispatch, no child-component
method calls) — it is restricted to pure structural resets on `GraphStateItem`
payloads. This is load-bearing, not a style preference: it is what makes
the mutator call safe to invoke from *anywhere* (any phase, any nesting,
including from inside another component's `Run`), without the framework
needing to recognize a "special" calling context or constrain what
surrounding user code does. See §18.9 for the alternative this replaces and
why that alternative was rejected.

**`[OPEN]`** How REQ-CHAR-016 step 2 identifies "direct structural
dependents" is not fully settled:

- Within one `ComponentGraph`, the referenced node's direct successors in
  its `DependencyNetwork` that are themselves `StateItemNode`s (as opposed
  to `TransformGraphNode`s, which are left to the lazy path) are a natural
  candidate — no new registration structure needed, dispatch by node kind
  at walk time.
- Across `ComponentGraph` boundaries (geometry shared with a sibling or
  descendant `OuterComponent` through the existing port/proxy chain,
  `02-component-hierarchy.md` §2.3), the walk must be orchestrated
  top-down by a parent's driver access to its children (REQ-HIER-003,
  REQ-MTH-008) recursing into each descendant's local proxy `StateItemNode`
  — still restricted to structural resets only (REQ-CHAR-017), never
  invoking a descendant's methods. This cross-graph recursive walk is more
  machinery than the single-graph case and has not been fully worked out.

**REQ-CHAR-018.** Content-side propagation requires no new mechanism: it is
exactly `11-revision-and-update.md` REQ-REV-006, unchanged. Where a
`MethodGraphNode`'s bound arguments may have gone stale due to a
`TransformGraphNode` chain feeding them, its invocation adapter MUST first
trigger `update()` over those bound IN/INOUT arguments before invoking the
underlying method (see `11-revision-and-update.md` REQ-REV-011, added by
this section). This is a separate, general staleness fix, not specific to
characteristics, but it is what makes the *content* half of REQ-CHAR-015
actually fire at a sensible point for ordinary (non-mid-Run) staleness.

## 18.9 Considered and rejected: a dedicated "ChangeGeom" phase

**Status: rejected**, recorded here so it is not re-proposed without
re-litigating the reasons.

An earlier iteration of this design considered requiring geometry mutation
to happen only inside a dedicated, framework-recognized phase (distinct
from ordinary `Initialize`/`Run`), which would skip its own pre-invocation
pull (avoiding wasted recompute of about-to-be-invalidated content) and
rely on the *existing* phase-boundary machinery (REQ-CHAR-018) to
reallocate dependents lazily-but-correctly at the next ordinary phase
invocation.

This was rejected for two concrete implementability reasons:

1. **No existing mechanism to mark a phase as special.** `SetServices`
   phase registration (`12-methods-and-drivers.md` REQ-MTH-010/011) has no
   hook for "this phase behaves differently in the demand-driven
   machinery" without inventing new registration plumbing.
2. **No way to constrain user code inside the phase.** Once inside any
   registered phase, a component can call anything, including a child's
   methods directly (Fortran has no access control here). The phase
   approach requires user code to *not* do certain things (e.g. reach into
   a child while the framework assumes a skip-pull, about-to-mutate state)
   that the framework has no way to actually prevent.

The eager-mutator design (§18.8) was adopted instead specifically because
it does not depend on the framework recognizing any calling context as
special, and because restricting the propagation walk to pure structural
resets (REQ-CHAR-017) means there is no unsafe window for user code to
exploit, regardless of where the mutator is called from.

**Cost of this choice, noted for completeness:** the ChangeGeom-phase
design would have given the framework an enforceable invariant ("geometry
is only mutated here," checkable at registration time). The mutator-call
design has no equivalent cheap check — if geometry is mutated through some
channel other than the sanctioned API, that is a discipline/documentation
requirement, not something the framework can detect and reject.
