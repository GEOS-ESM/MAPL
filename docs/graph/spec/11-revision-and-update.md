# 11. NodeRevision, Demand-Driven Update, and Runtime vs. Compiled Execution

Status: `[SETTLED]` NodeRevision type and update algorithm, including the
`MethodGraphNode` invocation trigger (§11.4a); `[SPECULATIVE]` eventual
storage relocation and compiled execution; `[OPEN]` composite revision
semantics (§11.4).

## 11.1 NodeRevision

**REQ-REV-001.** `NodeRevision` MUST be a small, encapsulated type with:

- an internal 64-bit integer representation, kept private (no client reads
  the raw integer)
- an initial **invalid** state, distinct from any valid revision value
- `advance()`: transitions invalid → first valid revision; increments a
  valid revision to the next valid revision
- equality and inequality operators
- overflow detection performed *before* wrapping (i.e., `advance()` on the
  maximum valid value MUST fail/report rather than silently wrap to an
  earlier-looking value)

**REQ-REV-002.** Only operations that establish or modify a logical value
MUST advance its revision. Read-only access, aliasing, or graph traversal
must not.

**REQ-REV-003.** `StateItemNode` MUST expose revision operations
(`get_revision()`, `advance_revision()`, comparison helpers) as methods,
not as direct field access, so that the underlying storage location can
change without breaking callers (see §11.2).

## 11.2 Eventual storage relocation `[SPECULATIVE]`

Initially, `StateItemNode` MAY contain `NodeRevision` directly as a field.
Eventually, the authoritative revision is expected to move into the
underlying ESMF object's private MAPL Info namespace (REQ-ESMF-004),
because `ESMF_NamedAlias` shares Info (REQ-ESMF-003) — so aliases would
automatically share revision without any extra graph-level bookkeeping.

**REQ-REV-004.** This relocation MUST be transparent to callers of the
REQ-REV-003 accessor methods; no client code should need to change when it
happens.

**Blocked, pending ESMF.** `ESMF_RouteHandle` does not currently support an
`Info` object, so this relocation cannot apply to a `RouteHandle`-kind
`GraphStateItem` as-is. This is a real constraint, not a documentation gap — the
MAPL author is pursuing this with the ESMF development team to see whether
it can be added. Until/unless it is, `RouteHandle` revision storage MUST
remain as a plain field (or on the wrapper `ESMF_State` introduced by
`07-component-graph.md` REQ-CG-010, whose own Info namespace *is*
available) rather than assuming eventual relocation into the RouteHandle's
own Info.

## 11.3 Demand-driven update algorithm

**REQ-REV-005.** Transform execution MUST be lazy. A `TransformGraphNode`
executes only when one of its declared outputs is requested *and* an input
has changed since the transform last ran.

**REQ-REV-006.** The update process, per requested output, MUST be:

1. Recursively update all required predecessor values (depth-first,
   respecting `DependencyNetwork` adjacency).
2. Obtain the current revisions of the transform's declared inputs.
3. Compare them against the revisions recorded after the transform's
   previous successful execution.
4. Execute the transform if any required input revision differs from the
   recorded value, or if the transform has never executed.
5. On successful execution, advance the revisions of **all** declared
   outputs.
6. Save the current input revisions as the new "last-run" baseline.

**REQ-REV-007.** A successful transform execution MUST update all declared
outputs (all-or-nothing contract). Partial-output execution is explicitly
**deferred**: it MAY be added later if a concrete use case requires it, but
MUST NOT be assumed or half-implemented in v1.0.

## 11.4 Composite values (State, FieldBundle) `[OPEN]`

**REQ-REV-008.** For advertised composite items, revision tracking follows
*advertised granularity*. Example: if a `TimeInterpolationBracket` is
advertised as a single `FieldBundle`, the owning component advances the
**bundle's** revision after producing a new bracket — member-level
revisions inside it are not separately significant to consumers who only
see the bundle-level advertisement.

**Nuance (recorded, believed already covered by the granularity rule
above, but worth stating explicitly so it isn't "helpfully" broken later):**
a bracket represented as a `FieldBundle` may be regridded as a whole by a
single `RegridTransform`. A naive implementation might instead apply that
Transform recursively to each member Field individually. Doing so is fine
functionally, but such a per-member-recursing implementation MUST NOT
consult the *member* Fields' own revisions to decide whether to run —
per REQ-REV-008, only the bundle's (container-level) revision is
authoritative for this purpose. Checking member revisions here would be
exactly the kind of double bookkeeping REQ-REV-008/Q8 already warns against.

Beyond this granularity rule, the general question of how container
(`StateValue`/`FieldBundleValue`) revisions relate to member revisions
(does adding/changing a member always bump the container? can a member
change without the container revision changing, if not part of the
advertised contract?) is **not fully specified**. See
`17-open-questions.md` Q8.

## 11.4a Update trigger for MethodGraphNode-bound arguments

**Observation (motivating question):** REQ-REV-005/006 describe the update
algorithm as running "when one of its declared outputs is requested," but
nowhere specified who issues that request or when. For values that are
themselves `TransformGraphNode` outputs consumed by further Transforms, a
request eventually arrives via some downstream chain. But an ordinary
`GraphStateItem` that a component's own `MethodGraphNode` (its `Run`/
`Initialize` phase) reads or writes directly is never a
`TransformGraphNode` output-request target — and per REQ-NODE-007,
`MethodGraphNode` invocation is explicitly *not* scheduled by demand-driven
logic. Without an explicit trigger, such a `GraphStateItem` could be silently
stale (content) or wrongly-shaped (structure) when the method that
consumes it actually runs.

**REQ-REV-011.** The concrete trigger points, by network, are:

- **Default `DependencyNetwork`** (`07-component-graph.md` REQ-CG-001a —
  ordinary GridComp `Run`/`Initialize` phases): the *first step* of
  invoking a GridComp run method MUST be to request updated values
  (`update()`, REQ-REV-006) for **all** of that method's import items.
  After the run method completes, **all** of its export items' revisions
  MUST be advanced (REQ-MTH-003a, `12-methods-and-drivers.md`) —
  unconditionally, on the coarse-grained default-network assumption that
  every phase touches every import/export.
- **Other (callback/method-specific) networks** (`15-callbacks.md` §15.10):
  the trigger points are immediately *before* and immediately *after*
  execution of the relevant state callback method, not tied to a whole
  GridComp phase boundary.

**REQ-REV-011a.** This trigger/advance logic is controlled by the
`OuterMetaComponent` layer (the private-state derived type inside an
`OuterComponent`'s wrapping GridComp that actually invokes methods on the
wrapped user component) — not by the `MethodGraphNode`/invocation-adapter
abstraction itself acting alone. The invocation adapter (§12.2) still
performs the call; `OuterMetaComponent` is what decides, on the
default-network path, to pull-all-before and advance-all-after.

**`[future direction, not yet specified]`** Later, individual run methods
MAY be annotated to declare a narrower set of imports actually pulled and
exports actually advanced, rather than the current default-network
all-imports/all-exports assumption — see `07-component-graph.md` REQ-CG-001a
for the matching future-direction note on per-method networks. Default
behavior, absent such annotation, MUST always remain "assume all
imports/exports are involved."

**Note:** REQ-REV-011 covers *content* staleness (Transform-produced
values). It does NOT by itself solve structural staleness (e.g. a Field
needing reallocation because a shared geometry changed) for the
same-call-immediate-reuse case — see
`18-state-item-characteristics.md` §18.8 for why that needs an eager,
non-phase-boundary mechanism instead, and REQ-CHAR-018 for how the two are
kept separate but complementary.

## 11.5 Runtime graph vs. compiled ESMF execution

**REQ-REV-009 (near-term, SETTLED).** The first implementation MUST execute
update traversal directly through `ComponentGraph`/`DependencyNetwork`
method calls (interpret the graph at runtime). This is the semantic
reference implementation and MUST remain available (even after compilation
exists) for:

- construction-time validation
- resource reuse search
- diagnostics
- teardown
- reference-mode testing (comparing compiled-path results against
  interpreted-path results)

**REQ-REV-010 (long-term, SPECULATIVE).** A frozen graph MAY eventually be
"compiled" into direct MAPL/ESMF wrapper relationships (bypassing runtime
map lookups) for performance. Motivating numbers: expected whole-hierarchy
graph size ~10,000 nodes; typical local fan-out only 0–3; existing MAPL
wrappers already propagate signals directly without map lookups. This
compilation step, if built, MUST preserve the same observable update
semantics as REQ-REV-006. See `17-open-questions.md` Q9.
