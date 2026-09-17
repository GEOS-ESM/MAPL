# 7. ComponentGraph

Status: `[SETTLED]` ownership and lifecycle, including the re-evaluated
structure-constructor position (§7.2) and the shallow-copy-identity
requirement (§7.4, REQ-CG-009/010); `[OPEN]` future fine-grained
per-method networks (§7.1, no design yet), convergence mechanics for
REQ-MTH-011c.

## 7.1 Ownership

**REQ-CG-001.** `ComponentGraph` MUST own:

- `NodeId → GraphNode` map (polymorphic)
- `DependencyNetworkId → DependencyNetwork` map
- `NodeIdGenerator`
- `DependencyNetworkIdGenerator`
- `PortIdGenerator`
- a default `DependencyNetworkId`
- public import ports
- public export ports
- child-port bindings
- semantic resource indexes (e.g., `RouteHandleKey → NodeId`; see
  `14-route-handles.md`)
- initialized/frozen lifecycle state

**REQ-CG-001a.** Initially, every `MethodGraphNode` representing a GridComp
initialize/run phase MUST use the single default `DependencyNetwork`. In
particular, all init phases (`12-methods-and-drivers.md` REQ-MTH-011)
always share the default network, regardless of any future refinement
below.

**`[future direction, not yet specified]`** Component registration is
expected to eventually be able to declare, per run-method phase, which
imports it actually consumes and which exports it actually updates
(increments the revision of), so that phase can use a narrower,
purpose-specific `DependencyNetwork` instead of the default one — this is
the same fine-grained-annotation direction noted in
`11-revision-and-update.md` §11.4a and `12-methods-and-drivers.md`
REQ-MTH-003a. Default behavior (no annotation) MUST always remain "this
phase touches all imports/exports," using the default network. This is not
a design yet, just a stated future direction the default-network naming
(below) is chosen to remain compatible with.

**Naming.** "Default" is kept as the name for this network, on the
reasoning that once per-method networks exist, "default" becomes the
permanent fallback network for any method that has not opted into
fine-grained declaration — a real, permanent role, not a placeholder.
Alternatives considered: `CoarseNetwork`, `UnscopedNetwork` (emphasizing
contrast with future fine-grained networks). No change made; revisit if a
better name surfaces.

**REQ-CG-002.** `ComponentGraph` MUST NOT depend on `OuterComponent`,
`StateRegistry`, or any component-hierarchy implementation type. Those
concerns belong to `GraphBuilder` (`08-graph-builder.md`). This is a hard
layering rule, not a style preference — it is what keeps `ComponentGraph`
unit-testable in isolation and reusable outside the full OuterComponent
machinery.

**Withdrawn restriction:** an earlier draft of this requirement also
excluded "connection-point parsing" from `ComponentGraph`. That exclusion
is withdrawn (not confirmed as necessary) — see `08-graph-builder.md`
REQ-GB-001.

## 7.2 No structure constructor

**REQ-CG-003 `[re-evaluated]`.** The original position — `ComponentGraph`
MUST NOT rely on a structure constructor "because initialization can
fail" — is reconsidered given REQ-CG-002 (as amended) already excludes
`OuterComponent`/`StateRegistry` from this layer. Checked against what
`initialize()` actually does (create ID generators, create the default
`DependencyNetwork`, REQ-CG-004): neither step has a realistic failure
mode — generators start from zero-initialized counters, and the very
first `NodeId`/`DependencyNetworkId` issued from a fresh generator cannot
hit the exhaustion case (REQ-ID-002), since exhaustion is only reachable
after substantial prior allocation. The bootstrap is therefore
unconditionally successful.

**REQ-CG-003a.** `ComponentGraph` MAY be produced by a structure
constructor (or an equivalent factory function) that performs the
bootstrap in REQ-CG-004 directly, rather than requiring a separate
fallible `initialize()` call for this step.

**REQ-CG-003b.** `finalize()` remains a required, genuinely fallible
operation (destroying owned ESMF resources can return errors) and MUST NOT
be folded into a structure destructor.

**REQ-CG-003c.** A private `initialized`-equivalent flag, if retained, MUST
guard against use *after* `finalize()`, not against use before a
now-unnecessary `initialize()` call.

**REQ-CG-004.** `initialize()` MUST create a default `DependencyNetwork`.
Its `DependencyNetworkId` MUST be an instance property (read at runtime),
not a global/module constant — different `ComponentGraph` instances have
different default network IDs.

## 7.3 Lifecycle: mutable → frozen

**REQ-CG-005.** A `ComponentGraph` is mutable during initialization and
wiring (nodes and dependencies may be added, ports created, etc.).

**REQ-CG-006.** After all initialization phases and connection processing
complete, the graph MUST be frozen. After `freeze()`:

- No nodes may be added
- No dependencies may be added
- No state *structure* may change (no new ports, no new members in
  `StateValue`/`FieldBundleValue` maps, no new `DependencyNetwork`s)
- Runtime values and revisions MAY still change (payload data updates,
  `NodeRevision.advance()`)

**REQ-CG-007.** Freeze is a `ComponentGraph`-level operation. It SHOULD
freeze the graph's own `DependencyNetwork`s (`DependencyNetwork.freeze()`)
as part of the same call, so that "the graph is frozen" implies "all of its
networks are frozen" — these must not be allowed to drift out of sync.

## 7.4 Time-dependent resources vs. frozen structure

**Caution (identified tension, addressed below):** some sections of this
design (`14-route-handles.md` §14.4, `13-geometry-and-vertical-grids.md`
§13.4) describe RouteHandles and geometries being "recreated" or "renewed"
in response to time-dependent geometry, *after* the graph would normally be
frozen. REQ-CG-006 forbids adding nodes after freeze. Therefore:

**REQ-CG-008.** Any post-freeze "renewal" of a resource (RouteHandle,
time-dependent geometry) MUST be implemented as an in-place value/revision
update on an *existing* `StateItemNode` (same `NodeId`), never as creation
of a new node or new dependency. If a genuinely new node would be required
(e.g., a materially different RouteHandle that cannot reuse the existing
node's identity), the design does not yet support this post-freeze, and
that gap is `[OPEN]` — see `17-open-questions.md` (related to Q9, and a
consideration to add explicitly if a concrete use case appears).

### 7.4.1 Shallow-copy identity hazard under destroy/recreate

**REQ-CG-009.** REQ-CG-008's "in-place update" MUST NOT be read as
permission to destroy-and-recreate the underlying ESMF object wholesale —
doing so invalidates any shallow copy (`ESMF_NamedAlias`, or a plain
assignment of the handle) that any other code currently holds
(REQ-ESMF-002). "In-place" means the *handle* held by the `StateItemNode`
remains valid and stable; only its *contents* change.

**Geom case.** For a `GraphStateItem` carrying a geometry proxy `esmf_field`
(`04-graph-value-hierarchy.md` §4.6.4), the required operation is
`ESMF_FieldEmptyReset` on the *Field* — the Field handle itself is never
destroyed and recreated, only reset. Any alias of that Field remains valid
across the reset for this reason. The contained `Grid`/`Mesh`/`LocStream`/
`XGrid` underneath MAY be destroyed and a new one created and associated,
as long as nothing outside this operation is holding a shallow copy of the
*Grid* object directly (as opposed to holding it indirectly via the Field).
**`[OPEN, needs ESMF-level verification]`** whether `ESMF_FieldEmptyReset`
is safe to call across a destroy-then-recreate of the underlying Grid (as
opposed to simply associating a different, already-existing Grid) has not
been confirmed against ESMF and should be checked before implementation.

**RouteHandle case.** `ESMF_RouteHandle` has no `ESMF_FieldEmptyReset`
equivalent — a genuinely new `RouteHandle` requires destroying the old one
and creating a new one, which invalidates any shallow copy of the raw
handle (§11.2 notes `RouteHandle` also has no `Info` object, compounding
this — see `11-revision-and-update.md` §11.2).

**REQ-CG-010.** A `GraphStateItem`'s route-handle kind (`esmf_kind() ==
ESMF_STATEITEM_ROUTEHANDLE`, `04-graph-value-hierarchy.md` REQ-SI-002b)
MUST be realized through a persistent wrapper — the `esmf_state`
component holding a nested `ESMF_State` that contains the
`ESMF_RouteHandle` as a member — rather than as a bare handle held
directly. The wrapper `ESMF_State` object itself persists across renewal;
only its contained `RouteHandle` member is destroyed and replaced. Any
"shallow copy" of a shared route-handle-kind `GraphStateItem` MUST be a
shallow copy of the wrapper `ESMF_State` (an `ESMF_NamedAlias` of it), and
MUST re-resolve the current `RouteHandle` via a fresh `ESMF_StateGet` at
each use rather than caching the raw handle across calls. This mirrors
the Field case one level up: the thing that must stay stable (the
wrapper State) is distinct from the thing that gets replaced (the
RouteHandle inside it).
