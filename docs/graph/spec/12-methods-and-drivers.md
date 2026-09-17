# 12. MethodGraphNode, GriddedComponentDriver, and Initialization Lifecycle

Status: `[SETTLED]` GriddedComponentDriver and lifecycle ordering;
`[OPEN]` exact invocation-adapter design (§12.2, Q2).

## 12.1 MethodGraphNode covers two call shapes

**REQ-MTH-001.** `MethodGraphNode` MUST represent both of the following
through one node type, using an invocation-adapter abstraction to hide the
ESMF call-signature difference:

- A GridComp initialize/run phase, invoked as
  `(importState, exportState, clock, component/private-state context)`
- An attached ESMF State callback method, invoked as `(callback State)`

**REQ-MTH-002.** At the graph level, both cases are "a method with named
argument bindings and `AccessSpec`s" (`15-callbacks.md` §15.4). The graph
model MUST NOT need to know or care which ESMF call signature underlies a
given `MethodGraphNode` — that is entirely inside the invocation adapter.

## 12.2 Invocation adapters `[OPEN — design shape, not the concept]`

Candidate adapters:

- `GridCompMethodInvocation`
- `StateMethodInvocation`

**REQ-MTH-003.** Whatever the final adapter shape, it MUST NOT duplicate
invocation logic that already exists in `GriddedComponentDriver` — the
adapter's job is to translate graph-level "invoke this method with these
bound arguments" into the appropriate driver/ESMF call, not to reimplement
ESMF calling conventions. See `17-open-questions.md` Q2 for the specific
open question of how to guarantee no duplication.

**REQ-MTH-003a.** The trigger/advance discipline in
`11-revision-and-update.md` REQ-REV-011 applies to `MethodGraphNode`
invocation as follows:

- Before invocation: `update()` (REQ-REV-006) MUST be triggered over the
  method's bound `IN`/`INOUT` arguments — this is the point at which
  ordinary (non-structural) staleness on method-bound arguments is
  resolved.
- After invocation: the revisions of the method's bound `OUT`/`INOUT`
  arguments MUST all be advanced — the method is assumed, by default, to
  have written all of them (this is the export-side counterpart of the
  pull-all-before rule, and shares its default-network, all-or-nothing
  assumption per REQ-REV-011).

Both are a required part of "invoke," not an optional extra step. On the
default `DependencyNetwork`, per REQ-REV-011, this discipline is actually
owned and executed by the `OuterMetaComponent` layer (the private-state
derived type inside an `OuterComponent`'s wrapping GridComp that invokes
methods on the wrapped user component) around the call the invocation
adapter performs, not by the adapter in isolation.

**`[future direction, not yet specified]`** A future annotation mechanism
MAY let a method declare that only a subset of its bound `OUT`/`INOUT`
arguments were actually updated, avoiding the blanket advance-all default.
Not designed yet — see `07-component-graph.md` REQ-CG-001a and
`11-revision-and-update.md` REQ-REV-011 for the matching notes on the
import/pull side of this same future direction.

## 12.3 GridComp import/export as a conceptual unified state

**REQ-MTH-004.** The import and export states of a GridComp MAY be treated
conceptually as nested parts of one larger method-argument state for
`MethodGraphNode` argument-binding purposes. This is a graph-level
convenience view; it MUST NOT require materializing an additional real
ESMF State that merges import and export.

## 12.4 No separate component node

**REQ-MTH-005.** A `GraphNode` for "the component as a whole" is not
required and MUST NOT be introduced. The component is represented
indirectly through its `MethodGraphNode`s (phases) and `StateItemNode`s
(state items). (Restated from `03-graph-node-hierarchy.md` REQ-NODE-008 for
locality.)

## 12.5 Clock

**REQ-MTH-006.** Clock is invocation *context* for a `MethodGraphNode`, not
a graph dependency, unless/until a concrete need arises to model it as one
(e.g., a Transform whose triggering genuinely depends on clock value beyond
what revision comparison already captures). Do not add a Clock
`GraphValue`/dependency preemptively.

## 12.6 GriddedComponentDriver

**REQ-MTH-007.** `GriddedComponentDriver` wraps: ESMF GridComp, Import
State, Export State, Internal State, Clock. It exposes high-level calls
that unpack into the explicit ESMF calls underneath.

**REQ-MTH-008.** Each `OuterComponent` MUST have:

- one driver for its own user component
- one driver per child `OuterComponent`

i.e., a parent holds a driver *for* each child; a child, internally, holds
a driver for its own wrapped user component. Drivers do not nest across the
parent/child `OuterComponent` boundary beyond this.

**REQ-MTH-009.** `MethodGraphNode` MUST reference a driver through a
stable local identifier (e.g., an index/key resolvable within the owning
`ComponentGraph`/`GraphBuilder` context), never through a raw pointer to
the driver object and never by copying the driver. Drivers are owned
elsewhere (by `OuterComponent`); the graph only needs a stable way to look
one up at invocation time.

## 12.7 SetServices and initialization lifecycle

**REQ-MTH-010.** `SetServices` is outside the graph. It bootstraps a
component by registering initialization and run phases with MAPL/ESMF.
`SetServices` itself is never represented as a graph node.

**REQ-MTH-011.** The initialization lifecycle MUST proceed in this order:

1. `SetServices` registers methods (phases) with the framework.
2. Registered initialize methods MAY subsequently be represented as
   `MethodGraphNode`s.
3. The following concrete initialization phases execute in order (this
   refines and replaces the earlier generic "Advertise, then later
   phases complete specs/payloads" description with the actual MAPL
   phase list):

   a. **`advertise`** — creates state items, their backing
      `StateItemNode`s/payloads, and ports. **Advertising an item does not
      make its revision valid** (`11-revision-and-update.md` §11.1) — a
      revision becomes valid only once some phase establishes an actual
      usable value (`NodeRevision.advance()` called).
   b. **`modify_advertised`** — currently a no-op; reserved for future use
      (e.g. altering an already-advertised item's declared characteristics
      before realization). Do not assume behavior here beyond "runs, does
      nothing" until a concrete need is specified.
   c. **Cycle the following three phases until convergence** (an iteration
      that makes no further progress — no item newly reaches `SPECIFIED`
      status on any characteristic, REQ-CHAR-003 — terminates the cycle):
      1. **`realize_provided`** — allocate any provided items/extensions
         that are ready to be materialized.
      2. **`accept_transfer`** — implement user connections for export
         items whose characteristics are now fully resolved (every
         relevant `CharacteristicStatus` is `SPECIFIED`,
         `18-state-item-characteristics.md` §18.3).
      3. **`realize_accepted`** — allocate any items that were waiting on
         characteristic resolution (possibly none, if nothing newly
         qualified this iteration).
   d. **`read_restart`**
   e. **`user_specific`**

   **`[OPEN]`** The convergence algorithm for step (c) (how progress is
   detected, what happens if an iteration limit is reached without
   convergence, whether non-convergence is a hard error or a partial
   result) is not specified here and needs a concrete design before
   implementation.

4. Parent wiring adds transforms, extensions, proxies, and callback
   connections (`GraphBuilder`, `09-extension-reuse.md`, `15-callbacks.md`).
5. Graph validation occurs (`DependencyNetwork.validate()` across all
   networks).
6. The graph is frozen (`ComponentGraph.freeze()`).

**REQ-MTH-012.** After freezing, runtime methods MAY update values
(payload data, revisions) but MUST NOT change topology (no new nodes,
dependencies, ports, or state-structure changes — restated from
`07-component-graph.md` REQ-CG-006).

**REQ-MTH-013.** Any initialization method that changes graph structure
MUST do so through `GraphBuilder`, never by calling `ComponentGraph`
mutation methods directly from arbitrary component code. `GraphBuilder` is
the sole structural-mutation entry point above the graph-neutral core.
