# Dependency network

Source: `superstructure/generic/graph/DependencyNetwork.F90` (597 lines —
the biggest graph-neutral module after `ComponentGraph`/`GraphStateItem`).

## Shape

```fortran
type :: DependencyNetwork
   private
   type(NodeId_NodeIdSet_Map) :: predecessors_map
   type(NodeId_NodeIdSet_Map) :: successors_map
   logical :: frozen = .false.
end type
```

(`DependencyNetwork.F90:31`.) No first-class edge object — a dependency
is addressed only by its `(source, target)` `NodeId` pair, stored
twice (once per direction) in two synchronized gFTL maps. No ownership
or node-kind knowledge lives here; this type is pure adjacency.

## Key operations

- `add_dependency(source, target, rc, valid_ids)` (`:79`) — validates
  before mutating either map: not frozen, both ids valid, no
  self-dependency, (if `valid_ids` passed) both ids known to the
  caller's allowed set, and **would not create a cycle**
  (`would_create_cycle`, DFS check run *before* insertion). Any
  rejection leaves both maps untouched. `valid_ids` is optional — pass
  it to get ownership/foreign-id checking; omit it for the standalone
  synthetic tests that exercise pure adjacency with no `ComponentGraph`
  present.
- `remove_dependency` (`:111`) — no-op success if the pair is absent.
- `freeze()` (`:142`) — irreversible, no failure path.
- `validate(rc, valid_ids, producer_ids, state_item_ids)` (`:250`) —
  the real integrity check: every referenced id must be in `valid_ids`
  (if passed); every `state_item_ids` member may have **at most one
  predecessor within this network** (producer uniqueness, REQ-DEP-008).
  This is a *per-network* check — the same `StateItemNode` can
  legitimately have different producers in different networks (e.g. a
  callback's `get` vs. `put` network). The stronger cross-network
  "never written twice in one update pass" rule is enforced at
  `GraphBuilder`/`ComponentGraph` construction time, not here (see
  `open-questions.md`, formerly Q4).
- `get_referenced_ids()` (`:166`) — public wrapper used by
  `ComponentGraph%validate()` to build its own per-call allowance set
  without this type needing any hierarchy-wide knowledge
  (keeps `DependencyNetwork` graph-neutral per REQ-CG-002 layering).

## Cycle detection

`would_create_cycle`/`has_cycle_from` use an **explicit-stack DFS**
(`DfsFrame` type, `:67`), not a recursive Fortran function — deliberate,
to avoid call-stack depth concerns at scale. Checked proactively on
every `add_dependency`, so a frozen, validated network is cycle-free by
construction, not just by a one-time check.

## Iteration convention

Every gFTL walk in this file (and everywhere else in the graph layer)
uses:

```fortran
iter = c%ftn_begin()
do
   call iter%next()      ! advance BEFORE first use
   if (iter == c%ftn_end()) exit
   ...
end do
```

`iter` starts one position *before* the first element.

## Ownership vs. storage

One `DependencyNetwork` instance per logical wiring purpose per
component — e.g. the default per-component network, plus a `get`/`put`
pair per callback interface attachment. Instances are owned and keyed
by `DependencyNetworkId` inside the shared `HierarchyGraph`
(`hierarchy-and-component-graph.md`), not inside this module.
