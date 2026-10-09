## Context

`docs/graph/spec/16-inout-items.md` settles only REQ-INOUT-001 (direct
alias, no transform either direction) and explicitly leaves four points
open (revision authority under two producers, lazy direction selection,
recursion, general authority rules) that gate any *non-trivial*
implementation (REQ-INOUT-002). This change's own roadmap entry (§20.4.4,
5b) scopes it to REQ-INOUT-001 only and states it adds no new
`TransformGraphNode`. Per the same discipline Phase 4's 4b and Phase 5's
5a followed for their own `[OPEN]` points, the decisions below resolve,
up front, exactly enough of "how" to implement REQ-INOUT-001 without
reopening any of REQ-INOUT-002's four deferred points.

Relevant existing machinery this change reuses rather than duplicates:

- `GraphBuilder.F90`'s `resolve_one` (`superstructure/generic/
  GraphBuilder.F90:689-782`) already implements the REQ-EXT-003 no-op
  check this change needs for the forward direction:
  `build_characteristics` + `find_mismatched_characteristics`, wiring a
  plain `add_dependency` edge when `size(mismatched) == 0` and delegating
  to `find_or_build_extension_chain` otherwise.
- `graph/graph-builder`'s existing callback-interface branch ("An import
  declaring an expected callback interface is resolved through the
  callback-wiring capability, not ordinary exact-name matching") is the
  direct structural precedent for this change's own new branch: a marker
  on the destination item's declaration causes resolution to be handed
  off to a different capability instead of the default exact-match path.
- `graph/callback-wiring`'s "get"/"put" per-method network pattern
  ("Each callback method may have its own get and put dependency
  networks") is the direct structural precedent for this change's
  forward/return networks — independently constructed, independently
  acyclic, same underlying items allowed to participate in both.
- `06-dependency-network.md` REQ-DEP-008's own note already anticipates
  this exact case ("a `StateItemNode` MAY have different producers in
  different networks it participates in (e.g., forward vs. return
  network for an inout item...)"), and REQ-DEP-008a's own carve-out for
  the callback get/put pattern ("the same `StateItemNode` is written to
  more than once ... within a single update pass" is permitted when the
  two writes are guaranteed never to be part of the same pass) is the
  direct precedent this change relies on to justify the same allowance
  for inout's forward/return pair.
- Legacy has no inout mechanism to match (unlike Phase 3's exact-parity
  requirement) and ESMF has no `INOUT` `StateIntent` value — this is new
  capability, not a reproduction of existing behavior, so declaration
  syntax is this change's own decision, not a legacy-compatibility
  constraint.

## Goals / Non-Goals

**Goals:**
- Let a component declare one of its items as an ordinary inout borrower
  of exactly one owner item.
- When borrower and owner payloads match exactly, wire both directions
  (forward and return) with no new field storage and no
  `TransformGraphNode`, reusing the existing REQ-EXT-003 no-op check.
- Explicitly reject every pairing shape this change does not resolve
  (mismatched payload, chained/recursive borrowing, borrower with no
  identifiable owner) rather than silently mis-wiring or falling back to
  extension-chain construction.

**Non-Goals (reserved for REQ-INOUT-002 / 5b2, not attempted here):**
- Non-direct-alias (mismatched) inout support of any kind.
- Chained/recursive borrowing (a borrower that is itself an owner).
- General revision-authority rules beyond the one specific
  eager-push-after-execution sequencing this change defines for the
  single-owner/single-borrower direct-alias case (Decision 3 below) —
  this change does not attempt to generalize that sequencing to cases
  with multiple borrowers of one owner, lazy direction selection, or any
  other shape REQ-INOUT-002 reserves.
- Any YAML/`ComponentSpecParser` surface syntax — see Decision 1.

## Decisions

### Decision 1: Borrower-side-only declaration, reusing the existing ordinary-connection mechanism for owner identification
Add one new field to `VariableSpec` (e.g. `is_inout_borrower`, following
the existing "mark the item, not a new itemType" precedent already used
for `state_item_variant`, 4f) on the **borrower's** declaration only. The
owner's item is declared and advertised exactly as an ordinary item
(typically an export) — no new field, no new role, on the owner side.
Which owner an inout borrower pairs with continues to be expressed
through the existing `MatchConnection` declaration mechanism already
used for ordinary import/export connections; this change does not
introduce a new connection-declaration syntax.

**Alternative considered and rejected:** a dedicated "inout connection"
declaration requiring both sides to opt in symmetrically. Rejected
because the owner/borrower roles are inherently asymmetric (REQ-INOUT-001
never asks the owner to declare anything special), and introducing a
second connection-declaration mechanism alongside the existing
`MatchConnection` would duplicate machinery this change does not need to
duplicate.

**Scope note:** this change adds the `VariableSpec` field and the
`GraphBuilder` resolution branch; it does NOT add YAML grammar for
declaring inout intent (see Non-Goals). The declaration surface for this
change is programmatic (`VariableSpec` construction), matching how
several prior graph-native sub-changes (e.g. `callback_interface_id`)
shipped their marker field before any YAML exposure was added.

### Decision 2 (revised during implementation): One return network per inout pairing, created via the existing `create_network()` API
Originally this document specified one shared "return" network per
component, created lazily the first time an inout pairing is resolved.
Implementation surfaced a real architectural mismatch: `ComponentGraph`'s
only persistent key-value store (`resource_index`) is typed to store
`NodeId` values, not `DependencyNetworkId` values, so there is no
existing mechanism to cache a lazily-created network id across separate
`resolve_match_connection` calls. The alternative - adding a second
distinguished network field directly to `ComponentGraph` (mirroring
`default_network_id`) - was rejected: that would bake an
ordinary-inout-specific concept into the graph-neutral Phase 1-2 core
type, against REQ-CG-002's spirit (`ComponentGraph` must not encode
higher-layer, capability-specific concepts).

**Revised decision:** each inout pairing gets its own fresh return
`DependencyNetwork`, created via `this_graph%create_network()` at the
point that pairing is resolved - exactly mirroring callback wiring's own
established precedent (`build_callback_method_binding` already calls
`graph%create_network()` twice, once per method binding, not once per
component). This requires no new `ComponentGraph` API and no new
resource-index usage.

**Why this does not weaken anything the spec requires:**
`graph/ordinary-inout`'s own spec text only requires the forward and
return edges for *one pairing* to be "each in its own dependency
network" - it does not require every pairing's return edge to share one
network. Every requirement this design needs from a "return network"
(acyclic on its own, distinct from the forward/default network,
REQ-DEP-008a satisfied) holds per-pairing exactly as well as it would
shared-across-pairings: REQ-DEP-008a's check iterates every network a
`ComponentGraph` owns regardless of count, and a single-edge network is
trivially acyclic.

**Alternative considered and rejected (original, see above):** one
shared return network per component. No longer pursued - superseded by
the revised decision for the architectural reason stated above, not a
new requirement.

### Decision 3 (revised during implementation): Return-edge is graph structure now; the runtime propagation trigger is explicitly deferred, not built here
Originally this document specified an eager, synchronous push of the
return edge immediately after "whatever `TransformGraphNode`/
`MethodGraphNode` produces the borrower's node completes." Implementation
surfaced that this premise does not hold against the actual codebase:

- Reading `ComponentGraph_DemandDrivenUpdate.F90`'s `finish_update_frame`
  directly: `update()`'s dispatch only does active work (execute,
  advance revision) for a `TransformGraphNode` frame; a `StateItemNode`
  frame - which is exactly what both the ordinary REQ-EXT-003 no-op edge
  and this change's forward/return edges are, by REQ-INOUT-001's own "no
  transform in either direction" requirement - is unconditionally a
  no-op in `finish_update_frame`'s `select type`. There is no
  `TransformGraphNode` anywhere in a direct-alias inout pairing to hang
  a "produces the borrower's node" hook on.
- The real event this change needs to trigger on is the borrower's own
  GridComp run completing (a `MethodGraphNode` invocation at the
  component-hierarchy/invocation-lifecycle layer, Phase 4) - and per
  this project's own roadmap (`20-implementation-roadmap.md` §20.4.2/
  §20.4.3), that real invocation-completion wiring is not yet hooked into
  the real init/run sequence even for the callback-wiring sub-change
  that landed before this one. Building that generic hook now would be
  Phase 4 invocation-lifecycle surgery, not a narrow Phase 5b addition.

**Revised decision:** this change lands the return edge as real,
queryable, validated graph *structure* only (REQ-INOUT-001's forward and
return `DependencyNetwork` edges, correctly distinct networks, correctly
satisfying REQ-DEP-008a) - exactly the same "structure now, execution
later" split this project has already used more than once (4e/4g landed
graph structure for geometry/route-handles while explicitly deferring
real regrid execution: "no RegridTransform or GraphBuilder.F90 wiring was
added; real regrid execution remains future work"). The *runtime*
eager-push trigger described in the original Decision 3 is explicitly
deferred to a follow-up change, to be filed once Phase 4's real
`MethodGraphNode`/`GriddedComponentDriver` invocation-completion hook
exists for ordinary (non-callback) component runs generally - this
deferral is a precondition this change surfaces, not something it
attempts to work around with a narrower, ad hoc hook of its own.

**Why this is still a complete, useful implementation of REQ-INOUT-001
as scoped:** REQ-INOUT-001 itself only commits to "no transform in either
direction" and settles the direct-alias case's *wiring* shape; it makes
no claim about when/how revision propagation runs at update() time
(that mechanics question was always `16-inout-items.md`'s own
`[OPEN]` territory, not something REQ-INOUT-001 resolves). Landing the
structure now makes the declaration, resolution, and rejection behavior
(every scenario `graph/ordinary-inout`'s spec actually states) real and
testable; the deferred runtime trigger is additive follow-up work, not a
narrowing of any committed scenario.

**Alternative considered and rejected:** build a new, narrow
"after-this-borrower-runs" hook scoped just to this change rather than
waiting for Phase 4's general invocation-completion mechanism. Rejected
(per explicit user direction when this gap was surfaced) - a one-off
hook for this feature alone risks diverging from whatever shape the
general Phase 4 mechanism eventually takes, duplicating work and
potentially needing to be reworked or removed once that mechanism lands.

**Alternative considered and rejected (original, see above):** lazy,
pull-based return propagation. Still rejected for the same reason as
originally stated - it is exactly the "lazy direction selection"
question REQ-INOUT-002 reserves as unresolved.

### Decision 4: Mismatch, missing-owner, and chained-borrowing pairings fail resolution explicitly
All three unsupported shapes (mismatched payload, borrower with no
identifiable owner, chained/recursive borrowing) are detected during the
same real-connection-resolution step that performs REQ-EXT-003's
characteristics comparison, and reported as an explicit resolution
failure — the same failure-reporting posture `GraphBuilder.F90` already
uses for other resolution failures (e.g. unresolved imports), not a
silent no-op and not a fallback to extension-chain construction.

## Risks / Trade-offs

- **[Risk]** A future change extending this to the general (REQ-INOUT-002)
  case may need a different return-network-per-pairing strategy once
  multiple borrowers per owner or lazy direction selection are in scope
  (Decision 2, revised). → **Mitigation**: this change's own
  `graph/ordinary-inout` capability spec scopes per-pairing wiring to the
  direct-alias case explicitly; REQ-INOUT-002's design addendum is
  expected to revisit Decision 2 if needed, not silently inherit it.
- **[Risk]** Without the runtime propagation trigger (Decision 3, revised),
  a declared inout pairing has correct, validated graph structure but no
  automatic revision propagation from borrower back to owner at `update()`
  time yet - a consumer relying on seeing the borrower's write reflected
  in the owner's node via `update()` alone will not observe it until the
  Phase 4 invocation-completion hook this change defers to lands. →
  **Mitigation**: this is an explicit, surfaced deferral (user-confirmed
  during implementation), not a silent gap - `graph/ordinary-inout`'s own
  spec scenarios describe the structural wiring this change delivers;
  the follow-up change implementing the runtime hook is tracked via this
  design.md section, mirroring how 4e/4g's own "real execution remains
  future work" deferrals were tracked.
- **[Risk]** REQ-DEP-008a's existing validation code may not yet
  recognize the inout forward/return pattern as an allowed exception the
  way it already recognizes the callback get/put pattern. → **Mitigation**:
  confirmed during implementation to need no code change - the forward
  edge writes the borrower's `NodeId` and the return edge writes the
  owner's `NodeId`, two different ids, so `validate_cross_network_writes`
  (`ComponentGraph.F90`) passes by the same structural reasoning that
  already makes it pass for callback get/put (different target ids per
  network, not a coded exception at all). Covered by a regression test
  (task 4.3) rather than a validation-code change.

## Migration Plan

Purely additive: no existing declaration, resolution path, or default
behavior changes for any component that does not declare an inout
borrower. No flag/toggle is introduced because there is nothing to gate
behind a default-off switch — the new branch only activates for items
explicitly marked `is_inout_borrower`, which no existing configuration
sets. No rollback mechanism beyond reverting the change is needed.
