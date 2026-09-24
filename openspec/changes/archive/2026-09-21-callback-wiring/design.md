## Context

See proposal.md - Why/What Changes for motivation and scope, including
the "Revised approach" note explaining why this design has no dedicated
`Connection` subtype. Relevant existing state this design builds on:

- **`GraphBuilder.F90`** (Phase 3b/3c) — stateless-per-call procedure set
  operating on an `OuterMetaComponent`'s own `ComponentGraph`. Already has
  `component_spec_for`/`component_for` helpers that resolve `<self>` vs. a
  named child to a `ComponentSpec`/`OuterMetaComponent` (via
  `get_child_component_spec`/`get_child_outer_meta`), and
  `get_or_make_local_node_id`/`proxy_key` for parent-local proxy
  `StateItemNode`s standing in for a child's real item. `resolve_match_connection`'s
  own `resolve_one` inner procedure is where every matched
  (destination-import, source-export) pair is currently handled
  unconditionally as an exact/mismatched-characteristic pair
  (`find_mismatched_characteristics`/`find_or_build_extension_chain`) -
  this change adds one new branch there, gated on the destination
  `VariableSpec`'s own `callback_interface_id`, ahead of that existing
  exact-match logic. This change's descendant-namespace walk and
  callback-collection materialization extend this same procedure set; no
  new persistent object is introduced.
- **`VirtualConnectionPt`** (`superstructure/generic/connection/
  VirtualConnectionPt.F90`) — already a `GraphBuilder.F90` dependency
  (used to construct the synthetic import-pattern `VirtualConnectionPt`
  every `for_each_matching_import` call already builds). `matches(this,
  item)` already does exactly the POSIX-regex comparison this change
  needs (`regcomp('^'//this%get_full_name()//'$')` against
  `item%get_full_name()`, via `utils/regex`) - this change reuses it
  unchanged rather than writing a second regex-matching routine.
  `get_full_name()` = `comp_name // '/' // short_name` when `comp_name`
  is set, and `StateRegistry_Propagation_smod.F90`'s
  `propagate_exports_virtual_pt` already builds exactly this qualified
  name while bubbling a subregistry's exports up to its parent
  (`VirtualConnectionPt(virtual_pt, comp_name=subregistry_name)`). This
  is the "existing flattening mechanism" REQ-CB-013/014 require reusing
  (the naming convention and the parent-bubbles-children shape),
  reimplemented graph-natively rather than by calling into
  `StateRegistry` itself (`GraphBuilder`'s own layering keeps it
  independent of `StateRegistry`'s subregistry/family machinery,
  matching 3b/3c's precedent of building graph-native equivalents rather
  than wrapping legacy machinery - `resolve_match_connection`'s own
  header comment: "recomputes it directly against
  ComponentSpec%var_specs... rather than going through StateRegistry's
  subregistry/family machinery").
- **`CallbackInterface`/`CallbackInterfaceRegistry`/`CallbackStateBinding`**
  (Phase 4c, `superstructure/generic/graph/Callback*.F90`) — the
  registrable contract and per-state binding this change wires up to a
  real hierarchy. `CallbackStateBinding` already validates argument/
  method names against a supplied `CallbackInterface` and stores
  `argument name -> member NodeId` via the existing `StateItemMemberMap`
  — this change is its first caller to supply *real*, `GraphBuilder`-
  resolved member `NodeId`s (from `composite-state-spec`'s
  `declare_member` tree) instead of test-synthetic ones.
- **`MethodGraphNode`/`StateMethodInvocation`** (Phase 4a) — the node
  type and adapter this change's `CallbackMethodBinding` points at.
  `MethodGraphNode` is permanently excluded from demand-driven update
  (`method-graph-node`'s own "never scheduled by demand-driven update"
  requirement) - this change's `invoke_callback_method` is the only path
  that ever calls a callback `MethodGraphNode`'s `invoke()`.
- **`mapl_MethodInvocation_mod%invoke_on_default_network`**
  (`MethodInvocation.F90`, Phase 4a/4b) — the existing REQ-REV-011 pull-
  before/invoke/advance-after discipline for a `MethodGraphNode`, today
  hardcoded to `graph%get_default_network_id()`. This change's
  `invoke_callback_method` mirrors its exact three-phase shape
  (collect-then-process, explicit indexed loops, no map iterator live
  across a `graph%update()`/`advance_revision()` call - that file's own
  header explains why) but parameterized by the get/put networks a
  `CallbackMethodBinding` names instead of the default network.
- **`GraphStateItem%add_state_member(name, id, rc)`**
  (`superstructure/generic/graph/GraphStateItem.F90`) — already public,
  already used by `CompositeStateMaterialization.F90` to populate a
  nested level's `state_members` map from a static `VariableSpec` tree.
  This change's callback-collection materialization calls the same
  method directly from a dynamically-resolved match list - no new
  "add a member to a composite state item" primitive is needed.
- **`ComponentGraph%create_network()`/`add_dependency()`/
  `bind_port()`** (Phase 1-2) — already support creating any number of
  named `DependencyNetwork`s per graph and wiring `TransformGraphNode`s
  into them. This change's per-method get/put networks are ordinary
  uses of this existing API, not an extension of it.
- **`ExtensionResolution.F90`/`find_or_build_extension_chain`** (3c) —
  the existing "insert a transform chain when characteristics mismatch"
  machinery this change's get/put network construction reuses verbatim
  for any callback argument whose provider representation does not
  already match the callback interface's declared `expected_kind`/
  characteristics.
- **`VariableSpec%itemType == MAPL_STATEITEM_SERVICE` /
  `ServiceClassAspect`** (`superstructure/generic/specs/VariableSpec.F90`,
  legacy) — the framework's own existing precedent for "this declared
  item needs special handling beyond a plain typed value," expressed as
  a marker on the item, not on the connection that wires it. This
  change's `callback_interface_id` field follows the same shape (a new
  field, not a new `itemType`, since a callback-consuming import is
  still an ordinary composite `STATE` item in every other respect).

## Goals / Non-Goals

**Goals:**
- Resolve an ordinary `MatchConnection` whose destination declares a
  `callback_interface_id` against a real, hierarchy-wide flattened
  export namespace built from real advertised `ComponentSpec`s
  (REQ-CB-013/014/016), without introducing a new `Connection` subtype
  or changing behavior for any connection whose destination does not
  declare one.
- Materialize one real, individually-addressable collection of matched
  callback states for the Invoker (REQ-CB-016's "flat callback
  collection").
- Provide `CallbackMethodBinding` and per-method get/put
  `DependencyNetwork`s wired through existing Phase 1-2 primitives
  (REQ-CB-018/019).
- Provide `invoke_callback_method` implementing the invoke-once-after-
  all-ready discipline (REQ-CB-020), without adding callback invocation
  to demand-driven update's own dispatch table.
- Make `CallbackStateBinding` (4c) usable end-to-end with real,
  `GraphBuilder`-resolved member `NodeId`s from a real advertised
  composite callback state.

**Non-Goals:**
- A `ComponentSpecParser`/YAML key or `SetServices`-convenience wrapper
  for setting `callback_interface_id` (proposal.md's own explicit
  deferral) - `VariableSpec` has no `private` clause, so plain field
  assignment already works and is what every test in this change uses;
  a `make_VariableSpec()` keyword was tried and reverted after review
  found it had no real caller anywhere outside its own unit test (the
  real `ComponentSpecParser` call site does not pass it, and nothing
  else does either) - speculative API surface this change's own scope
  does not need, mirroring `composite-state-spec`'s identical posture
  for `declare_member`.
- Resolving REQ-CB-011/012's `[OPEN]` ESMF-placement question (Q7) or
  REQ-CB-006's `[OPEN]` Handler/Invoker terminology question (Q6) - this
  change's API is agnostic to both, exactly like 4c's.
- A real `ESMF_MethodAdd`/`ESMF_MethodExecute`-backed `StateMethodInvoker`
  - still exercised only through the existing synthetic test-double
  invoker in tests; attaching a real invoker to a real `GridComp`'s
  callback method remains separate follow-up work, not blocked on
  anything this change introduces.
- Recursive matching for ordinary (non-callback) composite-to-composite
  connections - `composite-state-spec`'s own explicit deferral, untouched
  here. This change's namespace walk only ever inspects a component's
  *top-level* advertised items (exactly what `find_export_var_spec`
  already does for ordinary connections) plus, for a callback state
  specifically, that state's own declared member tree (needed to
  populate `CallbackStateBinding`'s argument bindings) - it does not
  attempt member-level matching for arbitrary composite exports.
- A general-purpose "any regex against any state" query API - pattern
  matching here is exactly `VirtualConnectionPt%matches()`, applied to
  this change's own flattened namespace; nothing new is exposed as a
  standalone utility.
- A new declarative `Connection` subtype for callbacks (Decision 0) -
  the connection stays an ordinary `MatchConnection`/`SimpleConnection`.

## Decisions

**0. Callback-ness is declared on the destination `VariableSpec`
(`callback_interface_id`), not on a new `Connection` subtype.**
The first draft of this change introduced `CallbackConnection`, a new
`Connection` subtype carrying its own regex source pattern and expected
`CallbackInterfaceId`. Design review rejected it: `SimpleConnection`/
`MatchConnection` exist precisely so a component author only ever
declares *where* two items are; `MatchConnection`'s own
`VirtualConnectionPt` is already regex-capable (`connect_all`'s
`short_name='^.*$'` idiom, already exercised by this codebase's own
tests) - a second, callback-specific "special connection" type would
duplicate that matching machinery under a new name rather than reuse it.
The field instead lives on `VariableSpec`, mirroring the framework's own
existing "mark the item, not the connection" precedent
(`MAPL_STATEITEM_SERVICE`/`ServiceClassAspect`). `GraphBuilder`'s
existing `resolve_match_connection`/`check_match_connection_unsatisfied`
dispatch - already iterating every matched destination import via
`for_each_matching_import` - checks the matched `VariableSpec`'s own
`callback_interface_id%is_valid()` and branches to this change's
resolution instead of the ordinary exact-match/extension-chain path,
exactly at the point where each match is already discovered. A
destination with no `callback_interface_id` set (`is_valid() ==
.false.`, the default) takes the existing path completely unchanged -
zero behavior change for every connection declared before this change.
*Alternative considered:* keep a `CallbackConnection` type but have it
compose a `MatchConnection` internally instead of duplicating its logic.
Rejected: it would still be a second declared `Connection` type a
component author must choose instead of the one they already use for
every other wiring decision, for no capability the item-marker approach
does not already provide - the field-on-item approach costs one new
`VariableSpec` field (additive, default-unset) against a whole new
public type family.

**1. The flattened qualified-export namespace is built by a fresh,
uncached recursive walk at resolution time, rooted at the connection's
own declared source component (not necessarily `this`), not a
maintained/cached index.**
`GraphBuilder`'s existing cross-boundary connection resolution
(`get_or_make_local_node_id`) already walks one level of
`get_child_component_spec`/`get_child_outer_meta` per call, uncached,
relying on `ComponentGraph`'s own resource index for memoization where
it matters (proxy dedup via `proxy_key`). This change's namespace walk
starts from the `OuterMetaComponent` the connection's own source
`ConnectionPt%component_name` resolves to (via the existing
`component_for` helper - `<self>` or a named child of the component
declaring the connection, exactly like ordinary resolution's own
scoping) and recurses across *all* descendant levels below it (not just
one), producing a `character -> (comp_path, VariableSpec)`-shaped lookup
scoped to one resolution call. *Alternative considered:* maintain a
persistent, incrementally-updated namespace cached on
`OuterMetaComponent`. Rejected for this change: advertising order across
a hierarchy is not fully settled by the time callback wildcard
resolution needs to run (mirrors REQ-CB-016's own "expand the wildcard
against the *currently-known* qualified export namespace" phrasing - a
one-shot snapshot at resolution time is what the spec asks for, not a
live-updated index), and a persistent cache would need its own
invalidation story this change does not need to design. Revisit only if
profiling shows repeated whole-hierarchy walks are a real cost on large
configurations - not assumed up front.

**2. Qualified names reuse `VirtualConnectionPt`'s `comp_name/short_name`
convention textually, without depending on `StateRegistry` or
`ExtensionFamily`.**
The namespace walk builds plain `character` keys of the form
`comp_path // '/' // short_name` (composing one segment per hierarchy
level for a multi-level descendant, e.g. `CHEM/DU/tracers`), matching
`get_full_name()`'s own single-level `comp_name // '/' // name` shape
generalized to arbitrary depth. Matching itself (Decision 3, below)
constructs a real, `comp_name`-less `VirtualConnectionPt` from each
qualified name and calls its own `matches()` - so, unlike the first
draft, this change *does* depend on `VirtualConnectionPt` directly
(already an existing `GraphBuilder.F90` dependency), just not on
`StateRegistry`/`ExtensionFamily`'s subregistry/propagation machinery
built around it (`GraphBuilder`'s own established layering, module
header). REQ-CB-014's "reuse the existing mechanism" is satisfied at
both the naming-convention/hierarchy-bubbling-shape level and, now, the
matching-implementation level.

**3. Wildcard/regex matching is `VirtualConnectionPt%matches()`, called
once per candidate qualified name from Decision 1's snapshot - no
separate regex helper, no direct `utils/regex` dependency.**
For each namespace entry, construct `candidate = VirtualConnectionPt(
ESMF_STATEINTENT_EXPORT, entry%qualified_name)` (no `comp_name` - the
qualified name is already fully composed) and call
`connection_source_v_pt%matches(candidate)`, where
`connection_source_v_pt` is the `MatchConnection`'s own declared source
`VirtualConnectionPt` (already carrying whatever regex short_name the
component author wrote, e.g. `'.*tracers'`). `matches()` internally
`regcomp`s its own pattern per call - the same per-candidate cost
`MatchConnection`'s own ordinary `activate()`/`connect()` filtering
already pays for every wildcard connection in the codebase today, so
this introduces no new cost profile. *Alternative considered (carried
over from the first draft):* a dedicated `compile_once`/`exec_per_
candidate` pair wrapping `utils/regex_module` directly, compiling the
pattern once per resolution rather than once per candidate. Rejected:
would need its own new module-level dependency on `utils/regex` for a
performance shape `VirtualConnectionPt%matches()` itself does not
already provide either (it does not expose a "compile once" entry point)
- matching the existing method's own cost profile is simpler and more
consistent than introducing a faster path only this one capability uses.

**4. `CallbackInterface` conformance validation checks argument names
and kinds, not method presence.**
An export "implements" the expected `CallbackInterface`
(REQ-CB-016 step 2) is checked by confirming the matched export's own
declared composite member names (via `VariableSpec%get_member_names()`/
`get_member()`, the same API `composite-state-spec` already provides)
cover every argument name the `CallbackInterface` declares, with each
member's `expected_kind` (from `CallbackArgumentSpec`, Phase 4c)
matching that member's own advertised `itemType` (compared by name -
`MAPL_StateItem_Flag%to_string()`'s vocabulary and `itemtype_name()`'s
existing ESMF-domain vocabulary already agree on 'FIELD'/'FIELDBUNDLE'/
'STATE' for the base kinds this check needs). Method presence
(`get`/`put`, etc.) is not separately checked at match time - a
`CallbackStateBinding`'s later `bind_method` call is what actually
attaches (or fails to attach) each method, at which point a missing
method attachment surfaces through that already-specified rejection
path, not a duplicate check here. *Alternative considered:* require the
matched export's `ComponentSpec` to separately declare which methods it
implements, checked at wildcard-match time. Rejected: `15-callbacks.md`
does not specify any such method-declaration mechanism at the
`ComponentSpec`/advertise level (methods are attached at `bind_method`
time in Phase 4c's model, an `ESMF_GridComp`/callback-state-owning-
component concern, not an advertise-time one) - inventing one here would
add speculative surface area the spec does not ask for.

**5. Callback-collection materialization creates one new `StateItemNode`
per resolved match set, built directly via `GraphStateItem%
add_state_member`, wired to the destination import via an ordinary
`add_dependency` edge - not routed through
`CompositeStateMaterialization`'s `VariableSpec`-tree walk, and not by
mutating the destination import's own already-advertised node payload.**
`CompositeStateMaterialization.materialize_composite` exists
specifically to walk a *statically declared* `VariableSpec` member tree
(`composite-state-spec`'s own scope); a wildcard-matched collection has
no such static declaration - its membership is discovered at resolution
time from the flattened namespace. The materialization code therefore:
allocates a fresh `NodeId`, constructs a `GraphStateItem` of `esmf_kind
== ESMF_STATEITEM_STATE` with a real, structurally-empty `ESMF_State`
(mirroring `CompositeStateMaterialization`'s own nested-level treatment,
reused verbatim here rather than reinvented), registers a
`StateItemNode` wrapping it in the destination component's graph, calls
`add_state_member(matched_qualified_name, matched_node_id, rc)` once per
resolved match, and is wired to the already-advertised destination
import's own `NodeId` via `ComponentGraph%add_dependency()` on the
connection's default network - exactly the same "producer node feeds a
consumer import via a dependency edge" shape `resolve_match_connection`'s
own ordinary/extension-chain branches already use, which also gets
`ComponentGraph%validate()`'s one-producer-per-item check for free.
Idempotency (spec scenario "re-resolving does not duplicate collection
members") reuses `find_or_build_extension_chain`'s own established
reuse-key convention: a resource-index key derived from the destination
import's `NodeId` is checked before rebuilding, mirroring `chain_key`'s
own "resource-index-as-memo" idiom rather than inventing a second one.
*Alternative considered:* synthesize a throwaway `VariableSpec` with one
member per match and feed it through the existing `materialize_composite`
entry point. Rejected: `materialize_composite` recursively re-derives
each member's own leaf-vs-nested role and payload from the `VariableSpec`
tree - for a callback collection every member is already a fully-
advertised, already-`NodeId`-identified export, so routing through the
`VariableSpec`-tree path would require constructing throwaway
`VariableSpec` shadows of already-real graph structure for no benefit.
*Alternative considered:* mutate the destination import's own
already-advertised `StateItemNode` payload in place instead of creating
a separate producer node. Rejected: every other `GraphBuilder` wiring
path (ordinary match, extension chain) represents "the value that feeds
this import" as a distinct producer node connected via `add_dependency`,
never by overwriting the import's own node payload directly - keeping
the callback case consistent with that shape (rather than a special
case) is what makes `ComponentGraph%validate()`'s one-producer check,
`add_dependency`'s own idempotent-set semantics, and freeze/validate
ordering all apply for free, with no callback-specific carve-out needed
anywhere else in `ComponentGraph`.

**6. `CallbackMethodBinding` stores argument source/target `NodeId`s as a
plain `StateItemMemberMap` per direction (get/put), keyed by argument
name, mirroring `CallbackStateBinding`'s own map-per-concept shape.**
Two maps (`get_bindings`, `put_bindings`, both `character -> NodeId`)
rather than one map keyed by `(argument_name, direction)` composite key
- matches `CallbackStateBinding`'s own established precedent of "one
plain gFTL map per concept, not one map with a compound key"
(`CallbackMethodAttachmentMap`, Phase 4c). `get_network_id()`/
`get_get_network_id()`/`get_put_network_id()` expose the get network as
"the" primary network identity (REQ-CB-019's own singular phrasing) plus
unambiguous per-direction accessors. *Alternative considered:* a single
map keyed by a new compound `(argument_name, AccessSpec)` type. Rejected:
REQ-CB-018 already frames get/put as two named networks, not one network
with per-entry direction tags - two maps mirrors that framing directly
and needs no new key type.

**7. `invoke_callback_method` mirrors
`mapl_MethodInvocation_mod%invoke_on_default_network`'s exact three-phase
shape (collect bound names, pull get-network inputs via
`ComponentGraph%update()`, invoke, advance put-network outputs only on
success), parameterized by a `CallbackMethodBinding`'s own get/put
networks instead of the default network - never registering the
callback `MethodGraphNode` into demand-driven update's own dispatch.**
For each of a binding's get-network argument `NodeId`s (drawn from
`get_bindings`, the argument's *provider-side* source), calls
`graph%update(get_network_id, provider_id, rc)` before invoking the
bound `MethodGraphNode`; after a successful invocation, advances the
`NodeRevision` of every put-network argument's *provider-side* target
(`put_bindings`) directly, the same `advance_revision()` call
`advance_bound_outputs` already makes. `MethodGraphNode`'s own exclusion
from demand-driven update (`method-graph-node` capability) is preserved
because this call graph never registers the *callback* method node
itself for automatic dispatch - only its upstream/downstream data
dependencies are pulled/advanced via the ordinary mechanism, exactly the
same pattern 4a/4b already established for the structurally identical
GridComp-phase case. *Alternative considered:* generalize
`invoke_on_default_network` itself to take a network id parameter and
call it directly. Rejected: `invoke_on_default_network` reads argument
access modes from the `MethodGraphNode`'s own single flat
`ArgumentSpecMap`/`StateItemMemberMap` (one map, not direction-split);
this change's callback method arguments are split across two
`CallbackMethodBinding` maps by direction, a different shape that needs
its own (structurally parallel, but not literally shared) traversal
rather than retrofitting the existing procedure's signature for a shape
it was not designed around. *Alternative considered:* give
`CallbackMethodBinding` its own independent readiness-tracking flag set,
separate from `DependencyNetwork`'s existing revision machinery.
Rejected: would duplicate exactly the staleness-tracking `NodeRevision`/
`DependencyNetwork` already provide, risking the two falling out of
sync.

## Risks / Trade-offs

- **[Risk]** A whole-hierarchy recursive walk on every wildcard
  resolution (Decision 1) could be measurably slow on very deep or wide
  component hierarchies with many callback connections, since it is not
  cached across calls. → **Mitigation:** scoped explicitly as a
  known, accepted trade-off (Decision 1's own "revisit only if profiling
  shows a real cost" - not attempted speculatively); `GraphBuilder`'s
  existing resolution calls already run once per connection at
  advertise/connect time, not per-timestep, bounding the practical
  frequency.
- **[Risk]** Decision 2/3's "reuse `VirtualConnectionPt` directly"
  approach means the graph-native qualified namespace and the legacy
  `StateRegistry`'s own `VirtualConnectionPt`-based qualified names
  share the matching implementation but are still built by two
  independent walks (this change's recursive `ComponentSpec` walk vs.
  legacy's `propagate_exports_all`), which could silently drift apart in
  naming behavior for an edge case in multi-level nesting. →
  **Mitigation:** a test in this change's own suite constructs a
  multi-level descendant hierarchy and asserts the graph-native
  qualified name for a given descendant export matches what
  `VirtualConnectionPt%get_full_name()` would produce for the same
  hierarchy via legacy's own single-level convention, applied
  recursively - a cross-check, not a shared implementation.
- **[Trade-off]** Decision 4's interface-conformance check (member names/
  kinds only, no method-presence check at match time) means a wildcard
  match can succeed against an export that later fails `bind_method`
  entirely (e.g. it has the right `tracers` member but never attaches a
  `get`/`put` method at all). This surfaces as a `bind_method` failure
  reported at binding time rather than at match time. Acceptable: 4c's
  own `CallbackStateBinding.bind_method` already reports this rejection
  explicitly; moving the check earlier would require inventing a new
  advertise-time method-declaration mechanism the spec does not
  otherwise call for.
- **[Trade-off]** `invoke_callback_method` pulls upstream producer
  readiness through the same mechanism ordinary demand-driven update
  uses (Decision 7), so a callback argument whose provider representation
  sits behind an expensive, deeply-chained transform pays that full
  chain's cost on every invocation request - no caching beyond what
  `NodeRevision` already provides. Acceptable: identical cost profile to
  every other consumer of a transformed value already accepted elsewhere
  in the graph-native design.
- **[Trade-off]** A `VariableSpec` with `callback_interface_id` set but
  used outside a `MatchConnection` (e.g. referenced only by a
  `SimpleConnection`, or never connected at all) is simply never resolved
  as a callback - `GraphBuilder`'s branch only triggers from inside
  `resolve_match_connection`. Acceptable and consistent: this mirrors
  `15-callbacks.md`'s own REQ-CB-017 example, which is itself expressed
  as a pattern-matching (`MatchConnection`-shaped) connection; a
  `SimpleConnection`-declared callback import is not a case the spec
  describes, and is unresolved the same way any other never-connected
  import is - reported, not silently accepted as satisfied.

## Migration Plan

Not applicable — purely additive new module, new `VariableSpec` field
(default unset), and new `GraphBuilder` procedures/branch; no existing
`GraphBuilder` procedure's behavior changes for any connection that does
not declare `callback_interface_id`, no data migration, nothing to roll
back beyond reverting the new files/field/procedures.
