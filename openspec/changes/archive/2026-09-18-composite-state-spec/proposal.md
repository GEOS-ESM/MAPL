## Why

`VariableSpec` (`superstructure/generic/specs/VariableSpec.F90`) can only
declare one flat item per advertise call: `itemType` is a single
`ESMF_StateItem_Flag`, and even a `MAPL_STATEITEM_STATE` declaration
produces an empty `ESMF_State` with no declared members
(`make_ClassAspect` just wraps `StateClassAspect(state_intent,
standard_name)`). There is no way to say "and here are the named members
this state contains, each with its own spec." Populating a nested
state's contents today is entirely imperative user code inside the
component, invisible to `GraphBuilder`, extension-reuse, and every
characteristic-based coupling mechanism.

The graph-native docs (`docs/graph/spec/01-19`) don't fill this gap
either — they only describe the *result* of composite structure, never
its *declaration*: `04-graph-value-hierarchy.md` REQ-SI-006 gives
`GraphStateItem` a `state_members` map, but that map is populated only
after member `StateItemNode`s already exist. This blocks any
coupler-visible composite export/import in general, and specifically
blocks the callback-interface capability (`15-callbacks.md`), whose
`CallbackStateBinding` requires each callback argument to resolve to an
individually addressable member `NodeId` — composite items cannot
satisfy that today.

**Revised approach, superseding this change's first draft.** The first
draft of this change introduced a wholly new, parallel declaration type
(`CompositeStateSpec`/`CompositeMember`) alongside `VariableSpec`, with
its own `ComponentSpec` storage (`composite_specs`) and its own
`GraphBuilder` advertise entry point (`graphbuilder_advertise_composite`).
Implementing that revealed a real cost: `GraphBuilder`'s existing
connection-resolution scan (`for_each_matching_import`/
`find_export_var_spec`) iterates only `ComponentSpec%var_specs` — a
composite item living in a *separate* vector would never be discovered
by an ordinary `MatchConnection`, the wiring mechanism every real MAPL
configuration uses, not because of any member-level limitation but
because the parallel vector is invisible to code that only knows about
`var_specs`. Fixing that would have meant duplicating (or branching)
every place that scans `var_specs` to also scan `composite_specs`.

Investigation confirmed a cleaner alternative: `VariableSpec` has no
custom `assignment(=)`, no code anywhere pattern-matches its concrete
type, and its own aspect-building code (`make_ClassAspect`'s
`MAPL_STATEITEM_STATE` case) reads only `standard_name`/`state_intent` —
so `VariableSpec` can be extended *additively* with member storage
directly, with zero risk to any existing (non-composite) use. Doing so
means a composite declaration simply *is* a `VariableSpec` (with
`itemType == MAPL_STATEITEM_STATE` and populated members, each itself an
ordinary `VariableSpec`), living in the same `ComponentSpec%var_specs`
every other declared item already lives in — every existing
`var_specs`-scanning code path (advertising, ordinary connection
matching, characteristic-mismatch detection) picks it up with **no
scan-site changes**, for free. It also means a future YAML/`ESMF_HConfig`
declaration path (an anticipated direction, not built by this change) is
a natural, small extension of the *existing*
`ComponentSpecParser/parse_var_specs.F90` parser, rather than a new
parallel mechanism.

## What Changes

- `VariableSpec` gains a `members` map (name → `VariableSpec`) and three
  new methods: `declare_member(name, member, rc)`, `has_member(name)`,
  `get_member(name, rc) -> VariableSpec`, `get_member_names() ->
  StringVector`. A "leaf" member is simply a member whose own `itemType`
  is `FIELD`/`FIELDBUNDLE`; a "nested" member is simply a member whose
  own `itemType` is `STATE` with its own populated `members` — no
  wrapper type, no `leaf=`/`nested=` keyword distinction: `declare_member`
  takes one plain `VariableSpec` argument and its role is read directly
  from its own `itemType`. `declare_member` asserts the *parent*'s
  `itemType` is already `MAPL_STATEITEM_STATE` (fails loudly otherwise —
  no implicit itemType mutation). A member's own `short_name` field is
  never read by `declare_member`/`get_member` — only the map key is a
  member's identity (mirrors REQ-VAL-006: "a state member name is a map
  key... the node pointed to does not itself store the name it was found
  under").
- `GraphBuilder.F90`'s existing `advertise_one` gains a recursion branch:
  when a `VariableSpec` has populated members, it builds a real,
  individually addressable `StateItemNode` tree (via a new
  `mapl_CompositeStateMaterialization_mod`) instead of leaving the
  top-level payload fully unallocated. Leaf members still get exactly
  today's "advertised, not yet realized" unallocated `GraphStateItem`
  payload; a nested-state level gets a `GraphStateItem` with its
  `esmf_state` component allocated (a freshly created, structurally
  empty `ESMF_State` — real ESMF membership is not populated by this
  change) and its `state_members` map (REQ-SI-006) filled in with each
  declared child's `NodeId`.
- **No changes** to `for_each_matching_import`, `find_export_var_spec`,
  `resolve_match_connection`, `check_match_connection_unsatisfied`, or
  any other connection-resolution code: a composite `VariableSpec`'s
  top-level identity is discovered by these exactly like any other
  declared item's, since it lives in the same `var_specs` vector.
  `build_characteristics` naturally produces an empty `CharacteristicMap`
  for a composite item (it has no `units`/`vertical_grid` of its own),
  so two matched composites always direct-wire, never spuriously
  interpose an extension chain.
- **No changes** to `ComponentSpec` at all — no new field, no new
  registration method. `add_var_spec` already suffices.
- **Explicit deferral:** connection resolution is NOT extended to recurse
  into a composite's members by this change — only the top-level item
  participates in ordinary connection resolution, exactly as today (and,
  as established above, for free — no new code needed for that part).
  Connecting two composites is expected to eventually mean connecting
  each corresponding member — a single top-level connection expanding
  into a set of member-level connections — tolerant of an import
  composite missing items an export has, and an export composite having
  extra items an import doesn't need (asymmetric match, not exact set
  equality). Callbacks complicate this further: the import/export side is
  itself blurred there (`15-callbacks.md` §15.5 Handler/Invoker). None of
  this matching logic is implemented by this change; recorded here so a
  follow-up change starts from this shape rather than rediscovering it.
- **Explicit deferral:** member-default/inherited characteristics (e.g. a
  composite-wide default precision or geometry that member leaves could
  inherit unless overridden) are not addressed. Every member's
  characteristics are independent, exactly as an ordinary `VariableSpec`
  declaration's are today. A defaulting/inheritance mechanism is a
  separable future concern.
- **Explicit deferral:** building a composite declaration from
  `ESMF_HConfig`/YAML. This change provides only the incremental Fortran
  `declare_member` API; a config-driven builder (a natural, small
  extension of the existing `parse_var_specs.F90` parser, per "Why"
  above) is a plausible, explicitly anticipated future entry point but
  is not built here.
- **Explicit deferral:** `FieldBundle`-internal member declaration
  (naming individual Fields inside one `ESMF_FieldBundle` at declare
  time) is out of scope — only State-level nesting is addressed.
- **Explicit deferral:** real ESMF realization (`ESMF_FieldCreate`/actual
  `ESMF_StateAdd` of real members) for either leaves or nested states —
  remains exactly as deferred as it already is for flat items today.
- No change to `StateItemSpec`/`StateRegistry` or any existing
  flat-item legacy advertise/connection behavior — `make_ClassAspect`'s
  `MAPL_STATEITEM_STATE` case reads only `standard_name`/`state_intent`,
  never `members`, so a composite `VariableSpec` still produces today's
  same empty, opaque `ESMF_State` through the legacy path, unaffected.

## Capabilities

### New Capabilities
- `graph/composite-state-spec`: the composite-declaration behavior
  (declare a state's shape as a tree of named members, each independently
  characterized, at arbitrary depth) — now realized as an extension of
  `VariableSpec` rather than a new parallel declaration type, but the
  *behavioral* contract is unchanged from the first draft.

### Modified Capabilities
(none — the earlier draft's `graph/graph-builder` delta is withdrawn:
`GraphBuilder`'s advertising/connection-resolution *requirements* are
unaffected; only `advertise_one`'s internal implementation gains a
recursion branch, which is not an externally observable requirement
change.)

## Impact

- **Affected code**:
  - `superstructure/generic/specs/VariableSpec.F90`: new `members` field
    and `declare_member`/`has_member`/`get_member`/`get_member_names`
    methods, additive only.
  - New files `superstructure/generic/specs/VariableSpecTag.F90` (bare
    abstract marker) and `superstructure/generic/specs/
    VariableSpecMemberMap.F90` (polymorphic gFTL2 map over that marker) —
    needed to let `VariableSpec` contain a map of itself without a
    circular module dependency (Fortran does not support mutual
    recursion between two named derived types across modules except via
    same-type self-reference or polymorphism over a common,
    already-defined ancestor).
  - `superstructure/generic/graph/CompositeStateMaterialization.F90`: the
    recursive advertise-time walk, now operating on `VariableSpec`
    directly.
  - `superstructure/generic/GraphBuilder.F90`: `advertise_one` gains a
    recursion branch; no new public entry point, no new hook.
  - `superstructure/generic/OuterMetaComponent/initialize_advertise.F90`:
    no change (no separate hook to wire in).
- **New Fortran types**: `VariableSpecTag` (abstract marker),
  `VariableSpecMemberMap`. No change to `GraphStateItem`'s own structure
  (REQ-SI-006's `state_members` map already exists; this change is its
  first real populator).
- **Dependencies**: `VariableSpec`'s module gains a dependency on its own
  new tag/map modules (self-recursive, via polymorphism to satisfy
  Fortran's module-cycle constraint) — no dependency on `ComponentGraph`
  or `GraphBuilder`, preserving `VariableSpec`'s existing layering.
- **Tests**: new unit tests for `VariableSpec`'s member API (declare/
  retrieve members, duplicate-name rejection, arbitrary depth,
  independent per-member characteristics, no parent-level characteristic
  leakage) and new `GraphBuilder` tests confirming a composite item's
  node tree is built correctly *and* that it is discovered by ordinary
  `MatchConnection` resolution with no new connection-resolution code
  (closing the gap the first draft's implementation surfaced).
- **Out of scope**: recursive/member-level connection resolution inside a
  composite tree; real ESMF realization; `FieldBundle`-internal member
  declaration; a YAML/HConfig-driven builder; a convenience
  SetServices-facing macro.
