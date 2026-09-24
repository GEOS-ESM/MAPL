## Context

`VariableSpec` (`superstructure/generic/specs/VariableSpec.F90`) is a
single concrete type: one `itemType`, one `short_name`, one set of
aspect-shaping fields, no custom `assignment(=)` (relies on Fortran's
intrinsic default assignment). `ComponentSpec`
(`superstructure/generic/specs/ComponentSpec.F90`) holds a flat
`VariableSpecVector` of these, populated by `add_var_spec`.
`GraphBuilder.F90`'s `graphbuilder_advertise`/`advertise_one` walks that
same flat vector and creates exactly one `StateItemNode` per entry, with
its `GraphStateItem` payload left completely unallocated ("advertised,
not yet realized" — real ESMF realization for even the simple flat case
remains legacy `StateRegistry`'s job via `advertise_variable.F90`/
`StateItemSpec%create()`, a separate, additive path). `GraphStateItem`
already has a fully specified `state_members` map (REQ-SI-006), but
nothing populates it today.

`for_each_matching_import`/`find_export_var_spec`
(`GraphBuilder.F90`) — the procedures ordinary `MatchConnection`
resolution uses to find import/export candidates — iterate
`ComponentSpec%var_specs` only. `build_characteristics(var_spec)` reads
only `var_spec%units`/`var_spec%vertical_grid`.

**Revision history.** This change's first design put a new,
`VariableSpec`-adjacent but separate type (`CompositeStateSpec`/
`CompositeMember`) in a new, separate `ComponentSpec%composite_specs`
vector. Implementing task groups 1-4 against that design surfaced a real
cost during task 5 (writing the connection-resolution test): a composite
item in a *separate* vector is invisible to `for_each_matching_import`/
`find_export_var_spec`, which only ever look at `var_specs` — not a
member-level limitation, a *top-level* one, and one this change's own
stated deferral did not anticipate. Fixing it would have meant
duplicating (or branching) those scan sites. Investigated and adopted
instead: make a composite declaration *be* a `VariableSpec` (see Decisions
below) — eliminates the duplication by construction, since a composite
item then lives in the one vector every relevant procedure already scans.

## Goals / Non-Goals

**Goals:**
- Let a component declare a state's shape as a tree, where every node in
  the tree — leaf or nested — is itself an ordinary `VariableSpec`, with
  zero new declaration type.
- Make `GraphBuilder`'s advertise step create real, individually
  addressable `StateItemNode`s for every level of that tree, using the
  already-specified `state_members` map as the parent-to-child linkage.
- Achieve top-level connectability (a composite item participates in
  ordinary `MatchConnection` resolution exactly like a flat item) as a
  consequence of the representation choice, not as separately-written
  logic.

**Non-Goals:**
- Recursing connection resolution into a composite's members. This
  design makes the tree exist, be walkable, and makes its *top level*
  connectable for free; matching a nested member against an import's own
  nested declaration is a follow-up change's job. **Notes for that
  follow-up:** connecting two composites is expected to mean connecting
  each corresponding member — a single top-level connection expanding
  into a set of member-level connections — tolerant of an import missing
  items an export has, and an export having extra items an import
  doesn't need (asymmetric match, unlike a flat item's exact-name-match
  rule today). Callbacks complicate this further: the import/export side
  is itself blurred there (`15-callbacks.md` §15.5 Handler/Invoker — a
  Handler's callback state sits in its own *export* State but is
  semantically "owned" data from the Handler's perspective). None of this
  is designed or implemented here.
- Member-default/inherited characteristics (e.g. a composite-wide default
  precision or geometry that members could inherit unless overridden).
  Every member's characteristics are independent, exactly as an ordinary
  `VariableSpec` declaration's are today. A defaulting/inheritance
  mechanism is a separable future concern.
- Building a composite declaration from `ESMF_HConfig`/YAML. This change
  provides only the incremental Fortran `declare_member` API; a
  config-driven builder is a plausible, explicitly anticipated future
  extension of the *existing* `ComponentSpecParser/parse_var_specs.F90`
  parser (which already builds `VariableSpec` values from YAML) — not
  designed or built here.
- Real ESMF realization of leaf members (`ESMF_FieldCreate`) or of a
  nested state's real ESMF membership (`ESMF_StateAdd` of real Field/
  FieldBundle objects). Mirrors the existing flat-item precedent exactly.
- `FieldBundle`-internal member declaration.
- Touching `StateItemSpec`, `StateRegistry`, or `ComponentSpec` at all.

## Decisions

**A composite declaration is a `VariableSpec`, not a new type — a
"leaf" member is a `VariableSpec` whose `itemType` is `FIELD`/
`FIELDBUNDLE`; a "nested" member is a `VariableSpec` whose `itemType` is
`STATE` with its own populated `members`.** This is the central decision
of this revision (see Context - "Revision history" for what it replaces
and why). It means: no `CompositeMember`/`CompositeStateSpec` wrapper
type; `declare_member(name, member, rc)` takes one plain `VariableSpec`
argument, with no `leaf=`/`nested=` keyword distinction — the caller's
role is read directly from `member%itemType`, exactly the same
vocabulary every other `VariableSpec` consumer already uses
(`make_ClassAspect`'s own `select case (this%itemType%ot)`, `GraphBuilder`'s
`itemtype_name`). A composite item lives in the same
`ComponentSpec%var_specs` every flat item already lives in — no new
`ComponentSpec` field, no new registration method.

**`declare_member` asserts the parent's `itemType` is already
`MAPL_STATEITEM_STATE`; it does not implicitly set it.** Alternative
considered: auto-promote a spec to composite on first `declare_member`
call. Rejected (confirmed with the spec author): implicit itemType
mutation is a silent side effect on a field every other part of
`VariableSpec`'s machinery (`make_ClassAspect`'s `select case`,
`itemtype_name`) treats as an explicit, caller-declared fact; asserting
instead surfaces a caller's mistake (declaring members on a spec they
forgot to mark `MAPL_STATEITEM_STATE`) immediately and loudly, matching
this codebase's existing convention of explicit `_FAIL`/`_ASSERT` over
silent inference for exactly this kind of "which item-class is this"
question (`GraphBuilder.F90`'s own `itemtype_name`/`unsupported_item_class`
handling).

**A member's own `short_name` field is never read by `declare_member`/
`get_member` — only the map key is a member's identity.** Confirmed with
the spec author. Mirrors REQ-VAL-006 exactly ("a state member name is a
map key and MUST NOT be duplicated inside the mapped value... the node
pointed to does not itself store the name it was found under"). A member
`VariableSpec` may have any `short_name` value (including none set) —
`declare_member`/`get_member` simply never look at it.

**`VariableSpec` is retrofitted to `extends(VariableSpecTag)` (a bare,
empty, zero-dependency abstract marker), and its new `members` field is
a `VariableSpecMemberMap` (`T_polymorphic` over that marker) — the same
mechanism (and for the same reason) this change's first draft used for
`CompositeMember`/`CompositeMemberMap`, just applied to `VariableSpec`
itself.** Fortran does not support a derived type containing a map of
itself without this indirection: the map's own generated module needs
`T` fully defined to instantiate the gFTL2 template, while `VariableSpec`
needs the map type for its own new field — a genuine two-module cycle
(F2003/2008 constraint C455: a component may forward-reference an
undefined derived type only via a pointer/allocatable *self*-reference,
never a different, mutually-defined type). `VariableSpecTag` breaks it:
`VariableSpecMemberMap`'s module needs only the tag (zero dependency on
`VariableSpec`'s own module); `VariableSpec`'s module depends on both the
tag and the map, one-directionally. Verified safe to retrofit: no code
anywhere pattern-matches `VariableSpec`'s concrete type
(`type is (VariableSpec)`/`same_type_as`) or relies on it having no
supertype; `VariableSpec` has no custom `assignment(=)` for a new
abstract ancestor with zero components to disturb; `VariableSpec` is
never constructed via an intrinsic default structure constructor
anywhere (always via `make_VariableSpec()` or direct field assignment),
so there is no positional-constructor call site to break.

**Nested-state materialization allocates a real, structurally-empty
`ESMF_State` (not left fully unallocated like a leaf).** Unchanged from
the first draft's reasoning: `GraphStateItem`'s own two-tier
classification (REQ-SI-002b) requires `esmf_kind()` to report the state
value before `state_members` (REQ-SI-006) is eligible to be populated —
gated on `esmf_kind() == ESMF_STATEITEM_STATE`. This is not "realizing"
the state's contents (no real Field/FieldBundle members are added) any
more than a geometry proxy Field's `ESMF_FIELDSTATUS_GRIDSET` allocation
(REQ-GEO-003) counts as realizing a Field's data. Leaves are unaffected —
they keep today's fully-unallocated "advertised, not yet realized"
`GraphStateItem`.

**The recursive materialization walk lives in a dedicated module
(`mapl_CompositeStateMaterialization_mod`), invoked from a small
recursion branch added directly inside `advertise_one`
(`GraphBuilder.F90`) — no new public `GraphBuilder` entry point, no new
lifecycle hook.** Because a composite item is an ordinary `VariableSpec`
in the same vector `advertise_one` already walks, the recursion check
("does this `var_spec` have populated members?") is a single `if` inside
the existing procedure, not a parallel procedure requiring its own hook
wiring in `initialize_advertise.F90`. Mirrors the existing
`ExtensionResolution`/`ExtensionMaterialization` separation-of-concerns
pattern `GraphBuilder.F90` already depends on for a different concern —
one dedicated module for the graph-structure-building logic, called from
`GraphBuilder.F90`'s own procedure body.

**Top-level connectability is a consequence of the representation, not
separately-implemented logic.** `for_each_matching_import`/
`find_export_var_spec`/`build_characteristics` need zero changes: a
composite item is discovered by name/state-intent exactly like a flat
item, and since it has no `units`/`vertical_grid` of its own,
`find_mismatched_characteristics` between two matched composites always
returns empty — a direct wire, never a spuriously-interposed extension
chain. This is the specific gap the first draft's implementation
surfaced (see Context) and is now closed by construction rather than by
an added scan branch.

**Idempotent re-advertisement checked at the top level only, mirroring
`advertise_one`'s own existing check.** Unchanged from the first draft:
"is this identity already in the resource index" is sufficient to
short-circuit re-advertisement without inspecting the item's members. An
actually-*changed* re-advertisement (same name, different declared
members) is not specially detected, matching `advertise_one`'s existing
behavior for a changed flat item.

## Risks / Trade-offs

- **[Risk]** Retrofitting `extends(VariableSpecTag)` onto `VariableSpec`
  touches a widely-used legacy type. **Mitigation:** the ancestor is
  bare/empty (no components, no deferred procedures) — every existing
  `type(VariableSpec)` declaration, field access, and intrinsic
  assignment continues to work unchanged; verified no code pattern-matches
  `VariableSpec`'s concrete type or relies on it having no supertype
  (design.md Decisions).
- **[Risk]** Allocating a real (if empty) `ESMF_State` for every
  nested-state node at advertise time adds a small fixed ESMF allocation
  cost per nesting level. **Mitigation:** `ESMF_StateCreate` with no
  members is lightweight, same category of cost already accepted for the
  geometry-proxy Field case (REQ-GEO-003), bounded by how deeply a
  component author chooses to nest.
- **[Trade-off]** Because member-level connection resolution is
  explicitly deferred, a composite item's nested members are graph-visible
  but not yet coupler-*reachable* at the member level (the top level is
  reachable, per this revision's central decision). Expected, stated
  split — not a regression relative to today (today there is no
  structure at all).

## Migration Plan

Purely additive to `VariableSpec` (new field, new methods only; no
existing field or method changes behavior). `ComponentSpec`,
`StateItemSpec`, `StateRegistry`, and every existing flat-item
advertise/connection code path are unchanged. A component that never
calls `declare_member` sees no behavior difference whatsoever. Rollback
is a plain revert of this change's commits.
