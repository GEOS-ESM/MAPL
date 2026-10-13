## Context

See proposal.md - Why. The current resolution path
(`GraphBuilder_ConnectionResolution_smod.F90`) treats `ComponentSpec%
var_specs` as a live, re-queryable source of truth throughout
resolution, rather than only at advertise time. Concretely:

- `for_each_matching_import(dst_spec, dst_pt, op, rc)` iterates
  `dst_spec%var_specs` directly (line ~416) to find every candidate
  destination import, filtering by `dst_pt%v_pt%matches(...)`.
- `build_characteristics(var_spec, rc)` (line ~508) builds a fresh
  `CharacteristicMap` from `var_spec%units`/`%typekind`/
  `%vertical_grid`/`%state_item_variant` on every call - there is no
  persistent per-item characteristics state consulted instead, even
  though `GraphStateItem%characteristics` (landed,
  `state-item-characteristics`) already exists for exactly this
  purpose and is simply never populated by `GraphBuilder`.
- `resolve_one`'s branch conditions
  (`var_spec%callback_interface_id%is_valid()`,
  `var_spec%is_inout_borrower`) read `var_spec` fields with no
  `StateItemNode`-level equivalent today.
- `try_materialize_field_extension`/`materialize_extension_chain` reach
  `export_var_spec%ungridded_dims`/`%vertical_stagger` via
  `component_for(this, export_comp_name)` -> a named child's own
  `ComponentSpec` -> that child's `var_specs` - a single-level-only
  reach (`component_for` only handles `"<self>"` or a direct child
  name, per its own existing implementation).
- `find_export_var_spec`/`find_inout_borrower_var_spec` both scan
  `comp_spec%var_specs` linearly by short name - the destination-side
  mirror of the same pattern.

`advertise_one` (`GraphBuilder_Advertising_smod.F90`) is the one place
reading `var_spec` is correct: it is the sole constructor of a
`StateItemNode`/`GraphStateItem` from a `VariableSpec`, exactly once
per identity (idempotent re-advertisement already guards this). This
change does not touch `advertise_one`'s own node-construction logic -
only extends what it populates on the node it already creates.

## Goals / Non-Goals

**Goals:**
- Make `StateItemNode` the sole source of truth for everything
  resolution needs about an already-advertised item: identity
  (`short_name`/`state_intent`), resolution-mode flags
  (`is_inout_borrower`/`callback_interface_id`), mismatch
  characteristics (via the existing `GraphStateItem%characteristics`
  map), and materialization inputs (`ungridded_dims`/
  `vertical_stagger`).
- Make every resolution-time lookup use `NodeId`/`ComponentGraph`
  (resource index, import/export ports) instead of a component name +
  `ComponentSpec%var_specs` scan - removing the implicit "resolution
  can only reach what it can name a component for" limitation that
  blocks multi-level propagation.
- Preserve every existing observable scenario in `graph/graph-builder`'s
  spec exactly - this is a mechanism correction, not a behavior change
  (proposal.md Modified Capabilities note).

**Non-Goals:**
- Any new propagation mechanism (import or export) - this change fixes
  what resolution reads once an item is visible to it; a separate
  prerequisite change (export propagation) and `3b2-cross-component-
  import-propagation` (import propagation) are responsible for making
  a multi-level-distant item visible in the first place.
- `GraphBuilder_CallbackWiring_smod.F90`'s own `var_spec` reads
  (namespace flattening, interface conformance, composite-member
  access) - out of scope per proposal.md.
- Changing `advertise_one`'s own construction logic beyond adding the
  new population steps (characteristics, identity/flag fields) - the
  existing idempotent-re-advertisement guard, composite-member
  handling, and port registration are unchanged.

## Decisions

**`StateItemNode` gains four new fields, set exactly once by
`advertise_one`, with simple get-only accessors (no setter exposed
outside the advertising submodule).** `short_name`/`state_intent`
mirror what `item_key()` already encodes as a composite string key -
exposing them directly on the node avoids needing to re-derive or
re-parse that string anywhere a caller already holds the node (e.g.
`for_each_matching_import`'s own match-filtering, which needs
`short_name`/`state_intent` as typed values, not a string to pattern-
match against a second time). `is_inout_borrower`/`callback_interface_id`
are plain copies of the identically-named `VariableSpec` fields -
no behavior change versus reading `var_spec` directly, just a change
in *where* the read happens (once, at advertise time, instead of
every resolution call). Alternative considered: compute these on
demand from the node's `GraphStateItem` payload instead of storing
them as separate `StateItemNode` fields. Rejected - `GraphStateItem`
represents the item's *payload* (what ESMF handle backs it); intent/
callback-expectation are resolution-time metadata about how the item
should be matched, a different concern, and `GraphStateItem`'s own
existing invariant (at most one of field/field_bundle/state allocated)
has no natural place for either flag.

**`advertise_one` populates `GraphStateItem%characteristics` using the
exact same field-to-characteristic mapping `build_characteristics`
already implements, moved verbatim.** `UnitsCharacteristic`/
`TypeKindCharacteristicAdapter`/`VerticalGridCharacteristic` and their
respective `StateItemCharacteristicKind` constants are unchanged;
only the call site moves from "inside every `resolve_one`/
`check_match_connection_unsatisfied` call, for both sides of every
comparison" to "once, right after a `StateItemNode` is registered in
`advertise_one`." This also fixes a latent inefficiency, not just the
architectural issue: today, two components connected N times (e.g. N
separate `MatchConnection`s referencing the same export) rebuild that
export's `CharacteristicMap` N times; after this change, it is built
once.

**`for_each_matching_import` is rewritten to iterate the destination's
own `ComponentGraph` import ports (via the existing `PortIdNodeIdMap`
iteration already supported by that container) rather than
`dst_spec%var_specs`.** For each import-port `NodeId`, the
corresponding `StateItemNode` is retrieved (`graph%get_node(node_id)`,
already public) and its own `short_name`/`state_intent` fields (new,
this change) are used for the `dst_pt%v_pt%matches(...)` filter -
structurally the same filter condition as before, just sourced from
the node instead of a `var_spec`. `op`'s own signature changes from
`subroutine(var_spec, rc)` to `subroutine(node, rc)` (a `StateItemNode`
instead of a `VariableSpec`) - every caller (`check_one`/`resolve_one`,
both internal to this submodule) is updated accordingly; this is a
private, submodule-internal interface change, not a public API
change (`I_match_op`'s own interface is declared private to
`mapl_GraphBuilder_mod`).

**`component_spec_for`'s own role in resolution shrinks to exactly one
remaining legitimate use: resolving `src_pt%component_name`/
`dst_pt%component_name` to the right `ComponentGraph` for
`for_each_matching_import`'s port-table iteration (replacing today's
`ComponentSpec%var_specs` target).** It is NOT removed - naming a
component and reaching its `ComponentGraph` (not its `ComponentSpec`)
remains necessary; what changes is which collection is read off the
result. `component_spec_for` itself is left as-is (still returns a
`ComponentSpec` pointer, since `component_for`/other callers outside
this change's scope still need it); callers inside this change's own
scope that previously called `component_spec_for` purely to reach
`%var_specs` are changed to call `component_for`+`get_component_graph`
(or the already-existing `get_child_component_graph` for a named
child) instead.

**Materialization's `ungridded_dims`/`vertical_stagger` move onto
`GraphStateItem` as two new fields, populated by `advertise_one`
alongside characteristics, read directly off the export `NodeId`'s own
node by `try_materialize_field_extension`/`materialize_extension_chain`
- removing the `component_for`/`ComponentSpec` reach from both
procedures entirely.** Alternative considered: keep passing
`export_var_spec` into these two procedures but have *callers* resolve
it via the new node-based lookup first (i.e. only move the "how do I
find the right `var_spec`" step, not eliminate `var_spec` from the
procedures' own signatures). Rejected - this would still leave a
`VariableSpec` reference alive past advertise time inside the
materialization procedures' own signatures, which is exactly the
pattern this change exists to eliminate; a future caller reusing
these procedures for a multi-level-propagated export (3b2's own
eventual need) would still have no `var_spec` to pass. Storing the two
fields directly on `GraphStateItem` (next to `characteristics`, same
lifetime, same "set once at advertise time" discipline) removes the
dependency entirely.

**`find_export_var_spec`/`find_inout_borrower_var_spec` are removed
outright, not merely rewritten to search a different collection.**
Both become unnecessary once `for_each_matching_import`'s port-table
iteration and the new `StateItemNode` fields exist: a source-side
export lookup becomes "get the export port's `NodeId` from the source
`ComponentGraph`, then read its node's own fields" (already what
`find_export_node_id` does for the `NodeId` half - this change only
removes its own separate `var_spec`-scanning half); an
inout-borrower-owner lookup becomes an equivalent scan over the
source's own import ports checking each node's `is_inout_borrower`
field instead of each `var_spec`'s.

## Risks / Trade-offs

- **[Risk] `for_each_matching_import`'s port-table iteration order may
  differ from `var_specs`' own insertion order, changing the order in
  which `GraphResolutionEntry` entries are reported (though not which
  entries are reported)** → **Mitigation:** no existing spec scenario
  or test asserts a specific *order* of unresolved/unsupported
  entries, only their presence/count/fields (confirmed by reading
  `Test_GraphBuilder.pf`'s own assertions, which use `%at(1, ...)` only
  for single-entry cases) - this is a safe behavior-preserving change
  per that check, but task verification should re-run the full
  existing suite to confirm no latent order-dependent assertion was
  missed.
- **[Risk] Moving characteristic-building from "every resolution call"
  to "once at advertise time" means a `var_spec` mutated after
  advertising (if any caller does this - not expected, but not
  statically ruled out) would no longer be reflected in resolution's
  view of that item's characteristics** → **Mitigation:** `advertise_one`
  already treats advertisement as a one-time, idempotent event (its
  own existing re-advertisement guard returns immediately if the
  identity already exists) - this change's characteristics population
  follows that same existing contract exactly, not a new one; no
  production code path re-advertises with changed `var_spec` content
  for an already-advertised identity today.
- **[Risk] Removing `find_export_var_spec`/`find_inout_borrower_var_spec`
  and `build_characteristics` outright, rather than keeping them
  as dead code during a transition, makes this a larger single diff
  with no intermediate rollback point** → **Mitigation:** accepted -
  keeping removed-but-unused helpers around contradicts the change's
  own purpose (proving `var_spec` is no longer reachable from
  resolution, proposal.md Impact "new coverage" item) and this
  project's own precedent is clean removal, not deprecation shims, for
  internal-only mechanism changes.
