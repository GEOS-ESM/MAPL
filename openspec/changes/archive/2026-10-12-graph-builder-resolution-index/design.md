## Context

See proposal.md - Why. The two existing single-level reaches this
change replaces/wraps:

- `component_spec_for(this, comp_name, rc)` (`GraphBuilder.F90`):
  `is_self(this, comp_name)` -> `this%get_component_spec()`, else
  `this%get_child_component_spec(comp_name, _RC)` - one hop, no
  recursion. Used today by `check_match_connection_unsatisfied`/
  `resolve_match_connection` to resolve `src_pt%component_name`/
  `dst_pt%component_name` before reading `%var_specs` off the result
  (the `var_specs` read itself is `graph-builder-stateitem-resolution`'s
  own concern, not this change's - this change only fixes how far the
  *component* lookup reaches, independent of what is read off it once
  found).
- `resolve_descendant(root, comp_path, rc)`
  (`GraphBuilder_CallbackWiring_smod.F90`): already does exactly the
  multi-level walk this change needs, via `get_child_outer_meta` one
  `/`-separated segment at a time - but it is private to that one
  submodule and keyed to callback-wiring's own call sites
  (`materialize_callback_collection`, `build_callback_state_binding`).

Both existing helpers are built from the same two already-public
`OuterMetaComponent` accessors: `get_child_outer_meta` (one-hop reach,
3b's REQ-GB-002 carve-out) and (for enumeration)
`get_num_children`/`get_child_name`.

## Goals / Non-Goals

**Goals:**
- Provide one shared, multi-level qualified-path resolution primitive
  (`resolve_qualified_component`) that both ordinary connection
  resolution and callback-wiring can call, replacing `resolve_descendant`'s
  own private duplicate and `component_spec_for`'s own single-level
  dispatch where it is used for component lookup (not for the
  `ComponentSpec` read itself, which stays as-is here - see
  "Non-Goals").
- Provide one shared enumeration primitive
  (`enumerate_qualified_exports`) generalizing
  `collect_qualified_exports`'s own recursive descent, decoupled from
  `QualifiedExportEntry`'s own callback-wiring-specific shape.
- Preserve every existing `graph/graph-builder` and callback-wiring
  scenario's observable outcome exactly for the single-level case
  (`"<self>"`/direct child) - this is additive reach, not a behavior
  change for what already worked.
- Record, in this document, the accepted architectural trade-off
  (proposal.md's "Architectural note") as a named, intentional
  decision - not something a future reader has to rediscover by
  re-deriving it from the code.

**Non-Goals:**
- Changing what `component_spec_for`/`component_for` return
  (`ComponentSpec`/`OuterMetaComponent` pointers) or removing them -
  `component_for` in particular is still needed by
  `try_materialize_field_extension`'s own geom/vgrid resolution
  (component-wide accessors, not per-item `var_spec` fields - a
  different concern from this change, per `graph-builder-
  stateitem-resolution`'s own scope boundary). This change only adds
  a multi-level alternative where today only a single-level one
  exists; it does not retire the single-level helpers themselves,
  since `is_self`/direct-child dispatch remains the common case and
  stays cheap.
- Creating any new graph structure (proxy nodes, ports,
  resource-index entries) from a resolved path - pure traversal/query,
  per proposal.md "Explicitly out of scope."
- Rewiring `GraphBuilder_ConnectionResolution_smod.F90`'s own
  `var_spec` reads once a component is resolved - that is
  `graph-builder-stateitem-resolution`'s job; this change only fixes
  which component gets reached, not what is read off it.
- Building the nested-substate export-visibility capability itself
  (need (c) in proposal.md) - this change supplies the traversal
  primitive only.
- Caching or memoizing resolution results across calls - matches
  `resolve_descendant`/`collect_qualified_exports`'s own existing
  "fresh, uncached snapshot at resolution time" contract
  (callback-wiring design.md Decision 1), carried forward unchanged.

## Decisions

**`resolve_qualified_component` is `resolve_descendant`, relocated and
renamed, with no algorithm change.** `resolve_descendant`'s own body
(walk `/`-separated segments via `get_child_outer_meta`, one hop per
segment) is already correct and already tested indirectly through
callback-wiring's own suite. This change moves it from
`GraphBuilder_CallbackWiring_smod.F90`'s own private scope into
`GraphBuilder.F90`'s shared-helpers section (same tier as `component_for`/
`is_self`/`item_key` - every submodule already has host-association
access to that section) and renames it to reflect its now-general
purpose; `GraphBuilder_CallbackWiring_smod.F90` keeps a `resolve_descendant`
entry point that is now a one-line forward to the shared primitive, so
every existing internal call site in that submodule needs no further
change beyond the one definition.

**`enumerate_qualified_exports` generalizes `collect_qualified_exports`'s
own recursion, returning a plain `(qualified_path, NodeId, owner)`-
shaped result rather than `QualifiedExportEntry`
(`(qualified_name, comp_path, var_spec)`).** Alternative considered:
make `enumerate_qualified_exports` return `QualifiedExportEntry`
directly, so `build_qualified_export_namespace` becomes a trivial
wrapper. Rejected - `QualifiedExportEntry%var_spec` is exactly the
`var_spec`-carrying shape `graph-builder-stateitem-resolution` is
removing from resolution's own vocabulary; keeping it as this change's
own shared return type would re-introduce a `var_spec` dependency into
a brand-new primitive on the same day a sibling change is removing it
elsewhere. The new primitive returns an owner `OuterMetaComponent`
pointer and `NodeId` (via the owner's own `ComponentGraph` resource
index, once advertised) instead; `build_qualified_export_namespace`
re-wraps that into `QualifiedExportEntry` for its own existing callers
(construction cost difference is negligible - same tree walk either
way, one extra field-copy step).

**Ordinary connection resolution's `src_pt`/`dst_pt%component_name`
lookup switches from `component_spec_for` to
`resolve_qualified_component` + `get_component_spec()`/
`get_component_graph()` on the result**, not from
`component_spec_for` being deleted - `component_spec_for` itself is
unchanged (still used by `component_for`'s own `"<self>"` special
case and anywhere else in `GraphBuilder` that only ever needs a single
hop). `check_match_connection_unsatisfied`/`resolve_match_connection`
each gain one call to `resolve_qualified_component(this, comp_name,
_RC)` in place of `component_spec_for(this, comp_name, _RC)`, then
read `%get_component_spec()` off the resolved `OuterMetaComponent` to
get the same `ComponentSpec` pointer `component_spec_for` used to
return directly. A single-segment path (`comp_name` with no `/`)
behaves identically to today's `component_for`/`is_self` dispatch -
`resolve_qualified_component`'s own first-segment handling already
needs to special-case "the path's first segment is this component's
own name or `<self>`" the same way `is_self` does today (confirmed by
reading `resolve_descendant`'s own existing first-segment handling,
which already calls `get_child_outer_meta` unconditionally rather than
checking `is_self` first - this is a real, if minor, behavioral gap
versus `component_for`'s own `is_self` short-circuit, worth closing in
this same change rather than carrying it forward silently: a
single-segment path equal to `this%get_name()` must resolve to `this`
itself, not attempt a child lookup for a child literally named that).

**Qualified-path syntax matches callback-wiring's own existing
convention exactly: `/`-separated, no leading/trailing slash, empty
path means "root itself."** No new syntax is introduced; this change
reuses `resolve_descendant`'s own existing parsing (`index(remaining,
'/')`, segment-by-segment) verbatim, relocated.

## Risks / Trade-offs

- **[Risk, architectural, accepted per proposal.md's "Architectural
  note"] A qualified-path lookup through this primitive does not
  create any real structure at an intermediate subtree's own
  `ComponentGraph` boundary - "subtree X exposes item Y" is true only
  in the sense of "findable by walking down from X," not "visible as
  real port/resource-index structure on X's own graph."** This is a
  materially weaker property than `StateRegistry`'s own eager,
  structural export-bundling (`propagate_exports_all`) provides today,
  and a departure from the "a subtree acts as one component,
  inspectably, at its own boundary" goal that motivated the original
  per-`ComponentGraph`-per-`OuterComponent` decision (REQ-HIER-004).
  → **Mitigation:** accepted explicitly, not silently - recorded here
  and in proposal.md as a named trade-off. Revisit if a concrete
  future consumer (visualization/export tooling wanting a subtree's
  full exposed surface as real, inspectable structure; a correctness
  need that genuinely requires eager materialization, not just lookup)
  demonstrates the weaker property is insufficient. Until then, this
  shape is accepted as adequate for ordinary connection resolution and
  callback-wiring, both of which only ever need "can I find this
  item," never "can I enumerate everything a subtree exposes without
  being told what to look for."
- **[Risk] `resolve_qualified_component`'s first-segment handling must
  special-case `this%get_name()`/`"<self>"` the same way `is_self`
  does, or a single-segment path naming the calling component itself
  (not a child) will incorrectly attempt a child lookup** →
  **Mitigation:** called out explicitly above (Decisions) as a real
  gap in `resolve_descendant`'s own existing behavior, to be closed in
  this change rather than carried forward; task list includes a
  regression test for exactly this case (a single-segment path equal
  to the calling component's own name).
- **[Risk] Generalizing `collect_qualified_exports`'s own recursion
  into a `var_spec`-free return shape, in the same change that keeps
  `QualifiedExportEntry` itself unchanged (still carrying `var_spec`),
  could leave two parallel representations of "what does this
  subtree export" with no single source of truth** → **Mitigation:**
  accepted as a transitional state - `graph-builder-stateitem-
  resolution`'s own eventual migration of `GraphBuilder_CallbackWiring_smod.F90`
  itself is explicitly out of scope for both changes (each change's own
  proposal.md says so); `QualifiedExportEntry` remains callback-wiring's
  own type until a future change decides to migrate it, which this
  change does not attempt to force.
