## Context

See `proposal.md` - Why. This design superseded three earlier approaches
explored for this same change; recording why, since the reasoning is
load-bearing for why the final design looks the way it does:

1. **Derive-from-inputs entirely within `create()`.** `create()` runs
   immediately, per-item, as each variable is advertised
   (`OuterMetaComponent::advertise_variable`) - not later, tree-wide. A
   registry lookup of an expression's referenced variables inside `create()`
   only succeeds if those variables were declared earlier in the same
   component's `states:` section (`self_advertise` just walks `var_specs` in
   declaration order). `expression_defer_geom/A.yaml` deliberately declares
   `expr` *before* the variables it references, precisely to test that
   ordering is not required for geometry - and it broke this approach
   immediately (`Virtual connection point does not exist in registry`).
2. **Reorder `var_specs` (or drive off a generic `dependencies` field) so
   inputs always precede the expression, then still derive-and-validate in
   `create()`.** This fixes the *registration*-ordering hazard, but not a
   deeper one: registration is not the same as resolution. `AspectStatus::is_resolved()`
   is false for `FROM_COMP`/`MIRRORED`, precisely because component-level
   defaults (including `GEOMETRY_FROM_CHILD`-style deferred geometry) can
   legitimately only become concrete well after a state item is registered.
   `create()` is a one-shot call with no retry mechanism, so there is no
   general way to wait for an operand to actually resolve from inside it.
3. **Split `VerticalGridAspect` into independent stagger and grid-resource
   aspects** (its own change, `split-vertical-stagger-aspect`, since abandoned)
   to fix the root conflation properly and reuse the existing mirror mechanism
   generically for every item type. Abandoned because `matches`/
   `needs_extension_for` (and `update_from_payload`) on the base
   `StateItemAspect` type do not receive `other_aspects` - only
   `make_transform` does - so the two new aspects could not consult each other
   without widening those signatures on every `StateItemAspect` subclass in
   the codebase, a far larger change than justified here. A narrower,
   `VerticalGridAspect`-local patch (forcing `ASPECT_STATUS_MIRRORED`
   whenever stagger is invalid, regardless of what the geom/grid-resource
   fallbacks computed) was also considered and rejected: it applies to every
   state item via the shared `VariableSpec::make_VerticalGridAspect`
   constructor (out of scope - "scope to expression only" was decided early in
   this change's exploration), and it actively discards real, valid vertical
   grid resource data via `VerticalGridAspect::update_payload`'s
   `is_mirror()`-gated `MirrorVerticalGrid` substitution - a real regression
   for any ordinary field that omits `vertical_dim_spec` while a real
   component-level grid is available.

A fourth approach - the one actually first implemented and merged - was also
subsequently found to be unsound and replaced:

4. **Force `ASPECT_STATUS_MIRRORED` in `create()`, let the existing
   mirror/`ExtendTransform` machinery resolve it from a connection.** This
   passed every test written for it (including the full `Test_Scenarios`
   suite) and was merged. It relies entirely on `is_mirror()` accurately
   reflecting "not yet declared" from the moment `create()` runs until the
   item is actually connected in `make_transform`. That assumption does not
   hold: `StateItemSpec::set_geometry` (invoked, for every item in a
   component's registry, whenever that component calls
   `MAPL_GridCompSetVerticalGrid` to declare its own default vertical grid -
   see `target_set_geom` in `StateItemSpec.F90`) unconditionally calls
   `VerticalGridAspect::set_vertical_grid` on *every* item that already has a
   `VERTICAL_GRID_ASPECT_ID` entry, with no check of - or exemption for - an
   item deliberately left mirrored. `set_vertical_grid` sets
   `characteristic_state` to `ASPECT_STATUS_SPECIFIED` and installs the
   component's real grid, but never touches `vertical_stagger` at all. The
   result: an expression's forced-mirror status is silently overwritten to
   "resolved" the moment its owning component declares any vertical grid
   resource of its own - typically well before the expression is ever
   connected - while its `vertical_stagger` is left behind at
   `VERTICAL_STAGGER_INVALID`. `make_transform` then sees `is_mirror() ==
   .false.`, skips the `ExtendTransform` escape hatch, and falls into the same
   real-regrid-transform-building crash this whole change exists to fix -
   just later, and only once a real component-level vertical grid happens to
   be present (which is why this was not caught by the original test suite:
   none of `superstructure/generic/tests/scenarios/expression/*.yaml`'s
   gridcomps had declared their own vertical grid resource independent of
   what the connection provides). This was discovered via CI failures on
   `MAPL3G_Comp_Test_case21` (ExtData's "Derived" export feature, which builds
   an `expression` item whose owning component - ExtData - does declare its
   own vertical grid resource) after the original fix had already merged.

The design that survived this process is deliberately narrower than the
original ask, on purpose, and no longer relies on `is_mirror()`/
`characteristic_state` for its core signal at all:

- `GeomAspect::make_transform` and `VerticalGridAspect::make_transform`
  (`specs/GeomAspect/make_transform.F90:31`,
  `specs/VerticalGridAspect/make_transform.F90:34`) both already have an
  identical `if (src%is_mirror()) then allocate(transform,
  source=ExtendTransform()); return; end if` escape hatch, and this remains
  correct and untouched for `GeomAspect` and for any `expression` item that
  genuinely is still mirror at connection time. But approach 4 above showed
  this escape hatch cannot be relied upon as the *only* path to resolution for
  `expression` items specifically, because nothing prevents a component-level
  vertical grid resource from silently flipping `is_mirror()` to `.false.`
  first.
- What *does* survive every scenario examined: `vertical_stagger` itself is
  never touched by `set_geometry`'s override, so `VERTICAL_STAGGER_INVALID`
  (how an omitted `vertical_dim_spec` is parsed - see
  `ComponentSpecParser/parse_var_specs.F90`) remains a reliable "not
  explicitly declared" signal for items that do go through that parser.
  Combined with `is_mirror()` as a second, independent signal (needed because
  not every `ExpressionClassAspect` item is constructed via that parser at
  all - ExtData's "Derived" exports build their own `VariableSpec` without
  ever setting `vertical_stagger`, leaving it at
  `VerticalGridAspect`'s own `CENTER`-if-absent constructor default instead,
  which `is_mirror()` alone still correctly reports as unresolved since that
  item is never actually declared or connected until later), the two together
  identify "not actually declared" without depending on `characteristic_state`
  surviving untouched.
- `ExpressionClassAspect::make_transform` (for the `CLASS_ASPECT_ID` entry) is
  only reached after `GEOM`/`VERTICAL_GRID`/`TYPEKIND` have already been
  resolved to match the connection target - that is how
  `StateItemSpec::make_extension`'s aspect-order loop works. This is exactly
  why approach 4's `create()`-time forcing could not, by itself, be sufficient
  even if it had survived unmolested: by the time `make_transform` runs, any
  `VERTICAL_GRID_ASPECT_ID` hop for *this same connection attempt* has already
  been resolved (or has already crashed) using whatever state the aspect was
  in at the start of that attempt. The fix therefore resolves the value
  directly, in place, rather than only flipping a status flag and hoping a
  later, generic mechanism picks it up in time.

## Goals / Non-Goals

**Goals:**
- Let an `expression` item that references at least one variable omit
  `vertical_dim_spec`, resolving it directly from whichever of its referenced
  variables are already resolved at the time - both as early as possible
  (`create()`) and, more reliably, at actual connection time
  (`make_transform`).
- Catch a genuine inconsistency between an expression's referenced inputs (or
  between an input and the expression's own resolved value) with a clear
  error, whenever that inconsistency is actually knowable at the point it is
  checked.
- Keep the entire fix local to `ExpressionClassAspect.F90`.

**Non-Goals:**
- Guaranteeing the consistency check catches every possible mismatch. An
  operand that is not yet resolved (or not yet even registered) when a check
  runs is skipped, not treated as an error (see design Decisions for why this
  is an acceptable, usually-complete approximation, not a guarantee).
- Fixing the general `VariableSpec`/`ComponentSpecParser` defect that causes a
  plain `FieldClassAspect` item's vertical stagger to become `INVALID` (rather
  than a sane default or a genuinely mirrored state) when `vertical_dim_spec`
  is omitted. Explicitly out of scope (see proposal.md and Context above).
- Fixing `StateItemSpec::set_geometry`/`target_set_geom`'s unconditional
  overwrite of every item's `VerticalGridAspect` in a component's registry.
  That is a generic, registry-wide mechanism used by every state item type,
  not specific to `expression` items; changing it is out of scope for a change
  scoped to `ExpressionClassAspect.F90` alone. This design instead works
  around its effects, as described in Context above.
- Inferring a vertical dim spec for a constant/literal expression (no
  referenced variables) from anything at all. Confirmed with the user:
  expressions are also used to initialize 3D fields with a constant formula,
  so there is no safe default, and there is nothing to resolve from. Left as
  a documented non-goal; existing behavior for this case is unchanged.
- Any change to `VerticalGridAspect`, `VariableSpec`, `ComponentSpecParser`, or
  any other `ClassAspect` subclass.

## Decisions

### Decision: Resolve directly, in `check_vertical_stagger_consistency`, called from both `create()` and `make_transform`
`ExpressionClassAspect::create` no longer touches `characteristic_state` at
all (see Context, approach 4, for why that was unsound). Instead, a single
subroutine, `check_vertical_stagger_consistency(src, other_aspects,
expression_variables, rc)`, is called from both `create()` (best-effort, as
early as possible) and `make_transform` (authoritative, at actual connection
time, after any component-level override has already happened). It:

1. Reads the expression's own vertical stagger. Treats it as "not actually
   declared" - a candidate for resolution, not an authoritative value to
   validate others against - if it is `VERTICAL_STAGGER_INVALID` **or**
   `is_mirror()` is true (either signal alone is insufficient; see Context).
2. For each variable the expression's formula references
   (`parser_variables_in_expression`), looks up its primary spec in the
   registry. A variable not yet present in the registry at all, or present
   but still genuinely mirror (`is_mirror()`), is skipped - best effort, not
   an error.
3. Among the expression's own value (if declared) and whichever referenced
   variables are resolved at this point, checks that they all agree;
   `_FAIL`s with a message naming the conflicting items on any disagreement.
4. If the expression's own value was not actually declared (step 1) and a
   consistent value was found among resolved variables (step 3), adopts that
   stagger - and, if it requires one, that vertical grid - directly onto the
   expression's own `VerticalGridAspect` entry (via
   `other_aspects%at(VERTICAL_GRID_ASPECT_ID)`, the same pointer-mutation
   idiom used elsewhere in the codebase, e.g. `StateItemSpec::set_aspect`).

Because `other_aspects` in `create()` is the item's own, real aspect map (not
a copy), and the corresponding argument in `make_transform` is the map used to
build the connection's `goal_spec` immediately afterward, this mutation is
visible exactly where it needs to be in both call sites, without needing
`other_aspects` to be `intent(inout)`.

### Decision: `create()`'s attempt is unconditionally best-effort; `make_transform`'s is authoritative
`create()` runs per-item, in declaration order, as each variable is advertised
- referenced variables declared later in the same component (or in a
different component entirely) are not yet registered when an expression's own
`create()` runs, and step 2 above simply skips them (not an error). This means
`create()`'s attempt often does nothing, and that is expected: `make_transform`
runs later, at actual connection time, once far more of the registry has had a
chance to settle, and is retried there with no assumption about what `create()`
already accomplished. `create()`'s attempt exists purely to resolve the common,
same-component, already-declared-earlier case as early as possible; it is not
relied upon for correctness.

### Decision: No new `AspectId`, no interface changes, no other files touched
Everything above is achievable using only the aspect-map pointer-mutation
idiom already established in this codebase (`other_aspects%at(id)` returning a
pointer whose target can be mutated regardless of the map argument's own
`intent`) and `VerticalGridAspect`'s existing public `set_vertical_stagger`/
`set_vertical_grid`/`get_vertical_grid`/`get_vertical_stagger`/`is_mirror`
accessors. No change to `ClassAspect`, `StateItemAspect`, `VerticalGridAspect`,
`VariableSpec`, or any other `ClassAspect` subclass's `get_aspect_order` is
needed.

## Risks / Trade-offs

- [Risk] The consistency check can miss a genuine mismatch if the disagreeing
  operand is not yet resolved (or not yet registered) whenever a check runs →
  Mitigation: accepted, per Non-Goals; the common case (operands are exports
  that resolve independently of connections, generally well before an
  expression referencing them is itself connected) makes this rare in
  practice, and this is strictly better than before this change (no check at
  all, and a confusing `BasicVerticalGrid` crash instead of a clean error in
  the cases that are caught).
- [Risk] The "not actually declared" signal (`VERTICAL_STAGGER_INVALID .or.
  is_mirror()`) is a specific, somewhat incidental combination of two
  independent lower-level defects (the general `VariableSpec`/
  `ComponentSpecParser` "omitted becomes INVALID, not a real sentinel" defect,
  and `set_geometry`'s unconditional per-registry override) → Mitigation:
  this coupling is explicit, commented at both use sites, and localized to
  one boolean expression; noted here for future maintainers. If either
  underlying defect is ever fixed, this expression should be revisited.
- [Trade-off] Resolution now happens twice (`create()` and `make_transform`)
  rather than once, and the value is set directly rather than being an
  emergent property of the generic mirror/`ExtendTransform` machinery. This
  is less elegant than approach 4, but approach 4 was not actually correct
  (see Context) - this trade-off was accepted once that became clear.

## Migration Plan

No migration needed. Strictly additive: any `expression` item that previously
required `vertical_dim_spec` and had it declared correctly continues to work
unchanged (the declared value is never overwritten - resolution only ever
fires when the item's own value was not actually declared). The only new
failures are for configurations that were already inconsistent (mismatched,
already-resolved operands) and would otherwise have either failed later with a
much less clear error, or silently produced a wrong result.

Regression coverage: `superstructure/generic/tests/scenarios/expression/A.yaml`
has its `expr` item's `vertical_dim_spec` line removed
(`superstructure/generic/tests/Test_Scenarios.pf` already runs this scenario
through both its `value` and `vertical_profile` checks, among others).
`expression_defer_geom` (which keeps `vertical_dim_spec` explicit on `expr`, so
the resolution branch never fires there) is confirmed unaffected.
`Test_ExpressionClassAspect.pf` unit-tests `create()`'s and
`check_vertical_stagger_consistency`'s behavior directly, including the
not-yet-registered-variable case that a naive `is_mirror()`-only or
registry-lookup-without-a-guard implementation would miss.
`MAPL3G_Comp_Test_case21` (ExtData's "Derived" export feature) is the
regression test that caught approach 4's failure and is confirmed passing
against the final design.
