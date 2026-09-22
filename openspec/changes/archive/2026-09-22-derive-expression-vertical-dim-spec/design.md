## Context

See `proposal.md` - Why. This design superseded two earlier approaches explored
for this same change; recording why, since the reasoning is load-bearing for
why the final design looks the way it does:

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

The design that survived this process is deliberately narrower than the
original ask, on purpose:

- `GeomAspect::make_transform` and `VerticalGridAspect::make_transform`
  (`specs/GeomAspect/make_transform.F90:31`,
  `specs/VerticalGridAspect/make_transform.F90:34`) both already have an
  identical `if (src%is_mirror()) then allocate(transform,
  source=ExtendTransform()); return; end if` escape hatch. This already works
  correctly for `GeomAspect` today. The only reason it doesn't work for
  `expression` items is that their `VerticalGridAspect` never actually reaches
  a genuine `ASPECT_STATUS_MIRRORED` state when `vertical_dim_spec` is
  omitted (see proposal.md - Why). Forcing that one item's own aspect into a
  genuinely mirrored state, scoped inside `ExpressionClassAspect::create`
  only, lets this already-correct, already-symmetric machinery do the actual
  work - no new derivation logic, no ordering dependency, no interface
  changes anywhere.
- `ExpressionClassAspect::make_transform` (for the `CLASS_ASPECT_ID` entry) is
  only reached after `GEOM`/`VERTICAL_GRID`/`TYPEKIND` have already been
  resolved to match the connection target - that is how
  `StateItemSpec::make_extension`'s aspect-order loop works (it resolves one
  aspect per hop, in the fixed order `ExpressionClassAspect::get_aspect_order`
  returns, and only reaches the last entry once nothing earlier still
  `needs_extension_for`). So by the time `make_transform` runs, the
  expression's own vertical dim spec is *already* concretely resolved -
  either mirrored from the connection or, if explicitly declared, unchanged.
  `make_transform` also already performs registry lookups of every referenced
  variable (`this%registry%get_primary_spec`) to wire up the arithmetic
  couplers - adding a consistency check alongside that existing lookup is not
  a new access pattern.

## Goals / Non-Goals

**Goals:**
- Let an `expression` item that references at least one variable omit
  `vertical_dim_spec`, resolving it via the same connection-mirroring
  mechanism `GeomAspect` already uses.
- Catch a genuine inconsistency between an expression's referenced inputs (or
  between an input and the expression's own resolved value) with a clear
  error, whenever that inconsistency is actually knowable at the point the
  expression's arithmetic wiring is constructed.
- Keep the entire fix local to `ExpressionClassAspect.F90`.

**Non-Goals:**
- Deriving the expression's own vertical dim spec *value* from its inputs.
  The value comes from whatever it connects to (mirroring); the inputs are
  only consulted afterward, for validation.
- Guaranteeing the consistency check catches every possible mismatch. An
  operand that is not yet resolved at `make_transform` time is skipped, not
  treated as an error (see design Decisions for why this is an acceptable,
  usually-complete approximation, not a guarantee).
- Fixing the general `VariableSpec`/`ComponentSpecParser` defect that causes a
  plain `FieldClassAspect` item's vertical stagger to become `INVALID` (rather
  than a sane default or a genuinely mirrored state) when `vertical_dim_spec`
  is omitted. Explicitly out of scope (see proposal.md and Context above).
- Inferring a vertical dim spec for a constant/literal expression (no
  referenced variables) from anything at all. Confirmed with the user:
  expressions are also used to initialize 3D fields with a constant formula,
  so there is no safe default, and there is nothing to mirror from without a
  connection. Left as a documented non-goal; existing behavior for this case
  is unchanged.
- Any change to `VerticalGridAspect`, `VariableSpec`, `ComponentSpecParser`, or
  any other `ClassAspect` subclass.

## Decisions

### Decision: Force `ASPECT_STATUS_MIRRORED` in `create()`, scoped to items with ≥1 referenced variable
`ExpressionClassAspect::create` computes
`n_vars = parser_variables_in_expression(this%expression)%size()` (the same
call already used elsewhere in this file). If `n_vars > 0` and the item's own
vertical stagger is `VERTICAL_STAGGER_INVALID` (the sentinel parsed when
`vertical_dim_spec` is omitted), it retrieves a pointer to its own
`VerticalGridAspect` entry (via `other_aspects%at(VERTICAL_GRID_ASPECT_ID)`,
the same pointer-mutation idiom used elsewhere in the codebase - e.g.
`StateItemSpec::set_aspect` - which does not require `other_aspects` to be
`intent(inout)`, since only the pointed-to object's fields are mutated, not
the map's own structure) and calls `set_characteristic_state(ASPECT_STATUS_MIRRORED)`
on it.

The `n_vars > 0` guard is deliberate and load-bearing: a constant expression
(no referenced variables) has nothing to mirror from, and per the Non-Goals
above, must not be forced into a mirrored state that could prevent it from
ever acquiring a real vertical grid (e.g. if it is meant to initialize a 3D
field and is never itself the source of a connection).

### Decision: Best-effort consistency check inside `make_transform`, not a hard requirement
For each variable `parser_variables_in_expression` returns, look up its
primary spec's `VerticalGridAspect`. If `is_mirror()` is true for that
variable at this point, skip it - its value is not yet knowable, and `create()`
has no retry mechanism to wait for it (this is the same fundamental
limitation that ruled out deriving the expression's own value from inputs
directly). Among variables whose stagger *is* resolved at this point, compare
them to each other and to the expression's own (by-construction-of-aspect-order,
already-resolved) stagger; `_FAIL` with a message naming the conflicting items
on any disagreement.

In practice this is rarely a weak check: referenced variables are always
exports (`ExpressionClassAspect` only ever looks up
`VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, ...)` in its own component's
registry), and exports normally resolve their own vertical dim spec (item-level
or component-level) independent of any connection - they do not typically wait
to be mirrored themselves. The "skip if unresolved" branch is a safety valve
for a genuine edge case, not the common path.

### Decision: No new `AspectId`, no interface changes, no other files touched
Everything above is achievable using only the aspect-map pointer-mutation
idiom already established in this codebase (`other_aspects%at(id)` returning a
pointer whose target can be mutated regardless of the map argument's own
`intent`) and the existing `is_mirror()`/`ExtendTransform` mechanism. No
change to `ClassAspect`, `StateItemAspect`, `VerticalGridAspect`,
`VariableSpec`, or any other `ClassAspect` subclass's `get_aspect_order` is
needed.

## Risks / Trade-offs

- [Risk] The consistency check can miss a genuine mismatch if the disagreeing
  operand is not yet resolved at `make_transform` time → Mitigation: accepted,
  per Non-Goals; the common case (operands are exports that resolve
  independently of connections) makes this rare in practice, and this is
  strictly better than today (no check at all, and a confusing
  `BasicVerticalGrid` crash instead of a clean error in the cases that are
  caught).
- [Risk] `create()`'s forced-mirror only fires when the item's own stagger is
  exactly `VERTICAL_STAGGER_INVALID` - if the general (out-of-scope)
  `VariableSpec` defect is ever fixed to produce some other "not specified"
  representation, this check would need to be revisited → Mitigation: this
  coupling is explicit and localized to one `if` condition; noted here for
  future maintainers.
- [Trade-off] This does not give expressions the "compute a dimension purely
  from operands, independent of any connection" semantics originally
  requested - only "mirror from connection, with a best-effort operand
  sanity-check." This was a deliberate, discussed trade-off (see Context)
  after the derive-from-inputs approaches proved to have real correctness
  gaps of their own (registration-vs-resolution timing) that this simpler
  design does not share.

## Migration Plan

No migration needed. Strictly additive: any `expression` item that previously
required `vertical_dim_spec` and had it declared correctly continues to work
unchanged (`create()`'s forced-mirror branch only fires when the stagger is
invalid, i.e. the key was omitted). The only new failures are for
configurations that were already inconsistent (mismatched, already-resolved
operands) and would otherwise have either failed later with a much less clear
error, or silently produced a wrong result.

Regression coverage: remove the `vertical_dim_spec` line from the `expr` item
in `superstructure/generic/tests/scenarios/expression/A.yaml`
(`superstructure/generic/tests/Test_Scenarios.pf` already runs this scenario
through both its `value` and `vertical_profile` checks). Confirm
`expression_defer_geom` (which keeps `vertical_dim_spec` explicit on `expr`,
so the new `create()` branch never fires there) is unaffected.
