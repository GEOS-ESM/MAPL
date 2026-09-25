## Why

`ExpressionClassAspect` state items (`expression: (A + B)/C`) currently require an
explicit `vertical_dim_spec`, even when the referenced variables (`A`, `B`, `C`)
already establish it, and even though the framework already has a generic
mechanism - symmetric with `GeomAspect` - for resolving an unspecified vertical
dimension from whatever a state item connects to (`VerticalGridAspect::make_transform`'s
`is_mirror()` → `ExtendTransform()` escape hatch). That mechanism is defeated for
`expression` items specifically: when `vertical_dim_spec` is omitted,
`VariableSpec::make_VerticalGridAspect` can mark the aspect resolved
(`SPECIFIED`/`FROM_COMP`) purely because a vertical grid *resource* was inherited
from the component, even though the field's vertical *stagger* itself
(`none`/`center`/`edge`/`mirror`) was never set - `is_mirror()` then reports
`.false.`, the mirror escape hatch is skipped, and `make_transform` falls into
real regrid-transform-building logic against a placeholder grid, failing with
"BasicVerticalGrid should have been connected to a different subclass before this
is called."

Requiring users to redundantly restate a value that's already fixed by an
expression's own inputs is unnecessary boilerplate and a source of copy/paste
drift when an input's dim spec changes but the expression's declared value isn't
updated to match.

## What Changes

- `ExpressionClassAspect::create` and `ExpressionClassAspect::make_transform`
  both call a single subroutine, `check_vertical_stagger_consistency`, which
  directly resolves the expression's own vertical stagger (and vertical grid,
  if the resolved stagger requires one) from whichever of its referenced
  variables are themselves already resolved - as soon as at least one is, and
  only when none of the currently-resolved ones disagree. This is attempted
  as early as possible (`create()`, best-effort - many referenced variables
  are not registered yet at this point and are simply skipped) and again,
  authoritatively, at the point the expression's arithmetic wiring is
  constructed (`make_transform`). This is a narrow, `ExpressionClassAspect`-
  local correction, not a change to the shared constructor used by every
  state item.
- The same subroutine performs a best-effort consistency check: among the
  expression's own value (once resolved or if explicitly declared) and
  whichever referenced variables are resolved at the time it runs, it fails
  with a clear error naming the conflicting items on any disagreement.
  Skipped, not treated as an error: a referenced variable that is not yet
  resolved, or not yet even registered.
- Expressions that reference no variables (e.g. a literal/constant expression
  used to initialize a field) are unaffected: there is nothing to resolve
  from, and a constant expression may need a real vertical structure of its
  own (e.g. to initialize a 3D field) rather than deferring to anything.
  Existing behavior (requiring an explicit `vertical_dim_spec`, subject to the
  pre-existing, out-of-scope general default defect) is unchanged for this
  case.
- No changes to `VariableSpec`, `ComponentSpecParser`, or any other `ClassAspect`
  subclass - the fix is fully contained within `ExpressionClassAspect.F90`.

**Superseded from the original proposal**: this no longer derives the
expression's own vertical dim spec purely by mirroring it through a
connection (an interim design that was implemented, merged, and found to be
unsound - see design.md Context, approach 4 - because a component-level
vertical grid resource silently overwrites the mirrored status before a
connection ever happens). It instead derives the value directly from
whichever referenced inputs are already resolved, which is closer to the
capability originally requested than either the mirroring interim design or
the earlier, abandoned derive-from-inputs-in-`create()`-only approaches (see
design.md Context for the full history and why each earlier approach was
rejected or superseded: registration-vs-resolution timing hazards, a
considered-and-rejected `VerticalGridAspect` restructuring, and the mirroring
design's unreliable `is_mirror()` signal).

## Capabilities

### New Capabilities
- `generic/expression-vertical-dim-resolution`: Defines how an `expression`
  state item's vertical dim spec is resolved via connection mirroring when
  unspecified, and how consistency with its referenced inputs is checked on a
  best-effort basis.

### Modified Capabilities
(none)

## Impact

- Affected code: `superstructure/generic/specs/ExpressionClassAspect.F90`
  (`create` and `make_transform` methods only).
- Affected tests: `superstructure/generic/tests/scenarios/expression/A.yaml`
  becomes a regression test for this behavior once its `expr` item's
  `vertical_dim_spec` line is removed
  (`superstructure/generic/tests/Test_Scenarios.pf` already exercises this
  scenario); a new scenario (or unit test) covers the mismatched-operands
  error path.
- No public API changes; no changes to YAML schema (the key simply becomes
  optional for expressions with at least one referenced variable).
- No impact on ordinary `FieldClassAspect`/other state item types, and no
  changes to `VerticalGridAspect`'s shared construction/matching logic.
