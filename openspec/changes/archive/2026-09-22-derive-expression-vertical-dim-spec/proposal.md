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

- `ExpressionClassAspect::create` forces its own item's vertical dim spec to a
  genuinely unresolved (`ASPECT_STATUS_MIRRORED`) state when `vertical_dim_spec`
  was omitted **and** the expression references at least one variable - instead
  of leaving whatever inconsistent status `VariableSpec::make_VerticalGridAspect`
  happened to compute. This is a narrow, `ExpressionClassAspect`-local
  correction, not a change to the shared constructor used by every state item.
- With the item genuinely mirrored, the framework's existing, already-symmetric
  mirror/`ExtendTransform` mechanism resolves the expression's vertical dim spec
  from whatever it connects to - no new derivation logic is needed for this
  part; it reuses machinery `GeomAspect` already relies on today.
- `ExpressionClassAspect::make_transform` gains a best-effort validation pass:
  when wiring the arithmetic couplers (at which point its own vertical dim spec
  is already resolved, by construction of the aspect-resolution order), it
  checks whichever referenced variables already have a resolved vertical
  stagger at that point (skipping any that are still genuinely unresolved -
  best effort, not a hard requirement) and fails with a clear error naming the
  conflicting items if they disagree with each other or with the expression's
  own resolved value.
- Expressions that reference no variables (e.g. a literal/constant expression
  used to initialize a field) are unaffected by the `create()` change: nothing
  is forced to mirror in that case, since there is nothing to mirror from and a
  constant expression may need a real vertical structure of its own (e.g. to
  initialize a 3D field) rather than deferring to a connection. Existing
  behavior (requiring an explicit `vertical_dim_spec`, subject to the
  pre-existing, out-of-scope general default defect) is unchanged for this
  case.
- No changes to `VariableSpec`, `ComponentSpecParser`, or any other `ClassAspect`
  subclass - the fix is fully contained within `ExpressionClassAspect.F90`.

**Superseded from the original proposal**: this no longer derives the
expression's own vertical dim spec *from* its inputs (the earlier design's
central mechanism). It resolves the expression's own value via connection
mirroring instead, and only uses the inputs for a best-effort consistency check
afterward. See design.md Context for why the derive-from-inputs approach was
abandoned (registration-vs-resolution timing hazards, and a considered-and-rejected
`VerticalGridAspect` restructuring).

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
