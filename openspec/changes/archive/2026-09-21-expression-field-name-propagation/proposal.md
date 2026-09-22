## Why

A user-reported case (`~/Fortran/for_tclune`, `GCM1.yaml`'s `E_sum: {expression: E_1+E_2,
standard_name: "foo", long_name: "bar", ...}`) shows that an `expression:`-derived export
loses its declared `standard_name`/`long_name` entirely: `ncdump -h` on the History output
shows `E_sum:standard_name = "unknown"` / `E_sum:long_name = "unknown"` instead of `"foo"`/
`"bar"`, even though the computed data values are correct.

Root cause (see design.md for the full trace): an `expression:` item's `ClassAspect` is an
`ExpressionClassAspect`, not a `FieldClassAspect`, and `ExpressionClassAspect` has never
carried `standard_name`/`long_name` at all - `VariableSpec.F90`'s `make_ClassAspect` drops
them when constructing it. Worse, because `ExpressionClassAspect%matches` is hardcoded
`.false.`, any connection into an expression export (including the implicit
same-name `MatchConnection` History uses for `var_list: {source: ...}`) always goes through
`StateItemSpec%make_extension`, which unconditionally replaces the `ExpressionClassAspect`
with the *consumer's* own `FieldClassAspect` "goal" via `set_aspect(dst_aspect, ...)` -
discarding the exporter's declared metadata before the `connect_to_export`/predecessor-
propagation fix from `field-name-propagation` ever gets a chance to run. That prior change
was explicitly scoped to `FieldClassAspect`-to-`FieldClassAspect` connections and did not
cover this case.

## What Changes

- `ExpressionClassAspect` gains `standard_name`/`long_name` members, populated from the
  export item's own var spec (the same values a plain Field export would get), instead of
  discarding them.
- A new virtual hook is added on the common `StateItemAspect` base - a default no-op,
  following the existing `connect_to_import` pattern - that lets an aspect being replaced
  during `StateItemSpec%make_extension`'s `CLASS_ASPECT_ID` substitution hand its own
  descriptive metadata to its replacement when the replacement doesn't already have its
  own. `FieldClassAspect` overrides it (reusing the same "own value wins, else inherit"
  policy as the existing `connect_to_export` propagation); `ExpressionClassAspect` exposes
  its stored `standard_name`/`long_name` through the accessors this hook uses.
- `StateItemSpec%make_extension` invokes this hook generically (for every aspect type,
  not just `CLASS_ASPECT_ID`) right before the aspect substitution that currently discards
  the predecessor outright, so an expression export's declared name survives being
  superseded by a consumer-supplied goal aspect - whether the consumer is reached via an
  explicit `connections:` entry or an implicit same-name `MatchConnection` (e.g. History's
  `var_list: {source: ...}`).
- No change to data/expression evaluation itself, to `FieldBundleClassAspect`/
  `StateClassAspect`/etc. (still out of scope, as in the prior change), or to any public
  API signature.

## Capabilities

### New Capabilities
(none)

### Modified Capabilities
- `generic/field-name-propagation`: adds a requirement that an `expression:`-derived
  export's own declared `standard_name`/`long_name` survives being connected to (whether
  via an explicit `connections:` entry or an implicit same-name match), instead of being
  silently discarded in favor of the consumer's nameless goal aspect.

## Impact

- `superstructure/generic/specs/ExpressionClassAspect.F90`: constructor gains
  `standard_name`/`long_name`; new accessor overrides.
- `superstructure/generic/specs/VariableSpec.F90` (`make_ClassAspect`,
  `MAPL_STATEITEM_EXPRESSION` case): pass `this%standard_name`/`this%long_name` through.
- `superstructure/generic/specs/StateItemAspect.F90`: new default no-op virtual method
  (metadata hand-off hook) alongside the existing `connect_to_import` default.
- `superstructure/generic/specs/FieldClassAspect.F90`: override the new hook and expose
  `get_standard_name`/`get_long_name` accessors (reusing the existing `mirror_name`-style
  policy from `connect_to_export`).
- `superstructure/generic/specs/StateItemSpec.F90` (`make_extension`): invoke the new hook
  before the `CLASS_ASPECT_ID` (and other aspect) substitution.
- `superstructure/generic/tests/`: new scenario reproducing an `expression:` export
  consumed via an implicit same-name connection (mirroring the user's real-world case),
  plus updates to `Test_Scenarios.pf`/`scenarios/` as needed.
- **Scope addendum** (discovered verifying against the user's actual repro; see
  design.md's addendum and tasks.md section 5): `infrastructure/field/FieldSet.F90`
  (`field_set` now writes `standard_name`/`long_name` through the same alias-scoped
  path `FieldGet.F90` reads from, instead of an unaliased slot) and
  `gridcomps/history/HistoryCollectionGridComp_private.F90` (`create_alias_field`
  explicitly re-copies the names across its `ESMF_FieldCreate`-based field
  duplication, since that duplicate has its own alias id distinct from the source
  field's).
