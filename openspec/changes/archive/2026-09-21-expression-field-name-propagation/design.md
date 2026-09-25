## Context

See `proposal.md - Why` for the reported symptom and root-cause trace. Two facts about
the existing aspect-substitution machinery shape this design:

1. **`ExpressionClassAspect%matches` is hardcoded `.false.`** (`ExpressionClassAspect.F90`,
   `matches` function - the real dynamic-type comparison is present but commented out).
   Combined with `needs_extension_for`'s non-overridable base implementation
   (`StateItemAspect.F90`, `.not. src%matches(dst)`), this means *any* connection into an
   expression export always takes the "needs extension" branch in
   `StateItemSpec%make_extension` (`StateItemSpec.F90:300-358`), never the "already
   compatible, reuse as-is" branch that a plain `FieldClassAspect`-to-`FieldClassAspect`
   connection can take.
2. **`make_extension`'s substitution is unconditional and one-directional**: once
   `needs_extension_for` is true for a given `aspect_id`, the code does
   `call new_spec%set_aspect(dst_aspect, _RC)` - replacing `new_spec`'s aspect at that id
   with the goal's (`dst_aspect`), full stop. For `CLASS_ASPECT_ID` this means whatever the
   predecessor's `ClassAspect` was (here, the `ExpressionClassAspect` holding `E_sum`'s own
   declared name) is discarded and replaced by the consumer-supplied goal `FieldClassAspect`
   - which, for an implicit `MatchConnection` (History's `var_list: {source: ...}`) or an
   `connections:`-declared import that doesn't redeclare metadata, has no name of its own.
   This happens *before* `connect_to_export`'s predecessor-propagation (added in
   `field-name-propagation`) ever runs, because by the time `connect_to_export` is invoked
   (from `SimpleConnection`/`MatchConnection`'s shared `connect_sibling`-style flow), both
   sides already reference the same substituted, nameless `FieldClassAspect`.

The prior `field-name-propagation` change fixed the case where a `FieldClassAspect` is
merely *aliased* (payload swapped, aspect object otherwise untouched) - that case never
hits `set_aspect` for `CLASS_ASPECT_ID` because `FieldClassAspect%matches` compares by
dynamic type and returns true for two `FieldClassAspect`s, so `needs_extension_for` is
false and the substitution branch is skipped entirely. Expression exports hit the
substitution branch unconditionally, which is the gap this change closes.

## Goals / Non-Goals

**Goals:**
- Let an `expression:`-derived export's own declared `standard_name`/`long_name` survive
  being superseded by a consumer-supplied `FieldClassAspect` during
  `StateItemSpec%make_extension`, whether the connection is explicit (`connections:`) or
  implicit (same-name `MatchConnection`, e.g. History's `var_list: {source: ...}`).
- Do this with a mechanism generic enough to also cover any *other* current or future
  `ClassAspect` subtype that gets wholesale-substituted rather than merely aliased,
  without hardcoding type-specific logic into the generic `make_extension` code.

**Non-Goals:**
- Changing `ExpressionClassAspect%matches`/`needs_extension_for` to make expression exports
  ever take the "no extension needed" path. That's a larger behavioral question (it would
  affect coupler insertion generally, not just naming) and is out of scope here.
- `FieldBundleClassAspect`, `StateClassAspect`, `VectorClassAspect`, `BracketClassAspect`,
  `VectorBracketClassAspect` - same exclusion as the prior change; still only
  `FieldClassAspect` and (now) `ExpressionClassAspect` are addressed.
- Re-deriving expression evaluation, coupler insertion, or the input-operand coercion logic
  in `ExpressionClassAspect%make_transform` (the `FieldClassAspect(standard_name='',
  long_name='')` at line ~285 there is for the input operands E_1/E_2's unit coercion, is
  unrelated to E_sum's own output metadata, and is left as-is).

## Decisions

### Decision: A new default-no-op virtual hook on `StateItemAspect`, not type-checking in `make_extension`

`StateItemSpec.F90` is generic, aspect-agnostic infrastructure - it already delegates
aspect-specific behavior via virtual methods (`matches`, `needs_extension_for`,
`connect_to_export`, `connect_to_import`, `make_transform`) rather than `select type`
switches. Follow that precedent: add a new non-deferred method to `StateItemAspect`
(alongside the existing default no-op `connect_to_import`), e.g.
`inherit_descriptive_metadata(predecessor, rc)`, with a default no-op body. Call it
unconditionally in `make_extension` for every substituted aspect_id, right before
`set_aspect`:
```fortran
call dst_aspect%inherit_descriptive_metadata(src_aspect, _RC)
call new_spec%set_aspect(dst_aspect, _RC)
```
Only `FieldClassAspect` overrides it (to do something); every other aspect type
(`GeomAspect`, `VerticalGridAspect`, `TypekindAspect`, `UnitsAspect`, `ExpressionClassAspect`
itself, etc.) keeps the inherited no-op, so this is a purely additive, safe-by-default hook.

Rejected alternative: special-case `CLASS_ASPECT_ID` with a `select type` in
`make_extension`. Works, but breaks the existing "aspects encapsulate their own behavior"
convention and would need updating every time a new `ClassAspect` subtype needs the same
treatment.

### Decision: Reuse `FieldClassAspect`'s existing `mirror_name` policy, not `connect_to_export`

`connect_to_export` was considered and rejected as the injection point for this hook,
because it also does `this%payload = export_%payload` (aliases the field). That's correct
for a real connection, but wrong here: `dst_aspect` in `make_extension` is a fresh
`FieldClassAspect` whose own payload is what the coupler/`EvalTransform` will actually
create and populate - replacing it with the predecessor's placeholder field would corrupt
the data path, not just the metadata. `inherit_descriptive_metadata` on `FieldClassAspect`
therefore only touches `standard_name`/`long_name`, reusing the same "own value wins, else
inherit from predecessor" logic already factored out as `mirror_name` in
`connect_to_export` - no new merge policy is introduced, just a second call site for the
existing one.

### Decision: `ExpressionClassAspect` gets `standard_name`/`long_name` members and read-only accessors, not a full `inherit_descriptive_metadata` override

`ExpressionClassAspect` is always the *predecessor* being substituted in this scenario, never
the thing being substituted into - so it only needs to expose what it has
(`get_standard_name`/`get_long_name` accessors, mirroring how `FieldClassAspect` will expose
its own for `mirror_name` to consult), not implement the inheritance/merge policy itself.
Populated the same way `FieldClassAspect` already is: `VariableSpec.F90`'s `make_ClassAspect`
passes `this%standard_name`/`this%long_name` into the constructor (currently dropped
entirely for the `MAPL_STATEITEM_EXPRESSION` case).

## Risks / Trade-offs

- **[Risk]** Adding a new virtual method to `StateItemAspect` touches a widely-implemented
  abstract base. → **Mitigation**: made it a concrete (non-deferred) method with a no-op
  default body, exactly like the existing `connect_to_import` - no existing subclass needs
  to change unless it wants to participate.
- **[Risk]** `make_extension`'s substitution loop (`StateItemSpec.F90`) runs for every
  `aspect_id` that needs extension, not just `CLASS_ASPECT_ID` - calling the new hook
  unconditionally means it also runs (as a no-op) for `GEOM_ASPECT_ID`,
  `VERTICAL_GRID_ASPECT_ID`, `TYPEKIND_ASPECT_ID`, etc. → **Mitigation**: intentional and
  harmless (default no-op costs nothing functionally); keeps the call site simple rather
  than adding an `if (aspect_id == CLASS_ASPECT_ID)` special case.
- **[Risk]** Only one reported case (History via implicit `MatchConnection`) has been
  verified; explicit `connections:` into an expression export haven't been exercised by an
  existing test. → **Mitigation**: `tasks.md` adds test scenarios for both the implicit
  (reproducing the user's case) and explicit paths.

## Migration Plan

No data migration. Implementation order (see `tasks.md`):
1. Add `standard_name`/`long_name` to `ExpressionClassAspect` + accessors; wire
   `VariableSpec.F90` to pass them through.
2. Add the `inherit_descriptive_metadata` default no-op to `StateItemAspect`; override in
   `FieldClassAspect` (reusing `mirror_name`'s policy) with matching
   `get_standard_name`/`get_long_name` accessors.
3. Call the hook from `StateItemSpec%make_extension` before `set_aspect`.
4. Add scenario coverage for both implicit (same-name/History-style) and explicit
   (`connections:`) expression-export connections; rerun `MAPL.generic.scenarios`/
   `MAPL.generic.core`; re-run the user's original repro
   (`~/Fortran/for_tclune/mapl1.yaml`) and confirm `ncdump -h` shows the declared
   `standard_name`/`long_name` instead of `"unknown"`.

Rollback is a plain revert; no persisted state or schema is affected.

## Addendum: scope expansion discovered during verification

Step 4 of the migration plan (re-running the user's actual repro) surfaced a second,
independent bug that fully masked whether steps 1-3 worked: `MAPL_FieldGet`'s
alias-scoped read (from `field-name-propagation`) has no symmetric counterpart in
`MAPL_FieldSet` - the setter still wrote `standard_name`/`long_name` to the single,
unaliased Info slot. This was invisible until now because the only production writer
was `FieldClassAspect%add_to_state`, which calls the alias-scoped setter directly
(bypassing the public `MAPL_FieldSet`). History's `HistoryCollectionGridComp_private.
F90:create_alias_field` duplicates a field via `ESMF_FieldCreate(...,
dataCopyFlag=ESMF_DATACOPY_REFERENCE)` (not `ESMF_NamedAlias`) and does a wholesale
Info-tree copy; the duplicate's own `ESMF_NamedAliasGet` id (`0`) differs from
whatever id the original field's name was written under, so the copied (but
differently-keyed) entry is never found on read, and the fallback `'unknown'` masked
tasks 1-3's fix end-to-end.

User-directed resolution (id `0` is a valid, ordinary scope like any other, not a
special case requiring different handling): made `FieldSet.F90`'s `field_set`
symmetric with `FieldGet.F90` (derive `alias_id` via `ESMF_NamedAliasGet` and write
through the alias-scoped setter), and had `create_alias_field` explicitly
`MAPL_FieldGet`/`MAPL_FieldSet` the names across the duplication, re-scoping them to
the new field's own id. This closes the gap for any current or future caller of the
public `MAPL_FieldSet`/`MAPL_FieldGet` API, not just History. See `tasks.md` section 5.
