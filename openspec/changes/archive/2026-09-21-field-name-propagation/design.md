## Context

See `proposal.md - Why` for the observed failure. Two facts about the existing
coupling mechanism drive this design:

1. **Aliasing.** Every placement of a Field state item into an `ESMF_State` goes
   through `ESMF_NamedAlias(this%payload, name=inner_name, _RC)` in
   `FieldClassAspect%add_to_state` (`superstructure/generic/specs/FieldClassAspect.F90`).
   This is true for the "owning" placement and for every connected/re-exported
   placement - there is no code path that places a raw, non-aliased field into a
   state. Each such call yields a distinct alias id via `ESMF_NamedAliasGet`, but all
   aliases of the same field share one underlying `ESMF_Info` host.
2. **A working precedent already exists.** `restart_mode` has exactly this same
   "one value per connection endpoint, same shared field" requirement, and it is
   already solved: `FieldInfoSetInternal`/`FieldInfoGetInternal`
   (`infrastructure/field/FieldInfo.F90`) has a `restart_mode`-specific overload
   that namespaces the `ESMF_Info` key by `named_alias_id`
   (`INFO_INTERNAL_NAMESPACE // "/alias" // id_str`), and it is written from
   `add_to_state`, which runs once per placement. This design generalizes that exact
   pattern to `standard_name`/`long_name`, rather than inventing something new.

Tracing the connection path (`SimpleConnection.F90:connect_sibling` →
`StateItemSpec.F90:connect` → `FieldClassAspect.F90:connect_to_export`) confirms
that an Import's own `FieldClassAspect` Fortran object (with its own
`standard_name`/`long_name` scalar members) survives connection intact - only its
`payload` (the `ESMF_Field` handle) is replaced to alias the Export's field:

```fortran
subroutine connect_to_export(this, export, actual_pt, rc)   ! this = Import's own aspect
   export_ = to_FieldClassAspect(export, _RC)                ! export = predecessor's aspect
   call this%destroy(_RC)
   this%payload = export_%payload                            ! only the field handle is swapped
   call mirror(this%fill_value, export_%fill_value)           ! existing precedent for "child wins, else inherit"
```

So the Import-side value is never actually lost at the Fortran level today - it is
lost only because persistence into `ESMF_Info` (currently done once, unaliased, in
`allocate()`) never runs for the Import side. This significantly narrows the fix.

Also confirmed: in a multi-hop transform chain, `StateItemSpec%make_extension`
clones the predecessor's entire aspect map via `clone_base` before deciding which
individual aspects need replacing for compatibility
(`aspect_ids = this%get_aspect_order(goal_spec)` /
`FieldClassAspect%matches_a`, in `FieldClassAspect_smod.F90`, only compares dynamic
type, never `standard_name`). `CLASS_ASPECT_ID` therefore never triggers
`needs_extension_for => true` and is never replaced mid-chain - it is always
inherited by cloning. This means every intermediate hop already carries the
predecessor's name forward automatically once `connect_to_export`'s propagation
(this design) makes the *first* hop correct; no separate per-hop change is needed
in `make_extension`.

## Goals / Non-Goals

**Goals:**
- Persist `standard_name`/`long_name` independently per connection endpoint
  (per `ESMF_NamedAlias` id) instead of one shared, field-wide value.
- Make an endpoint that does not declare its own name inherit its predecessor's
  name (one-directional, downstream only), instead of resolving to `'unknown'`
  whenever a predecessor did declare one.
- Do this without changing the public signature of `MAPL_FieldGet`/`MAPL_FieldSet`
  or requiring changes at existing call sites.

**Non-Goals:**
- Extending `long_name`/`standard_name` per-alias behavior to
  `FieldBundleClassAspect`, `StateClassAspect`, `VectorClassAspect`,
  `BracketClassAspect`, or `VectorBracketClassAspect`. These already only forward
  `standard_name` (not `long_name`) into their respective aspects
  (`superstructure/generic/specs/VariableSpec.F90`, `make_ClassAspect`) and are
  left as follow-up work using the same pattern established here.
- Detecting or warning about conflicting `standard_name`/`long_name` declared on
  both sides of a connection. Unlike `fill_value` (which logs a mismatch via the
  existing `mirror` helper because a numeric disagreement is likely an authoring
  error), differing descriptive names on each side is the expected, common case
  (see the already-existing `names_1` scenario) and is not treated as an error.
- Changing how `units`, `typekind`, or grid/geom metadata is shared across a
  connection. Those are already unaliased-and-shared by design because a
  connection either requires them to match or inserts a transform - this design
  does not touch that logic.

## Decisions

### Decision: Per-alias namespacing in `ESMF_Info`, not a new sharing primitive

Two options were considered for making per-endpoint metadata possible on a shared
field (see prior discussion thread for full comparative analysis):

- **Chosen - per-alias `ESMF_Info` namespace ("Option 1").** Reuse
  `ESMF_NamedAliasGet(field, id=)` (already used for `restart_mode` and in
  `RestartHandler.F90`) to key a namespaced slot per placement. Small, localized
  change confined to `FieldClassAspect.F90` and `FieldInfo.F90`/`FieldGet.F90`.
- **Rejected - stop sharing the whole `ESMF_Field`, share only the data pointer
  ("Option 2").** Would require redesigning the coupling primitive used by every
  `ClassAspect` subtype (`FieldBundleClassAspect`, `BracketClassAspect`,
  `VectorBracketClassAspect`, `VectorClassAspect`, `StateClassAspect`,
  `ServiceClassAspect`) and `RestartHandler.F90`'s alias-based restart identity, all
  of which are built around `ESMF_NamedAlias`/`ESMF_NamedAliasGet`/
  `mapl_FieldsAreAliased`. Substantially larger blast radius for a problem Option 1
  already solves with a mechanism proven in production for `restart_mode`.

### Decision: Persist metadata in `add_to_state()`, not `allocate()`

`allocate()` runs once, only for the endpoint that owns memory allocation (in
practice, the original Export/primary). `add_to_state()` runs once per placement -
Export, Import, and every transform-chain hop - which is exactly the granularity
needed. Move the `mapl_FieldSet(..., standard_name=, long_name=, ...)` call out of
`allocate()` (keep `fill_value` there, unaffected) and into `add_to_state()`,
alongside the existing `restart_mode` block, guarded by
`allocated(this%standard_name) .or. allocated(this%long_name)`.

### Decision: Replace the `'unknown'` default with "unassigned" (`allocated()` as the presence test)

`FieldClassAspect`'s constructor currently forces `standard_name`/`long_name` to
the literal `'unknown'` when not supplied. This makes "the author wrote nothing"
indistinguishable from "the author wrote 'unknown'". Changing the default to leave
these components unallocated (matching `VariableSpec.F90`'s already-unforced
`character(:), allocatable :: standard_name`/`long_name`) makes `allocated(...)`
a reliable presence test for the inheritance logic. `'unknown'` remains only as
the final fallback used at read time (`FieldGet`) when nothing was ever assigned
anywhere in the chain. No test in the current suite asserts the literal default
value, so this is a safe, non-breaking internal change.

### Decision: Propagate in `connect_to_export`, one-directional, no `connect_to_import` change

`connect_to_export` is the single call site that fires for every connection,
whether or not a transform chain preceded it (`SimpleConnection.F90:connect_sibling`
always ends with `dst_spec%connect(new_spec, ...)`, where `new_spec` is whatever the
chain resolved to). Add a character-string sibling of the existing numeric `mirror`
helper there:

```fortran
! dst (this, the Import side) keeps its own value if assigned;
! otherwise inherits from src (export_, the predecessor). No mismatch logging.
if (.not. allocated(dst)) dst = src   ! only when src is allocated
```

applied to both `standard_name` and `long_name`. `connect_to_import` (which runs on
the Export's own aspect) is intentionally left unchanged - unlike `fill_value`,
names must not converge bidirectionally; the Export's own declared name must never
be overwritten by a downstream Import's declaration.

### Decision: Read path derives the alias id from the field handle, no API change

`FieldGet.F90`'s `field_get`/`FieldInfoGetInternal` will call
`ESMF_NamedAliasGet(field, id=alias_id, _RC)` internally and look up the per-alias
namespace, falling back to `'unknown'` if that specific alias never had a value set
(defensive default for hand-built fields in tests, or any path that bypasses
`add_to_state`). Because callers of `MAPL_FieldGet` always pass a field handle
retrieved from a specific `ESMF_State` (already the correct alias for "their" view),
this is transparent - no call site changes needed.

## Risks / Trade-offs

- **[Risk]** Downstream consumers (History output, restart/`SharedIO.F90`) may
  implicitly assume "a field has one true `standard_name`/`long_name` regardless of
  which state it was fetched from." → **Mitigation**: audit
  `MAPL_FieldGet(..., standard_name=` / `long_name=` call sites before merging
  (`infrastructure/geom_io/SharedIO.F90` is the one found so far); confirm each
  fetches the field from a specific state context it already controls, which is the
  correct alias to report for that context.
- **[Risk]** Changing the `FieldClassAspect` default from `'unknown'` to unassigned
  could change behavior for any code that reads `this%standard_name`/`long_name`
  directly (not through `MAPL_FieldGet`) and assumes it is always allocated.
  → **Mitigation**: those members are `private` to `FieldClassAspect`; grep confirms
  the only internal readers are the constructor and the (moved) persistence call,
  both updated by this change.
- **[Risk]** Multi-hop transform chains were reasoned about via code inspection
  (`clone_base`/`make_extension`/`matches_a`) but not yet exercised by a test with an
  actual coupler in the chain. → **Mitigation**: add a `names_1`-style scenario that
  forces a transform (e.g. unit conversion or regrid) between differently-named
  Export/Import endpoints, per `tasks.md`.

## Migration Plan

No data migration or deployment sequencing is needed (single-process, in-memory
metadata). Implementation order (see `tasks.md` for full detail):
1. Add per-alias `standard_name`/`long_name` helpers in `FieldInfo.F90`.
2. Move persistence from `allocate()` to `add_to_state()` in `FieldClassAspect.F90`.
3. Add predecessor-propagation in `connect_to_export`.
4. Update the read path (`FieldGet.F90`) to resolve per-alias with fallback.
5. Update `FieldClassAspect`'s constructor default.
6. Extend test coverage (existing `names_1` scenario + new inheritance/transform
   cases) and run the full `MAPL.generic.scenarios`/`MAPL.generic.core` suites.

Rollback is a plain revert; no persisted state or schema is affected.

## Open Questions

- Should the per-alias fallback in `FieldGet.F90` log/warn when it falls through to
  the legacy `'unknown'` default (to help discover truly-undeclared fields), or stay
  silent as today? Deferred - does not affect the spec, approach, or task breakdown;
  can be decided during implementation review.
