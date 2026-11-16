# Zero-DE ("no local decomposition element") Fix — Status: COMPLETE

**Branch:** `feature/bmauer/fix_zero_de_ll`
**Context:** Supporting MPI ranks that own zero grid cells of a coarse/low-res
grid ("extra" ranks with `localDECount == 0`), across ExtData reading,
GridPFIO read/write, coordinate writing, and the shared low-level field
pointer/copy utilities used throughout MAPL.

This file is a scratch/working note kept up to date across sessions. All
work described here has been implemented, built, and tested. Commits are
handled by you; this file does not track commit boundaries.

See `CHANGELOG.md`'s `[Unreleased]` section for the user-facing summary of
everything below.

## Final status

All planned work is done and verified:
- Read path (ExtData) — done, verified.
- Write path + GridPFIO read twin (History/Restart) — done, verified.
- Centralized `get_cptr` fix (protects `FieldBLAS`, `FieldUtilities`,
  `FieldCondensedArray`, `FieldApplyUserRoutine`, and the generic3g
  `ConvertUnitsTransform`/`NormalizationTransform`/`TimeInterpolateTransform`
  transparently) — done, verified.
- NAG disassociated-pointer follow-up fix (found via new tests) — done,
  verified.
- Tier A/B regression tests — done, all passing.
- Full `ctest` suite run to completion: 76 tests, 69 pass; the 7 failures
  are pre-existing/environmental (unrelated `LOCAL_REGRESSION_DATA_DIR`/
  `pyyaml` gaps, present before this work started) — no regressions from
  anything in this session.

## Fixes, in the order they were found

### 1. `infrastructure/geom_io/pFIOServerBounds.F90`
Added optional `has_de` (default `.true.`) threaded through
`pFIOServerBounds` / `pFIOServerBounds_vert_only_field` /
`pFIOServerBounds_gridded_field`.

**Key correctness insight (found via a real crash, not guessed):**
`global_start`/`global_count` (and `corner_global_*`) describe the **true,
full file-variable shape** and **must be identical on every rank**
collaborating on a given collective request — pfio's server
(`ServerThread.F90` `read_and_share`/`read_and_gather`) only inspects **one
arbitrary representative message per `request_id`** to size and populate
its shared read buffer. Zeroing these for a no-DE rank crashed with:
```
ServerThread.F90:462: Subscript 1 of I_PTR (value 1) is out of range (1:0)
```
Fix: only the **local** shape (`file_shape`/`new_element_count`, i.e. the
`ArrayReference`'s shape — this rank's own contribution) is zero for a
no-DE rank, which happens automatically since `field_shape` (=
`element_count`, from the DE-safe `MAPL_FieldGetLocalElementCount`
wrapper) is already all-zero. Only the genuinely DE-dependent
`MAPL_GridGet(grid, interior=...)` call is guarded (unsafe: leaves
`interior` unallocated when `local_de_count==0`, per
`infrastructure/geom/GridGet/grid_get_interior.F90:37`); placeholder
`i1=in=j1=jn=1` used instead when `has_de=.false.` (harmless since count=0
there). `mapl_GridGetGlobalCellCountPerDim` / `ESMF_GridGet(tileCount=)`
confirmed safe regardless of DE (pure grid-topology metadata).

**Known, documented, unaddressed limitation:** the vert-only path
(`pFIOServerBounds_vert_only_field`, triggered when `size(field_shape)==1`)
does NOT preserve this invariant — `global_start`/`global_count` DO zero
out when `has_de=.false.`, because there's no independent source for the
true level count once the caller's `field_shape` is already zeroed. Believed
benign since vert-only (non-horizontally-decomposed) fields aren't expected
to legitimately have `has_de=.false.` in practice, but flagged via a test +
comment in `Test_pFIOServerBounds.pf::test_vert_only_no_de` rather than
silently accepted. Revisit if a real vert-only no-DE crash ever surfaces.

### 2. `gridcomps/extdata/ExtDataFileReader.F90`
`element_count`/`server_bounds` construction (passing `has_de=has_de`)
hoisted out of the `if`; `if/else` reduced to guarding only
`MAPL_FieldGetCptr` (→ `c_null_ptr` when no DE). `collective_prefetch_data`
called **unconditionally** every field/every rank — required so the
per-`ClientThread` `collective_counter` (shared, local, unsynchronized —
see `pfio/ClientThread.F90:59,269,341`) stays in lock-step across ranks.

### 3. `infrastructure/esmf/FieldPointerUtilities.F90` — `copy()`
`copy()` (backing `MAPL_FieldCopy`, called from
`NonClimDataSetFileSelector.F90`'s `swap_bracket_fields` during
time-bracket updates) now early-returns when `.not. field_has_de(x)`. This
is a **purely local memcpy** (no MPI/collective semantics) — found via a
crash (`FieldPointerUtilities.F90:220/254`, `ESMF_FieldGet(...,
farrayPtr=...)` failing status=508) after fixing #1 above. (Superseded in
practice by fix #6 below, which makes this guard redundant-but-harmless;
left in place.)

### 4. `infrastructure/geom_io/GridPFIO.F90` — all 3 functions
- **`stage_data_to_file`** (WRITE; used by `HistoryCollectionGridComp.F90`
  History output, `RestartHandler.F90` restart write,
  `FieldBundleWrite.F90`): removed `if (.not.has_de) cycle`; mirrors the
  `ExtDataFileReader.F90` fix exactly — `element_count`/`server_bounds`
  (`has_de=has_de`) computed unconditionally, `FieldGetCptr` guarded (→
  `c_null_ptr`), `collective_stage_data` called unconditionally.
- **`request_data_from_file`** (READ; distinct from ExtData's own
  `read_items` — used by `RestartHandler.F90` restart read,
  `FieldBundleRead.F90`): same treatment, mirrors `collective_prefetch_data`.
- **`stage_coordinates_to_file`** (writes `lons`/`lats`/`corner_lons`/
  `corner_lats`): previously had **no `has_de` guard at all** — unconditional
  `ESMF_GridGetCoord(farrayPtr=...)` would crash on a no-DE rank. Fixed by
  computing `has_de` from the locally-created `field`, passing
  `has_de=has_de` into each `pFIOServerBounds(...)` call, and allocating a
  `(0,0)` placeholder array instead of calling `ESMF_GridGetCoord` when
  `.not. has_de`.

Both `stage_data_to_file`/`request_data_from_file` calling
`collective_stage_data`/`collective_prefetch_data` unconditionally (rather
than `cycle`-skipping) is required for the same `request_id`-lock-step
reason as fix #2.

### 5. Centralized fix: `infrastructure/esmf/FieldPointerUtilities.F90` — `get_cptr`
Rather than adding a `has_de` guard at each of ~15 individual call sites
across `FieldBLAS.F90`, `FieldUtilities.F90`, `FieldCondensedArray.F90`,
`FieldApplyUserRoutine.F90`, and the generic3g `ConvertUnitsTransform`/
`NormalizationTransform`/`TimeInterpolateTransform.F90`, the fix was
centralized in `get_cptr` — the single choke point every `assign_fptr`
variant and every direct `FieldGetCptr` caller funnels through:
```fortran
has_de = field_has_de(x, _RC)
if (.not. has_de) then
   cptr = c_loc(no_de_dummy_target)
   _RETURN(_SUCCESS)
end if
```
This works because every caller derives its `c_f_pointer()` shape from
`FieldGetLocalElementCount`/`FieldGetLocalSize` (both already DE-safe,
yielding zero-size shapes), so a zero-size, well-defined pointer results and
all downstream elementwise operations become no-ops. Verified this is
*sufficient* (no other call-site changes needed) by hand-tracing every
affected function.

### 6. NAG compatibility follow-up: `no_de_dummy_target`, not `C_NULL_PTR`
Fix #5's first version used `cptr = c_null_ptr`. A new test
(`test_assign_fptr_no_DE` in `Test_FieldPointerUtilities.pf`) immediately
crashed under NAG:
```
Runtime Error: Reference to disassociated POINTER X_PTR
```
Although `c_f_pointer(C_NULL_PTR, fptr, shape)` is legal per the Fortran
standard when every element of `shape` is zero, **NAG's runtime treats the
resulting pointer as fully disassociated** and aborts on any subsequent
reference to it (even a bare `SIZE()`) — a real compiler-compatibility gap,
not a hypothetical concern, since NAG is this project's primary compiler.
Fixed by pointing at a dedicated, never-dereferenced static dummy target
instead:
```fortran
integer(kind=C_INT8_T), target, save :: no_de_dummy_target = 0_C_INT8_T
...
cptr = c_loc(no_de_dummy_target)
```
This is standard-compliant (any valid address is acceptable as the basis
for a zero-size `c_f_pointer()` result) and resolved the NAG abort. Verified
`FieldCopy`'s `_RETURN_IF(c_associated(cptr_x, cptr_y))` early-exit still
works correctly (both no-DE fields now share the same dummy address, so
`c_associated` is still `.true.` when both sides have no DE).

## Tests added (Tier A + Tier B)

### Tier A — new dedicated test files
- **`infrastructure/esmf/tests/Test_FieldPointerUtilities.pf`** (added to
  the existing `MAPL.esmf.tests` target, no `MAX_PES` change needed — 3 PEs
  fits within its existing `MAX_PES 8`). 2-DE grid, 3 PEs, so PET 2 owns
  zero DEs. Tests `FieldGetCptr`, `assign_fptr` (asserts zero-size pointer
  on the no-DE rank), and `FieldCopy` (the function that actually crashed).
- **`infrastructure/geom_io/tests/Test_pFIOServerBounds.pf`** — its own new
  ctest target/executable, `MAPL.pFIOServerBounds.tests` (`MAX_PES 6`).
  **Important:** initially added to the existing `MAPL.GeomIO.tests` target
  with a `MAX_PES` bump, which deadlocked the *unrelated*, pre-existing
  `Test_FieldBundleIO.pf` (its pfio client/server singleton assumes it owns
  the whole VM/communicator — confirmed via `sample`-based stack trace
  showing all 6 ranks spinning in `MPI_Allreduce` inside
  `AbstractServer.F90::get_writing_pe`). Resolved by giving the new test its
  own target so it never shares a communicator with a pfio-server-using
  test. 8 test cases: gridded lat-lon (with/without `time_index`),
  cubed-sphere, and vert-only, each with `has_de=.true.`/`.false.` — `has_de`
  is a plain explicit argument to the constructor, so these test both
  branches directly at 1 (or 6, for cubed-sphere) PE without needing a real
  multi-PE decomposition mismatch.

### Tier B — extended existing files (`infrastructure/field/tests`)
- `field_utils_setup.F90`: `mk_field_r4_2d`/`mk_field_r8_2d` switched from
  raw `ESMF_FieldGet(farrayPtr=)` to `assign_fptr` (prerequisite — otherwise
  building a test fixture on the extra no-DE PET crashes before the test
  under test even runs).
- `CMakeLists.txt`: `MAX_PES` bumped 4 → 5 for the `MAPL.field.test_utils`
  target (covers `Test_FieldBLAS.pf`, `Test_FieldArithmetic.pf`,
  `Test_FieldCondensedArray_private.pf`, `Test_FieldDelta.pf`,
  `Test_FieldInfo.pf`).
- `Test_FieldBLAS.pf`: `npes=[5]` no-DE variants for `FieldCOPY`,
  `FieldSCAL`, `FieldAXPY`, `FieldConvertPrec`.
- `Test_FieldArithmetic.pf`: no-DE variants for `FieldPow`/`FieldNegate`,
  plus **new basic + no-DE tests for `FieldSet`/`FieldIsConstant`** (zero
  test coverage existed for either before this).
- `Test_FieldCondensedArray_private.pf`: no-DE shape assertions for
  `get_fptr_shape_private` (`product(shape) == 0` invariant).

## Verification performed

```bash
source ~/.zshrc && cmake --build build-debug -j 8 --target build-tests
source ~/.zshrc && ctest --test-dir build-debug -R "MAPL.esmf.tests" --output-on-failure
source ~/.zshrc && ctest --test-dir build-debug -R "MAPL.GeomIO.tests|MAPL.pFIOServerBounds.tests" --output-on-failure
source ~/.zshrc && ctest --test-dir build-debug -R "MAPL.field.test_utils|MAPL.field.test_fieldcreate" --output-on-failure
source ~/.zshrc && ctest --test-dir build-debug --output-on-failure   # full suite
```
Full suite: 76 tests, 69 pass. The 7 failures (`ll-ll`/`cs-cs`/`cs-ll`/
`ll-cs` Regrid_Util regression tests missing `LOCAL_REGRESSION_DATA_DIR`;
`case02`/`case11`/`case23` missing `pyyaml` on `ctest`'s `python3`) are all
pre-existing/environmental and predate this session's changes.

## Quick reference: root cause classes found this session

| Symptom | Root cause | File(s) fixed |
|---|---|---|
| `I_PTR` subscript out of range in `read_and_share` | `global_count`/`global_start` zeroed for no-DE rank, but server picks one representative message per `request_id` assuming consistent full-domain shape across all contributing ranks | `pFIOServerBounds.F90` |
| `ESMF_FieldGet(..., farrayPtr=...)` fails (status 508) in `FieldCopy`/`copy()` | Purely local field-to-field memcpy unconditionally dereferenced a no-DE field's local array pointer | `FieldPointerUtilities.F90` |
| `request_id` desync risk from `cycle`-skipping collective calls | `collective_prefetch_data`/`collective_stage_data` share one local, unsynchronized, per-`ClientThread` counter (`collective_counter`) — skipping a call on some ranks but not others (when has_de varies per-field within one rank) desyncs which `request_id` means which field across ranks | `ExtDataFileReader.F90`, `GridPFIO.F90` (both functions) |
| Unconditional `ESMF_GridGetCoord(farrayPtr=...)` | Same class as `FieldGetCptr` — no `has_de` guard existed at all | `GridPFIO.F90::stage_coordinates_to_file` |
| Same-class crash, ~15 call sites (`FieldBLAS`, `FieldUtilities`, `FieldCondensedArray`, `FieldApplyUserRoutine`, generic3g transforms) | All funnel through `assign_fptr`/`FieldGetCptr` → `get_cptr` | Centralized fix in `FieldPointerUtilities.F90::get_cptr` |
| `Reference to disassociated POINTER` under NAG | NAG treats `c_f_pointer(C_NULL_PTR, fptr, zero_shape)` as disassociated, contrary to a strict standard reading | `FieldPointerUtilities.F90::get_cptr` (`no_de_dummy_target`) |
| MPI deadlock (`MPI_Allreduce` in `AbstractServer.F90::get_writing_pe`) when adding a multi-PE pFIOServerBounds test | `MAPL.GeomIO.tests`'s pfio client/server singleton assumes it owns the whole VM; bumping that target's `MAX_PES` broke `Test_FieldBundleIO.pf` | Gave `Test_pFIOServerBounds.pf` its own ctest target/executable |

## Key file:line reference index

| File | What |
|---|---|
| `infrastructure/geom_io/pFIOServerBounds.F90` | `has_de`-aware bounds ctor |
| `gridcomps/extdata/ExtDataFileReader.F90` | `read_items` |
| `infrastructure/esmf/FieldPointerUtilities.F90` | `copy()` guard, `get_cptr` centralized fix + `no_de_dummy_target` |
| `infrastructure/geom_io/GridPFIO.F90` | `stage_coordinates_to_file`, `stage_data_to_file`, `request_data_from_file` |
| `pfio/ClientThread.F90` | shared `collective_counter` root cause (lines 59, 269, 341, 526-534) |
| `pfio/ServerThread.F90` | `read_and_share` crash site (lines 405-477) |
| `pfio/AbstractServer.F90` | `get_writing_pe` — MPI_Allreduce deadlock site when `MAPL.GeomIO.tests`'s MAX_PES was (incorrectly) bumped |
| `infrastructure/esmf/tests/Test_FieldPointerUtilities.pf` | new — `get_cptr`/`assign_fptr`/`FieldCopy` no-DE tests |
| `infrastructure/geom_io/tests/Test_pFIOServerBounds.pf` | new — `pFIOServerBounds` no-DE tests (own ctest target) |
| `infrastructure/field/tests/{Test_FieldBLAS,Test_FieldArithmetic,Test_FieldCondensedArray_private}.pf` | extended with no-DE variants |
| `infrastructure/field/tests/field_utils_setup.F90` | `mk_field_r4_2d`/`mk_field_r8_2d` switched to `assign_fptr` |

## Build/test commands (NAG, `build-debug/`)

```bash
# Build (modules already loaded via ~/.zshrc, marked "L")
source ~/.zshrc && cmake --build build-debug -j 8 2>&1 | tee -a build-debug/build.log
source ~/.zshrc && cmake --build build-debug -j 8 --target build-tests

# Targeted retest
source ~/.zshrc && ctest --test-dir build-debug -R case12 --output-on-failure
source ~/.zshrc && ctest --test-dir build-debug -R "MAPL.esmf.tests|MAPL.GeomIO.tests|MAPL.pFIOServerBounds.tests|MAPL.field.test_utils|MAPL.field.test_fieldcreate" --output-on-failure

# Full suite
source ~/.zshrc && ctest --test-dir build-debug --output-on-failure 2>&1 | tee build-debug/ctest-full.log
```

## Nothing outstanding

No open TODOs remain from this effort. Possible future follow-ups (not
requested, not started):
- Fix the vert-only `has_de=.false.` asymmetry in `pFIOServerBounds.F90`
  (see "Known, documented, unaddressed limitation" above) if it's ever
  found to matter in practice.
- Consider whether `FieldCreateFieldSlice` (`FieldApplyUserRoutine.F90`,
  public API) and `FieldDelta.F90`'s `reallocate_field`/`reallocate_fields`
  (also public) warrant their own `has_de` guards for robustness against
  future direct callers, even though nothing currently calls them directly
  with a no-DE field.
- The broader "Tier 3/4" sweep (raw `farrayPtr=` usage in
  `superstructure/state/*.F90`, `infrastructure/geom/VectorBasis/*.F90`,
  `infrastructure/regridder_mgr/Regridder.F90`, various
  `geom/*/fill_coordinates.F90`) was explicitly deferred/skipped per your
  answer #4 in the earlier planning round.
