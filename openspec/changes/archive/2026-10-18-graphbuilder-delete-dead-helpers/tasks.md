## 1. Independent re-verification (do not trust 17-2's figures blindly)

- [x] 1.1 Re-run `grep -rn 'component_spec_for\b' --include="*.F90" --include="*.pf" .` across the whole repo (excluding `nag/`, `gfortran/`, `build/`), classify every hit as (a) declaration/end-statement, (b) `public ::` line, (c) comment prose, or (d) actual call.
- [x] 1.2 Repeat 1.1 for `enumerate_qualified_exports`.
- [x] 1.3 Repeat 1.1 for `collect_qualified_export_index`.
- [x] 1.4 Confirm no hit in 1.1-1.3 is classification (d) (an actual call) — specifically check `apps/graph_diagnostic/graph_diagnostic.F90` (landed after the 17-2 characterization session) for any new call that session could not have seen. If any real call is found, stop and report it instead of deleting.
- [x] 1.5 Record the exact grep commands run and their output for the final report.

## 2. Capture pre-change baseline

- [x] 2.1 Record current line count of `superstructure/generic/GraphBuilder.F90` (`wc -l`).
- [x] 2.2 `module load nag-stack/7.2.41`; `cmake --build nag --target build-tests -j 8`; `ctest --test-dir nag -L ESSENTIAL --output-on-failure`; save pass/fail counts.
- [x] 2.3 `module load gfortran-stack`; `cmake --build gfortran --target build-tests -j 8`; `ctest --test-dir gfortran -L ESSENTIAL --output-on-failure`; save pass/fail counts.

## 3. Delete the three dead procedures

- [x] 3.1 Locate current (possibly shifted) line ranges for `component_spec_for`, `enumerate_qualified_exports`, `collect_qualified_export_index` in `GraphBuilder.F90` — do not assume the roadmap's cited line numbers (~905-920, ~1028-1039, ~1041-1067) are still accurate.
- [x] 3.2 Delete the three procedure bodies (function/subroutine through matching `end function`/`end subroutine`).
- [x] 3.3 Remove `public :: component_spec_for` and `public :: enumerate_qualified_exports` declarations. (`collect_qualified_export_index` has no `public ::` line — nothing to remove there.)

## 4. Comment cleanup (judgment call, scoped)

- [x] 4.1 Review `GraphBuilder.F90`'s own internal comments mentioning `enumerate_qualified_exports`/`collect_qualified_export_index` (including the design.md D4 reference near line ~1070-1074); delete or rewrite comments that are purely historical "why we didn't use X" color explaining the now-deleted code, while preserving any comment substance that still conveys real design rationale (just without naming a deleted procedure).
- [x] 4.2 Review `GraphBuilder_ConnectionResolution_smod.F90`'s comments referencing `component_spec_for` (near lines ~111, ~204); apply the same judgment as 4.1.
- [x] 4.3 Confirm (from the 1.1-1.3 classification) that `Test_GraphBuilder.pf`'s comments near line ~3734 (`component_spec_for`) and ~2916 (`enumerate_qualified_exports`) are prose-only with no actual call; leave them untouched per the proposal's explicit scope.
- [x] 4.4 Leave `apps/graph_diagnostic/graph_diagnostic.F90`'s comment referencing `enumerate_qualified_exports` untouched — it documents a naming-convention precedent, not a dependency on the procedure's existence.

## 5. Build and test — NAG

- [x] 5.1 `module load nag-stack/7.2.41`; `cmake --build nag --target build-tests -j 8` — confirm clean build.
- [x] 5.2 `ctest --test-dir nag -L ESSENTIAL --output-on-failure`; diff pass/fail counts against the 2.2 baseline — confirm 0 new failures.

## 6. Build and test — gfortran

- [x] 6.1 `module load gfortran-stack`; `cmake --build gfortran --target build-tests -j 8` — confirm clean build.
- [x] 6.2 `ctest --test-dir gfortran -L ESSENTIAL --output-on-failure`; diff pass/fail counts against the 2.3 baseline — confirm 0 new failures, identical to NAG's result.

## 7. Documentation and reporting

- [x] 7.1 Record post-change line count of `GraphBuilder.F90`; confirm reduction is in the expected ~70-90 line range (or explain the discrepancy if not).
- [x] 7.2 In `docs/graph/spec/20-implementation-roadmap.md` §20.4.17, mark **17-4a** as **DONE** in the sub-sequencing list (matching the existing 17-1/17-2/17-3 "DONE" convention), with a one-line summary of what was deleted and the measured line-count reduction.
- [x] 7.3 If independent re-verification (Section 1) found any discrepancy versus the 17-2 characterization's claims, correct the roadmap text directly and note the correction explicitly — do not silently fix it.
- [x] 7.4 Final report: re-verification commands/findings, before/after `GraphBuilder.F90` line counts, exact ctest commands run on both compilers with before/after pass counts, roadmap update confirmation, and any discrepancy found.
</content>
