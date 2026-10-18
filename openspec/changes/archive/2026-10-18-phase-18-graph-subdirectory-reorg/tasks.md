## 1. Preflight

- [x] 1.1 Confirm current file inventory matches proposal/design counts: `ls superstructure/generic/graph/*.F90` and diff mentally against the 15 characteristics / 5 transforms / 7 callback / 5 id_types lists in design.md's Context section — re-verify, do not assume this count hasn't drifted since this change was planned.
- [x] 1.2 Capture a pre-move baseline: `ctest --test-dir nag -L ESSENTIAL --output-on-failure` (and gfortran equivalent if a build already exists) — record pass/fail counts to compare against after each sub-task.

## 2. Move `transforms/` (18-1)

- [x] 2.1 `git mv` the 5 files into `superstructure/generic/graph/transforms/`: `Transform.F90`, `TransformBase.F90`, `TransformGraphNode.F90`, `TypeKindConverterTransform.F90`, `UnitsConverterTransform.F90`.
- [x] 2.2 Update the 5 corresponding path strings in `superstructure/generic/graph/CMakeLists.txt`'s `target_sources` list.
- [x] 2.3 Move/update any corresponding test files under `superstructure/generic/graph/tests/` and their path strings in `superstructure/generic/graph/tests/CMakeLists.txt`, if any test file's own location is also being reorganized (otherwise confirm no test-side path references the old production path directly).
- [x] 2.4 Build with NAG (`module load nag-stack/7.2.41 && cmake --build nag --target build-tests -j 8`); fix any path/visibility error.
- [x] 2.5 Run `ctest --test-dir nag -L ESSENTIAL --output-on-failure`; confirm identical pass/fail counts to the 1.2 baseline.
- [x] 2.6 Build and test with gfortran (`module load gfortran-stack && cmake --build gfortran --target build-tests -j 8` then `ctest --test-dir gfortran -L ESSENTIAL --output-on-failure`); confirm identical results.
- [x] 2.7 Commit this family's move alone (per design.md D1 — four independent commits).

## 3. Move `id_types/` (18-2)

- [x] 3.1 `git mv` the 5 id-type files into `superstructure/generic/graph/id_types/`: `NodeId.F90`, `DependencyNetworkId.F90`, `PortId.F90`, `CallbackInterfaceId.F90`, `HierarchyId.F90`.
- [x] 3.2 `git mv superstructure/generic/graph/include/IdTemplate.inc` into the same `id_types/` directory; remove the now-empty `include/` directory.
- [x] 3.3 Confirm each of the 5 moved files' `#include "IdTemplate.inc"` directive still resolves as a same-directory relative include (no text change expected, since the template moved with them) — verify by reading each file, not assuming.
- [x] 3.4 Update `target_include_directories(MAPL.generic PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)` in `superstructure/generic/graph/CMakeLists.txt` to point at `id_types` instead of `include`.
- [x] 3.5 Update the 5 corresponding path strings in `target_sources`.
- [x] 3.6 Force a clean (not incremental) CMake reconfigure for both build directories before building, per design.md's Risks section (stale include-path caching).
- [x] 3.7 Build + test with NAG, then gfortran, per the same pattern as 2.4-2.6; confirm identical pass/fail counts to baseline.
- [x] 3.8 Commit this family's move alone.

## 4. Move `callback/` (18-3)

- [x] 4.1 `git mv` the 6 top-level callback files into `superstructure/generic/graph/callback/`: `CallbackArgumentSpec.F90`, `CallbackInterface.F90`, `CallbackInterfaceRegistry.F90`, `CallbackMethodBinding.F90`, `CallbackMethodSpec.F90`, `CallbackStateBinding.F90`. (`CallbackInterfaceId.F90` is NOT in this list — it has a real `#include "IdTemplate.inc"` directive, same criterion as every `id_types/` file, and moves under 18-2/task group 3 instead; see design.md D3-correction.)
- [x] 4.2 Confirm the 5 `containers/Callback*Map.F90` files are left in place under the shared top-level `containers/` (per design.md D2 — do not create `callback/containers/`).
- [x] 4.3 Update the corresponding path strings in `target_sources`.
- [x] 4.4 Build + test with NAG, then gfortran, per the same pattern as 2.4-2.6; confirm identical pass/fail counts to baseline.
- [x] 4.5 Commit this family's move alone.

## 5. Move `characteristics/` (18-4) — gated

- [x] 5.1 Immediately before starting this task group, confirm no Phase-12 openspec change exists yet: check `openspec/changes/` (both top-level/active and `archive/`) for any `*phase-12*`/5a2-dispatch-wiring entry. If one is found active (not yet archived), stop and defer this task group until it lands. **Confirmed during apply:** no `openspec/changes/` entry (active or archived) named or scoped to Phase 12 exists; the only hits for "phase 12"/"5a2" across `openspec/changes/` are incidental mentions in other phases' own sequencing notes (e.g. `2026-10-12-phase-6-sub-sequencing`), not a Phase-12 implementation itself. Safe to proceed.
- [x] 5.2 `git mv` the 15 characteristic-family files into `superstructure/generic/graph/characteristics/`: `AttributesCharacteristic.F90`, `CharacteristicStatus.F90`, `ConservationCharacteristic.F90`, `GeometryCharacteristic.F90`, `NormalizationCharacteristic.F90`, `PhysicalUnitsCharacteristic.F90`, `QuantityTypeCharacteristic.F90`, `SetSharedCharacteristic.F90`, `StandardNameCharacteristic.F90`, `StateItemCharacteristic.F90`, `StateItemCharacteristicDetection.F90`, `StateItemCharacteristicKind.F90`, `TypeKindCharacteristic.F90`, `UngriddedDimsCharacteristic.F90`, `VerticalCoordinateCharacteristic.F90`.
- [x] 5.3 Update the 15 corresponding path strings in `target_sources`.
- [x] 5.4 Update/move corresponding test files and `superstructure/generic/graph/tests/CMakeLists.txt` entries if any live there under old paths.
- [x] 5.5 Build + test with NAG, then gfortran, per the same pattern as 2.4-2.6; confirm identical pass/fail counts to baseline.
- [x] 5.6 Commit this family's move alone.

## 6. Final verification

- [x] 6.1 Run the full top-level `ctest --test-dir nag` and `ctest --test-dir gfortran` (not just `-L ESSENTIAL`) after all four sub-tasks have landed; confirm the same pass/fail counts as the pre-change baseline documented in `docs/graph/ARCHITECTURE.md` §4 (71/75, same 4 pre-existing `Regrid_Util` failures) — zero new regressions from the reorganization.
- [x] 6.2 Confirm no file was left duplicated (old path still present alongside new) via `git status` showing clean moves only (renames), not add+delete-elsewhere-untracked pairs.
- [x] 6.3 Update `docs/graph/ARCHITECTURE.md` §1.1's module-map table and any other doc referencing the old flat `graph/` paths, if this change's own review finds stale path references worth fixing (optional polish, not a build-correctness requirement).
