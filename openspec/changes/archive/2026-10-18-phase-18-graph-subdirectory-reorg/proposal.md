## Why

`superstructure/generic/graph/` holds ~62 top-level `.F90` files in one
flat directory. Several large, clearly-related families
(characteristics, transforms, callback, id types) are hard for a human
reader to see the structure of because nothing on disk groups them.
This is Phase 18 of the graph-architecture implementation roadmap
(`docs/graph/spec/20-implementation-roadmap.md` §20.4.18), raised
directly by the codebase owner.

## What Changes

- Move 15 characteristic-family files into a new
  `superstructure/generic/graph/characteristics/` subdirectory.
- Move 5 transform-family files into a new
  `superstructure/generic/graph/transforms/` subdirectory.
- Move 6 top-level callback-family files into a new
  `superstructure/generic/graph/callback/` subdirectory (the existing
  `containers/Callback*Map.F90` gFTL map instantiations stay under the
  shared top-level `containers/`, consistent with every other family's
  own map instantiations). `CallbackInterfaceId.F90` is excluded from
  this family — it has a real `#include "IdTemplate.inc"` directive,
  same as the other `id_types/` files, so it belongs there instead
  (corrects a double-count in the roadmap's own §20.4.18 file lists,
  confirmed with the user during apply).
- Move 5 id-type files plus `include/IdTemplate.inc` into a new
  `superstructure/generic/graph/id_types/` subdirectory, folding
  `include/` into it; update the `#include "IdTemplate.inc"` directive
  in each of the 5 moved files and the `target_include_directories`
  CMake entry that currently points at `include/`.
- Update `superstructure/generic/graph/CMakeLists.txt` (flat
  `target_sources` path-string list) and
  `superstructure/generic/graph/tests/CMakeLists.txt` for every
  relocated production file's test counterpart.
- Sequence `characteristics/` strictly after Phase 12 (wiring 5a2's
  remaining `StateItemCharacteristic` subclasses into real dispatch)
  has landed, since Phase 12 is expected to touch files this move
  relocates. The other three moves (`transforms/`, `id_types/`,
  `callback/`) have no Phase 12 overlap and may proceed independently,
  in any order, at any time.
- Pure `git mv` + path-string update + import/include-path audit. No
  module's public interface, module name, or runtime behavior changes.
  **Not BREAKING** for any consumer outside this directory — every
  moved file's own module name is unchanged; only its file-system path
  and (for the 5 id-type files) the relative `#include` path change.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

(none — this is a pure file/directory reorganization with zero
spec-level behavior change; see `skip_specs: true` in this change's
`.openspec.yaml`.)

## Impact

- **Affected files:** 15 characteristics + 5 transforms + 6 callback +
  5 id_types + 1 `IdTemplate.inc` = 32 production files relocated,
  plus their corresponding test files under
  `superstructure/generic/graph/tests/` and both `CMakeLists.txt`
  files (production and tests) updated for the new paths.
- **Not affected:** `containers/*.F90` (all 21 files stay in place,
  including the 5 `Callback*Map.F90` instantiations), every other
  top-level file in `superstructure/generic/graph/` not named above,
  and all files in `superstructure/generic/` outside the `graph/`
  subtree (`GraphBuilder.F90` and its submodules, `OuterMetaComponent/`,
  etc. — Phase 17's own `GraphBuilder.F90` size work is a separate,
  only loosely-related phase; see design.md for the explicit
  sequencing note between the two).
- **Build systems:** both NAG and gfortran CMake configurations must
  be re-verified after the move (the hard layering rule and all
  `#include`/`use` resolution are compiler-visibility-sensitive, per
  `docs/graph/ARCHITECTURE.md` §4.2's own NAG-vs-gfortran linkage
  lesson).
- **Dependents:** `GraphBuilder.F90` and its 11 submodules `use` many
  of these modules by name (unaffected — module names don't change)
  but may have path-based `#include`/build-dependency assumptions to
  re-check for the 5 id-type files specifically.
