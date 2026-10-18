## Context

See `proposal.md` - Why. Current state, verified directly against the
source tree in this session (not re-derived from the roadmap's own
counts, which this design cross-checks):

- `superstructure/generic/graph/` has 62 top-level `.F90` files (listed
  directly via `ls`), plus a `containers/` subdirectory (21 gFTL map
  instantiations) and `include/` (1 file, `IdTemplate.inc`).
- `superstructure/generic/graph/CMakeLists.txt` lists every production
  source as a flat `target_sources(MAPL.generic PRIVATE ...)`
  path-string list (85 entries per the roadmap's own count, covering
  more than just the `graph/` subtree — the `generic` target spans
  multiple directories). `superstructure/generic/graph/tests/CMakeLists.txt`
  is the analogous list for the pFUnit test files.
- `target_include_directories(MAPL.generic PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include)` in
  `superstructure/generic/graph/CMakeLists.txt` is the only CMake
  entry that resolves `#include "IdTemplate.inc"` for the 5 real
  `#include` sites (`NodeId.F90`, `DependencyNetworkId.F90`,
  `PortId.F90`, `CallbackInterfaceId.F90`, `HierarchyId.F90` — verified
  by grepping for the literal directive, not a comment mention;
  `CallbackInterfaceRegistry.F90` and
  `superstructure/generic/graph/tests/IdGeneratorTestSupport.F90` each
  only *mention* the template in a comment and are not moved).
- **Phase 12** (wiring 5a2's seven remaining `StateItemCharacteristic`
  subclasses into real dispatch) has **not landed** — confirmed: no
  `openspec/changes/archive/*phase-12*` and no active
  `openspec/changes/*phase-12*` exists in this checkout as of this
  session. Phase 12 is expected to edit `build_characteristics()`'s
  own insertion point in `GraphBuilder.F90` and touch some subset of
  the 15 `characteristics/`-bound files. This is the one real,
  documented sequencing dependency this change has on anything else in
  flight.

## Goals / Non-Goals

**Goals:**
- Relocate the four named families (`characteristics/`, `transforms/`,
  `callback/`, `id_types/`) with zero behavior change: no module
  renamed, no public interface changed, no `use`-site source edits
  beyond what the directory move itself requires (there are none —
  Fortran `use` resolves by module name via the build's module-file
  search path, not by source-file path).
- Keep every moved family's own build target membership unchanged
  (`MAPL.generic`, `MAPL.generic.graph` tests) — this is a path-string
  edit inside existing `target_sources`/`target_include_directories`
  calls, not a new CMake target or `add_subdirectory`.
- Preserve the existing `containers/` convention: family-specific gFTL
  map instantiations (e.g. `CallbackArgumentSpecMap.F90`) stay under
  the shared top-level `containers/`, not a new
  `callback/containers/`, matching every other family's own map
  instantiations (`PortIdStringMap.F90`, `NodeIdGraphNodeMap.F90`,
  etc., which are not characteristic- or transform-specific either).
- Respect the real sequencing dependency on Phase 12 for
  `characteristics/` specifically; do not block the other three moves
  on it.

**Non-Goals:**
- No change to `GraphBuilder.F90` itself or any of its 11 submodules'
  own internal organization (Phase 17's own, separate scope — see
  `docs/graph/spec/20-implementation-roadmap.md` §20.4.17). This
  design only notes the two phases interact at the review-timing
  level (per the roadmap's own sequencing note), not that either edits
  the other's files.
- No change to any module's public interface, type definitions, or
  runtime behavior. If code review finds any non-mechanical edit
  creeping into one of the four move tasks, that edit is out of scope
  for this change and should be filed separately.
- No decision about Phase 12's own eventual `characteristics/`
  insertion point or dispatch logic — this change only moves files;
  Phase 12, whenever it lands, does so against whichever directory
  layout is current at that time (pre-move if 18-4 hasn't run yet,
  post-move if it has).

## Decisions

**D1 — Four independent `git mv` batches, not one combined commit.**
Each family (`transforms/`, `id_types/`, `callback/`,
`characteristics/`) is mechanically independent of the other three —
no file is a member of two families, and the four destination
directories don't nest. Splitting into four batches (matching the
roadmap's own 18-1..18-4 sub-sequencing) lets the three
Phase-12-independent moves land immediately, in any order, without
waiting on or blocking each other, while `characteristics/` alone
waits on Phase 12. A single combined commit would force the three
ready moves to wait for the one blocked move for no mechanical reason.

**D2 — `callback/` map instantiations stay in the shared top-level
`containers/`, not a new `callback/containers/`.** Alternative
considered: move the 5 `Callback*Map.F90` files into
`callback/containers/`, mirroring the family-subdirectory idea
literally. Rejected: every other family's own gFTL map instantiations
(`PortIdStringMap.F90`, `NodeIdGraphNodeMap.F90`,
`NodeId_NodeIdSet_Map.F90`, `DependencyNetworkIdNetworkMap.F90`, etc.)
already live in the single shared `containers/` directory with no
per-family split; inventing one only for `callback/` would make
`containers/` organization inconsistent depending on which family you
ask about, trading one minor inconsistency (callback maps sit beside
non-callback maps) for a worse one (some families get a nested
`containers/`, most don't). This matches the roadmap's own explicit
recommendation (§20.4.18).

**D3 — `CallbackInterfaceRegistry.F90` moves to `callback/`, not
`id_types/`, despite textually mentioning `IdTemplate.inc`.**
Alternative: group it with the id-type files since it is adjacent in
the roadmap's own id_types discussion. Rejected: it has no real
`#include "IdTemplate.inc"` directive (grep-verified; only a comment
mentions it) and it is conceptually a registry *of* callback
interfaces, not an identity type itself — it belongs with the other
callback-family files it's used alongside.

**D3-correction (found during apply, task 1.1 preflight) —
`CallbackInterfaceId.F90` moves to `id_types/` only, making `callback/`
a 6-file move, not 7.** The roadmap's own §20.4.18 double-counts this
file: it appears in both the "7 top-level callback files" list and the
"5 real Id-type files" list (the latter confirmed by the same
`#include "IdTemplate.inc"` grep this design already uses as its
placement criterion for every other `id_types/` file). D3's own
reasoning above (no real `#include`, conceptually a registry) applies
to `CallbackInterfaceRegistry.F90` only — it was never meant to cover
`CallbackInterfaceId.F90`, which the roadmap's own id_types paragraph
independently and correctly places by `#include` evidence. Applying
this change's own stated placement criterion consistently
(`id_types/` = files with a real `#include "IdTemplate.inc"`) resolves
the duplicate unambiguously in favor of `id_types/`. Confirmed with
the user during apply rather than guessed. `callback/` is therefore 6
files: `CallbackArgumentSpec.F90`, `CallbackInterface.F90`,
`CallbackInterfaceRegistry.F90`, `CallbackMethodBinding.F90`,
`CallbackMethodSpec.F90`, `CallbackStateBinding.F90`.

**D4 — `id_types/` folds `include/` into itself rather than keeping a
separate `include/` directory.** The one file under `include/`
(`IdTemplate.inc`) is used exclusively by the 5 moved id-type files;
keeping a standalone `include/` sibling to `id_types/` for one file
with one consumer family adds a directory for no grouping benefit.
Each of the 5 `#include "IdTemplate.inc"` directives stays a relative,
same-directory include (unchanged text) once the template sits beside
them in `id_types/`; only `target_include_directories` in
`superstructure/generic/graph/CMakeLists.txt` needs its path updated
from `include` to `id_types`.

**D5 — Sequencing gate on `characteristics/` enforced by a tasks.md
checklist gate, not a tooling/CI check.** Alternative: add an
automated pre-flight check (e.g. a script asserting no open Phase-12
change exists) before allowing the `characteristics/` move to proceed.
Rejected as disproportionate: this is a one-time, human-reviewed
directory move; the roadmap document and this design both already
state the dependency explicitly, and `tasks.md` records an explicit
manual check (confirm no Phase-12 openspec change is in-flight) as the
task's own first step. Automating this for a single historical move
would add tooling with no reuse value.

## Risks / Trade-offs

- **[Risk]** A `git mv` across many files in one working session risks
  an incomplete move (some files moved, some `use`/`#include` sites
  not updated) leaving the build broken until fully repaired. →
  **Mitigation:** each of the 4 sub-tasks (18-1..18-4) is scoped to
  complete and verify (`cmake --build <dir> --target build-tests` +
  `ctest -L ESSENTIAL`) *within the same session*, per family, before
  moving to the next family. No family's move is considered done until
  its own build+test verification passes on at least one compiler.
- **[Risk]** `id_types/`'s relative `#include` path could silently
  resolve to a stale, pre-move copy of `IdTemplate.inc` if the old
  `include/` directory is deleted incompletely or a build directory
  caches an old search path. → **Mitigation:** delete the old
  `include/` directory as part of the same commit (not left as a
  stale duplicate), and do a clean (not incremental) CMake
  reconfigure before the first post-move build, matching
  `docs/graph/ARCHITECTURE.md` §4's own emphasis on NAG/gfortran build
  commands being environment-sensitive.
- **[Risk]** NAG and gfortran resolve submodule/cross-TU symbol
  visibility differently (documented precedent:
  `docs/graph/ARCHITECTURE.md` §4.2, the `is_ordinary_resolvable_import`
  missing-`public` bug that only gfortran's linker caught). A pure
  directory move changes no symbol visibility, but the only way to be
  sure of that is to build+test with both compilers, not one. →
  **Mitigation:** each of the 4 sub-tasks' own exit criterion requires
  verification on NAG first (per `AGENTS.md`'s own stated preference
  for faster iteration), then confirmed with gfortran, not either one
  alone.
- **[Risk]** Landing the `characteristics/` move (18-4) concurrently
  with Phase 12 work-in-progress would force Phase 12 to resolve a
  rename/move conflict on every characteristics file it touches, on
  top of its own real work. → **Mitigation:** D5/18-4's own explicit
  gate — confirm, immediately before starting 18-4, that no Phase-12
  openspec change exists yet (neither `archive/` nor an active
  top-level entry); if one has been created and is in-flight, defer
  18-4 until it lands.
- **[Trade-off]** Four separate `git mv` batches (D1) instead of one
  commit means `git log` shows the reorganization as four smaller
  commits rather than one atomic one, and `characteristics/` may land
  weeks or months after the other three if Phase 12 is delayed. This
  is accepted deliberately — the roadmap's own sequencing requirement
  makes "one atomic commit" impossible without either blocking the
  three ready moves or violating the Phase-12 ordering constraint.

## Migration Plan

Each of the 4 sub-tasks is its own mechanical migration with the same
shape, executed independently:

1. **18-1 (`transforms/`, 5 files) and 18-2 (`id_types/`, 5 files +
   `IdTemplate.inc`) and 18-3 (`callback/`, 7 files)** — no ordering
   constraint between these three; may run in any order, any session,
   as soon as this change's tasks.md is approved.
2. **18-4 (`characteristics/`, 15 files)** — gated: do not start until
   confirmed (by checking `openspec/changes/` for a Phase-12 entry,
   archived or active) that Phase 12 has landed or is not yet filed as
   an in-flight change.
3. For each sub-task: `git mv` the files to their new directory, edit
   the two `CMakeLists.txt` path-string lists (production + tests),
   edit the 5 `#include` sites only for 18-2, build with NAG, run
   `ctest -L ESSENTIAL`, then repeat with gfortran. Commit only after
   both compilers pass with 0 new failures relative to the
   pre-move baseline.

**Rollback:** each sub-task is an independent, small commit (per D1);
reverting any single sub-task's commit (`git revert` or `git mv` back)
fully undoes that one family's move without affecting the other
three, since no file is shared between families and no non-mechanical
code edit is bundled into any move commit.

## Open Questions

None — the one genuine unknown (whether Phase 12 has landed by the
time 18-4 is picked up) is a timing check performed at the start of
that one sub-task, not a design, spec, or task-breakdown question;
tasks.md states the check explicitly as 18-4's own first step.
