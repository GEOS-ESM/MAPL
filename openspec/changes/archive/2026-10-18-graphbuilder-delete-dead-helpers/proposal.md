## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.17's 17-2
characterization session found 3 of `GraphBuilder.F90`'s 19 plain
shared-helper procedures have zero real callers anywhere (production
code or tests): `component_spec_for`, `enumerate_qualified_exports`,
and `collect_qualified_export_index` (the last reachable only through
the second, itself dead). This is sub-step 17-4a, the first and
smallest slice of 17-4 — a strict net-reduction with no design
tradeoff, deliberately separated from 17-4b (moving the 7 clean
single-caller helpers) and 17-4c (the shared-helpers-module design
question), which depend on different decisions and come later.

## What Changes

- Delete the three dead procedure bodies from
  `superstructure/generic/GraphBuilder.F90`: `component_spec_for`,
  `enumerate_qualified_exports`, `collect_qualified_export_index`.
- Remove the corresponding `public ::` declarations for
  `component_spec_for` and `enumerate_qualified_exports`
  (`collect_qualified_export_index` was never exported — no
  `public ::` line exists for it).
- Remove or rewrite comments in `GraphBuilder.F90` and
  `GraphBuilder_ConnectionResolution_smod.F90` that reference these
  three names purely as historical "why we didn't use the old
  approach" prose, where that comment's substance is not otherwise
  worth preserving. Comments in `Test_GraphBuilder.pf` and
  `apps/graph_diagnostic/graph_diagnostic.F90` that reference these
  names in prose only (no actual call) are explicitly out of scope
  and left untouched.
- No production behavior changes: these three procedures have no
  live caller today, so removing them has no observable effect on
  any code path.

## Capabilities

### New Capabilities

(none — pure dead-code removal, no new behavior)

### Modified Capabilities

(none — no spec-level requirement changes; `skip_specs: true` is set
in this change's `.openspec.yaml`)

## Impact

- **Affected file:** `superstructure/generic/GraphBuilder.F90` (net
  reduction of roughly 70-90 lines: three procedure bodies, two
  `public ::` lines, and any dead-comment cleanup).
- **Possibly affected file:**
  `superstructure/generic/GraphBuilder_ConnectionResolution_smod.F90`
  (comment-only edits, if any, at the two `component_spec_for`
  references near lines 111/204).
- **Not affected:** `Test_GraphBuilder.pf` (no test calls any of the
  three procedures; any comment mentioning them is left as accurate
  historical prose, per this phase's own scope), the 7 clean-move
  helpers and the shared-helpers-module question (17-4b/17-4c,
  separate later work), and everything under
  `superstructure/generic/graph/` (none of the three helpers live
  there).
- **Verification:** `cmake --build nag --target build-tests` and
  `cmake --build gfortran --target build-tests`, then
  `ctest -L ESSENTIAL --output-on-failure` on both compilers, compared
  against a pre-change baseline run. Zero new failures expected on
  either compiler, since no live caller exists for any of the three
  deleted procedures.
</content>
