## Why

`GraphBuilder`'s connection resolution dispatches on `class is
(MatchConnection)` only; every other `Connection` subtype — including
`SimpleConnection`, the exact-name primitive legacy's own
`MatchConnection%activate`/`connect` construct and delegate to for each
matched pair (`MatchConnection.F90:109,166`) — falls to `class default`
and is silently skipped (`GraphBuilder_ConnectionResolution_smod.F90:427-433,1028-1034`).
This inverts legacy's own structure: legacy's `MatchConnection` is a
regex *enumerator* over `SimpleConnection`'s real wiring; graph-native
code gave full resolution to the enumerator and nothing to the
primitive it's supposed to generalize.

The gap is not cosmetic. 32 of the 42 real scenario fixtures under
`superstructure/generic/tests/scenarios/` declare connections via
explicit `src_name`/`dst_name` (`SimpleConnection`), against only 10
using `all_unsatisfied: true` (`MatchConnection`). The one end-to-end
graph-vs-legacy parity test, `Test_GraphBuilderEquivalence.pf`, had to
invent a synthetic `all_unsatisfied`-only fixture
(`graph-scenarios/graphbuilder_equivalence/`) specifically because
every pre-existing named-connection fixture (`statistics_real`,
`history_1`, `scenario_1`, …) is invisible to graph resolution — the
file's own header comment says so
(`Test_GraphBuilderEquivalence.pf:10-24`). A later regression
(Task 6.8 of `hierarchy-wide-component-graph`, discovered via the
`statistics_real` fixture) had to patch around the gap rather than
close it: `connection_resolves_import`'s `class default` branch
(`GraphBuilder_ConnectionResolution_smod.F90:602-617`) treats *any*
non-`MatchConnection` connection naming a destination by exact short
name as "already resolved," without checking the source side has a
matching export at all — a trust heuristic standing in for real
resolution, and a latent false-negative: a `SimpleConnection` whose
source export does not actually exist would still be waved through as
resolved, suppressing the unresolved-import report that should fire.

`docs/graph/spec/` mentions `SimpleConnection` exactly once, as a bare
name in `20-implementation-roadmap.md`'s legacy-file deprecation list
(line 1649) — zero requirements, zero scenarios anywhere in the spec
tree. `openspec/specs/graph/graph-builder/spec.md`'s own "Ordinary
connections are resolved into dependency edges" requirement describes
exact-name matching as if it were `MatchConnection`'s property, when
exact-match is actually `SimpleConnection`'s defining shape and
`MatchConnection` only degenerates to it per matched pair.

The resolution logic itself is already connection-type-agnostic: both
`resolve_match_connection` and `check_match_connection_unsatisfied`
touch their `conn` argument only through the `Connection`-base
accessors `get_source()`/`get_destination()`
(`GraphBuilder_ConnectionResolution_smod.F90:53-54,136-137`); the
~350 lines of real resolution logic that follow (callback detection,
inout detection, characteristic-mismatch extension chains, state
materialization) operate entirely on the already-matched
`StateItemNode`, never on anything specific to `MatchConnection`'s own
type. The declared dummy-argument type is the only thing excluding
`SimpleConnection` — this is a narrow, mechanical repair, not a
redesign.

## What Changes

- Widen `GraphBuilder`'s three connection-dispatch sites
  (`graphbuilder_check_unsatisfied_imports`,
  `graphbuilder_resolve_connections`, and `connection_resolves_import`)
  so `SimpleConnection` is resolved through the same real logic
  `MatchConnection` already uses — `MatchConnection` becomes the
  explicit regex generalization that resolves each matched pair the
  same way a declared `SimpleConnection` would, restoring legacy's own
  `MatchConnection`-delegates-to-`SimpleConnection` relationship instead
  of maintaining two independent, unequally-supported dispatch arms.
- Retire the `connection_resolves_import` `class default` trust
  heuristic (`:602-617`) once `SimpleConnection` has real resolution.
  Replace "does a non-`MatchConnection` connection name this
  destination by exact name" with a real check — does the destination
  node already have a resolved producer in the graph — closing the
  false-negative risk described above.
- `ReexportConnection` is explicitly **out of scope**: it remains
  `class default`/unresolved by this change. It is structurally
  different (export-to-export passthrough via `ExtensionFamily`
  reuse, not a dependency edge) and is named here only so this repair
  is not mistaken for having covered it.
- Update `openspec/specs/graph/graph-builder/spec.md`'s existing
  "Ordinary connections are resolved into dependency edges" and
  "Non-exact-match cases are not resolved by this capability"
  requirements to name `SimpleConnection` and `MatchConnection`
  explicitly, and to state `MatchConnection`'s relationship to
  `SimpleConnection` (pattern-expansion over the same exact-match
  primitive), rather than describing exact-match resolution as if it
  were `MatchConnection`'s own property.
- Add `SimpleConnection` resolution tests mirroring existing
  `MatchConnection` coverage (exact match, mismatch/extension-chain,
  unresolved import, idempotent re-resolution), and replace
  `test_non_matchconnection_is_skipped`'s premise (which currently
  asserts `SimpleConnection` is skipped — the opposite of this change's
  outcome) with a test asserting it resolves identically to an
  equivalent `MatchConnection`.
- Extend `Test_GraphBuilderEquivalence.pf` to run the pre-existing
  `statistics_real` scenario fixture (a flat, sibling-to-sibling
  `SimpleConnection` configuration exercising vertical-regrid and
  vector-bundle extension chains, not just a trivial exact match)
  through `GRAPH_MODE_BOTH` parity, instead of relying solely on the
  synthetic `graphbuilder_equivalence` fixture invented to work around
  this gap.

## Capabilities

### New Capabilities

(none — this extends an existing capability's requirements)

### Modified Capabilities

- `graph/graph-builder`: "Ordinary connections are resolved into
  dependency edges" and "Non-exact-match cases are not resolved by
  this capability" requirements change to cover `SimpleConnection`
  explicitly and to state `MatchConnection`'s delegation relationship
  to it; the unsatisfied-import determination and resolve-connections
  requirements gain scenarios covering `SimpleConnection` directly.

## Impact

- `superstructure/generic/GraphBuilder_ConnectionResolution_smod.F90`
  — dispatch and resolution-body changes (see design.md).
- `superstructure/generic/GraphBuilder.F90` — any new/widened public
  interfaces the submodule split requires.
- `superstructure/generic/tests/Test_GraphBuilder.pf` — new
  `SimpleConnection` resolution tests; revised
  `test_non_matchconnection_is_skipped`.
- `superstructure/generic/tests/Test_GraphBuilderEquivalence.pf` —
  additional `statistics_real` parity case.
- `openspec/specs/graph/graph-builder/spec.md` — requirement text and
  scenarios.
- `docs/graph/spec/20-implementation-roadmap.md` — line 1649's bare
  `SimpleConnection` mention gets a real cross-reference once this
  lands, instead of listing it only as a legacy file pending removal.
- No change to `ReexportConnection`, `superstructure/generic/connection/*.F90`
  (legacy `Connection` hierarchy itself), or any `StateRegistry` code
  path — legacy behavior is unaffected; only `GraphBuilder`'s
  graph-native resolution gains coverage.
