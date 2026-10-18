## Why

`AttributesCharacteristic.F90` and `VerticalCoordinateCharacteristic.F90`
each carry their own, near-identical private `sorted_names(names) result(sorted)`
insertion-sort helper over a `StringVector`, used only to produce a stable,
sorted, comma-joined signature string for `get_signature`. The duplication
was introduced because each file's helper is `private` to its own module,
and the second copy's own comment admits it was "reimplemented here...
since that module's own copy is private." `docs/graph/spec/20-implementation-roadmap.md`
§20.4.19 (Phase 19) investigated the backlog's original, broader framing
("some growable arrays of strings should be `StringVector` instead of
fixed-width arrays") and found no genuine `StringVector`-candidate
instance in the graph codebase — every array at these sites is sized once
from a known count before any loop runs. The real, actionable issue
narrowed to this one duplication, which this change fixes now while it is
still a small, two-call-site change.

## What Changes

- Extract one shared `sorted_names(names) result(sorted)` function (same
  signature both existing copies already use: `type(StringVector),
  intent(in) :: names` -> `character(:), allocatable :: sorted(:)`) into a
  new shared utility module under `superstructure/generic/graph/`.
- Update `AttributesCharacteristic.F90` and
  `VerticalCoordinateCharacteristic.F90` to `use` the shared helper and
  delete their own private copies.
- No behavior change: both call sites (`attributes_get_signature`,
  the vertical-coordinate signature/overlap-list call sites) produce
  byte-identical output before and after, since the extracted function is
  a verbatim copy of the existing logic.
- Out of scope (explicitly, per the roadmap's own narrowing): the two
  `GraphBuilder_CallbackWiring_smod.F90` call sites named in the original
  backlog item are a different problem shape (indexed iteration, not
  sorting) and need no change. The one genuine growable-array instance
  found repo-wide (`mp_utils/profiler/reporting/CsvProfileReporter.F90`)
  is outside the graph/generic codebase and is explicitly not scoped here.

## Capabilities

This is a pure internal refactor: it extracts a private helper function
into a shared location with no change to any observable behavior, public
interface, or spec-level requirement. No capability's requirements change.
`skip_specs: true` is set in this change's `.openspec.yaml` accordingly.

### New Capabilities
(none)

### Modified Capabilities
(none)

## Impact

- **Code**: `superstructure/generic/graph/characteristics/AttributesCharacteristic.F90`,
  `superstructure/generic/graph/characteristics/VerticalCoordinateCharacteristic.F90`,
  one new small utility module file under
  `superstructure/generic/graph/` (exact filename left as an
  implementation-time choice between a new `StringSortUtilities.F90` or
  reuse of an existing general-utility module, per the roadmap's own
  "exact home TBD" note), plus the owning `CMakeLists.txt` for the new
  file if one is added.
- **Tests**: no new test needed for behavior (signature output is
  unchanged); re-run the existing
  `Test_StateItemCharacteristicSubclasses.pf`/`Test_StateItemCharacteristic.pf`
  coverage that already exercises
  `AttributesCharacteristic%get_signature`/
  `VerticalCoordinateCharacteristic%get_signature` as the regression
  check.
- **Dependencies**: none new: the shared helper has the same
  `gFTL2_StringVector` dependency both existing copies already have.
- **Risk**: very low — mechanical extraction of verbatim logic, two call
  sites, no public API change.
