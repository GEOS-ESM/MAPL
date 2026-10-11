## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4.9 ("Phase 11") names
two separate, confirmed-by-direct-code-reading gaps left open by the
landed `graph-child-state-population` change, both standing between the
codebase today and Phase 11's own stated goal — a real run-phase value
provably propagating through a real extension chain into a sibling's own
real `ESMF_State`:

1. **Multi-hop extension chains have no materialized intermediate
   nodes.** `ExtensionResolution.F90`'s own `build_chain` leaves every
   link's payload unallocated by design; only the chain's final node is
   ever materialized. A real two-characteristic chain (e.g.
   `UnitsConverterTransform` -> `TypeKindConverterTransform`) fails
   outright the first time anything actually pulls on it
   (`GraphStateItem: esmf_field is not allocated`). Fixing this needs a
   real change to `find_or_build_extension_chain`/`build_chain`'s own
   public interface so every intermediate `NodeId` is exposed, not only
   the final one.
2. **A sibling connection's export-side proxy is never aliased to the
   real export field a run phase actually writes.** Even with gap (2)'s
   own prerequisite work from `graph-child-state-population` landed
   (export-node materialization in `resolve_one`'s mismatch branch), the
   materialized field is an independent `FieldCreate`, never a
   `MAPL_NamedAlias` of the real export field — confirmed by a resulting
   `NaN` rather than a crash when a real run phase's changed value was
   asserted. Fixing this needs the same "always alias, never duplicate"
   treatment `graph-child-state-population`'s own design.md (D1-D6)
   already applied to the *import* (sibling-receiving) side, mirrored
   onto the *export* (sibling-producing) side.

Phase 3 (§20.4.1), Phase 4 (§20.4.3), Phase 5 (§20.4.4), and Phase 10
(§20.4.8) each hit the same "too heterogeneous for one spec-driven
change" problem — or, in Phase 10's case, a strict dependency chain
needing explicit ordering — and were each resolved with their own
sub-sequencing section before implementation started. Phase 11, as
currently recorded in §20.4.9, is a flat two-item list with no stated
ordering, dependency relationship, or scope boundary between the two
gaps. On inspection the two gaps are **not** a strict chain the way
Phase 10's sub-changes were: gap (2) (export aliasing) is useful and
independently demonstrable against the *existing* single-characteristic
fixture (`units_mismatch_run_phase`) with no need for gap (1)'s
interface change; gap (1) (multi-hop materialization) is only required
once a chain has two or more characteristics. Scoping real implementation
work for either gap without first recording this independence — and the
lower-risk, do-this-first ordering it implies — would repeat the
"ambiguous bucket" mistake the earlier sub-sequencing sections were
written to avoid.

## What Changes

- Add a new `§20.4.10 Phase 11 sub-sequencing` section to
  `docs/graph/spec/20-implementation-roadmap.md`, following the same
  shape as §20.4.1/§20.4.3/§20.4.4/§20.4.8: ordered (or explicitly
  parallel-safe) sub-changes, each with its own scope boundary,
  dependencies, and cross-reference back to the two gaps named in the
  existing §20.4.9 text.
- Name two sub-changes (11a, 11b), record that they are independent in
  principle (neither needs the other's code to land first) but that 11a
  is the recommended first pick — no public-interface change, reuses
  the already-landed `units_mismatch_run_phase` fixture, and completes
  Phase 11's own single-characteristic demonstration outright:
  - **11a. Export-side field aliasing** — mirror
    `graph-child-state-population`'s own D1-D6 "always alias, never
    duplicate" treatment from the import side onto the export side, so
    a sibling connection's export-side proxy is a real
    `MAPL_NamedAlias` of the field a run phase actually writes. Demonstrable
    immediately against the existing `units_mismatch_run_phase` fixture
    (single characteristic, no chain-interface change needed).
  - **11b. Multi-hop extension-chain intermediate materialization** —
    change `ExtensionResolution.F90`'s own `find_or_build_extension_chain`/
    `build_chain` public interface to expose every intermediate `NodeId`
    (not only the final one), so each link can be materialized with its
    own correctly-evolved typekind/units/geom. Needed before any
    two-or-more-characteristic chain (e.g. `units_and_typekind_mismatch`)
    can execute `compute()` for real. Independent of 11a's own code path
    (export aliasing vs. chain-building), but only becomes *useful* for a
    real end-to-end demonstration once 11a has also landed — Phase 11's
    own stated goal is a propagated value, and without 11a the export
    side still never carries the real written value regardless of how
    many hops are correctly materialized.
- Record explicitly that neither sub-change reopens Phase 10d's own
  double-invocation hazard (unaffected either way — both gaps are
  state-population/materialization concerns, not invocation-count
  concerns) or 3b2/Phase 7c (unrelated fixture families).
- No source code changes, no `openspec/specs/` capability requirement
  changes in this change itself — this proposal only establishes the
  sub-sequencing plan that 11a (and later 11b) will implement as their
  own, separate `openspec` changes.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None — this proposal only extends a planning-record document
(`docs/graph/spec/20-implementation-roadmap.md`); it does not change any
requirement in an `openspec/specs/` capability. `.openspec.yaml` for this
change sets `skip_specs: true` accordingly, matching the existing
`phase-5-sub-sequencing`/`phase-10-sub-sequencing` precedent.

## Impact

- **Affected file:** `docs/graph/spec/20-implementation-roadmap.md` only
  (new `§20.4.10` section under the existing `§20.4.9` Phase 11 entry; no
  other section's content changes).
- **Downstream effect:** once this section exists, 11a (export-side field
  aliasing) can be filed as its own `openspec` change and demonstrated
  against the already-landed `units_mismatch_run_phase` fixture without
  waiting on 11b; 11b (multi-hop intermediate materialization) can be
  filed independently, in parallel or afterward, as its own change — it
  is the one that unlocks a real `units_and_typekind_mismatch`-style
  two-characteristic run-phase demonstration.
- **No code, build, or test impact.** This is a documentation-only
  planning change.
