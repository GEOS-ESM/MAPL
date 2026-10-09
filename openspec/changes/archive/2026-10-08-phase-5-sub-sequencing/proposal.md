## Why

`docs/graph/spec/20-implementation-roadmap.md` §20.4 names "Phase 5" as a
single bucket covering three independent, speculative/deferred items —
`18-state-item-characteristics.md` (StateItemCharacteristic hierarchy),
`16-inout-items.md` (ordinary inout items), and Q9 (compiled-execution
optimization, `17-open-questions.md`). Phase 3 and Phase 4 each hit the
same "too heterogeneous for one spec-driven change" problem and were
resolved by adding an explicit sub-sequencing section to the roadmap
(§20.4.1, §20.4.3) before any implementation work started. Phase 5 has no
equivalent section yet, and each of its three items carries its own
explicit "do not implement without further design" caveat (REQ-INOUT-002;
`18`'s own header; Q9's "distinct, later phase, gated on..." framing) —
attempting to scope real implementation work for any of them without first
settling how they relate to each other and in what order would repeat the
exact mistake the Phase 3/4 sub-sequencing sections were written to avoid.

## What Changes

- Add a new `§20.4.4 Phase 5 sub-sequencing` section to
  `docs/graph/spec/20-implementation-roadmap.md`, following the same
  shape as §20.4.1 (Phase 3) and §20.4.3 (Phase 4): ordered sub-changes,
  each with its own scope boundary, dependencies, and an explicit note of
  what design work (if any) must happen before that sub-change's own
  `openspec` proposal can be filed.
- For each of the three Phase 5 items, record in that section:
  - Whether it is ready to be split into an actual sub-change proposal now,
    or whether it first requires a design addendum (per `16`'s own
    REQ-INOUT-002, `18`'s own header caveat, or Q9's own gating) before a
    sub-change can be meaningfully scoped.
  - Its dependency relationship to the other two items and to prior phases
    (e.g. Q9 explicitly depends on the interpreted/reference implementation
    from Phases 1-4 being validated first; `18` is gated on Q11, which
    `17-open-questions.md` already records as resolved).
- No source code, no `openspec/specs/` capability requirements, and no
  implementation of `16`/`18`/Q9 themselves change as part of this
  proposal — this is a planning-record update only, matching the existing
  precedent of §20.4.1/§20.4.3, which were written directly into the
  roadmap document rather than as their own `openspec` change with a
  capability spec delta.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None — this proposal only extends a planning-record document
(`docs/graph/spec/20-implementation-roadmap.md`); it does not change any
requirement in an `openspec/specs/` capability. `.openspec.yaml` for this
change sets `skip_specs: true` accordingly.

## Impact

- **Affected file:** `docs/graph/spec/20-implementation-roadmap.md` only
  (new `§20.4.4` section; no other section's content changes).
- **Downstream effect:** once this section exists, each Phase 5 item that
  is marked "ready to scope" can be filed as its own `openspec` change
  (mirroring 3a/3b/.../4a-4g); items marked "needs design addendum first"
  are explicitly blocked from having an implementation change filed until
  that addendum lands, consistent with `16` REQ-INOUT-002 and `18`'s own
  gating language.
- **No code, build, or test impact.** This is a documentation-only
  planning change.
