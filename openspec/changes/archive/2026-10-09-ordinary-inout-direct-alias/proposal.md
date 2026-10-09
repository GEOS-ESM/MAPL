## Why

`docs/graph/spec/16-inout-items.md` (REQ-INOUT-001) settles the direct-alias
case of an ordinary inout state item: when a borrower's required payload
exactly matches an owner's, no transform is needed in either direction —
it follows directly from the already-implemented REQ-EXT-003 no-op
principle. `docs/graph/spec/20-implementation-roadmap.md` §20.4.4 lists
this as Phase 5b, "ready to scope now," independent of 5a/5c, and
explicitly scoped to REQ-INOUT-001 only — the general (non-direct-alias)
case remains blocked behind an unwritten REQ-INOUT-002 design addendum
(tracked separately as 5b2) and is out of scope here. Today the
graph-native path has no way to declare an item as inout at all, so even
this already-settled degenerate case cannot be exercised.

## What Changes

- Add a way to declare a state item as an ordinary inout item (a borrower
  that shares one owner's underlying ESMF payload, no new Transform
  needed), distinct from a plain import/export declaration.
- When a declared inout borrower's required payload exactly matches its
  owner's (no grid/units/precision/other mismatch), `GraphBuilder` wires
  owner and borrower as direct aliases, reusing the existing REQ-EXT-003
  no-op path — no new field storage or `TransformGraphNode` is created in
  either direction.
- Represent the owner-write-back relationship as a dependency edge in a
  second ("return") network distinct from the forward network the
  ordinary no-op path already populates, per `06-dependency-network.md`
  REQ-DEP-008's existing per-network producer allowance for exactly this
  case, sequenced so the forward-network read and return-network write
  are never part of the same update pass (same non-overlapping-pass
  precedent `REQ-DEP-008a` already grants the callback get/put pattern).
- When a declared inout borrower's required payload does NOT match its
  owner's exactly, the system SHALL reject the declaration explicitly
  rather than silently building an extension chain — REQ-INOUT-002
  reserves the non-trivial (non-direct-alias) case for a future design
  addendum; this change MUST NOT attempt it.
- `GraphBuilder`'s real connection-resolution step gains a new branch,
  parallel to its existing callback-interface branch: a destination item
  declaring inout intent is delegated to this capability's resolution
  instead of plain ordinary (import/export) exact-match resolution.
  Existing non-inout resolution behavior is unchanged.

## Capabilities

### New Capabilities
- `graph/ordinary-inout`: declaring an ordinary inout state item
  (owner/borrower pair) and graph-native wiring for the direct-alias case
  only (REQ-INOUT-001) — forward and return dependency-network edges, both
  no-op (no `TransformGraphNode`), sequenced across distinct update
  passes, plus explicit rejection of any non-direct-alias (mismatched)
  pairing rather than silent extension-chain construction.

### Modified Capabilities
- `graph/graph-builder`: real connection resolution gains a new branch so
  a matched destination item declaring inout intent is delegated to the
  `graph/ordinary-inout` capability rather than resolved through the
  existing ordinary (single-direction) exact-match path. Existing
  non-inout resolution behavior is unchanged.

## Impact

- `superstructure/generic/specs/VariableSpec.F90` (and
  `VariableSpec_private.F90`): new way to mark/declare an item as an
  inout borrower and identify its owner.
- `superstructure/generic/GraphBuilder.F90`: new inout-resolution branch
  in real connection resolution (parallel to the existing
  callback-interface branch), reusing the existing no-op
  characteristics-match check (REQ-EXT-003) and rejecting mismatches
  instead of invoking extension-chain construction.
- `superstructure/generic/tests/Test_GraphBuilder.pf` and/or a new
  `Test_OrdinaryInout.pf`: pFUnit coverage for the direct-alias wiring,
  the two-network (forward/return) edge structure, and the explicit
  mismatch-rejection path.
- `docs/graph/spec/16-inout-items.md`: Status note update once
  REQ-INOUT-001 is implemented (the `[DEFERRED]`/`[SPECULATIVE]` status
  at the top of the document applies to the whole document including the
  general case; this change does not flip that status — only
  REQ-INOUT-001 itself is addressed).
- No change to `ESMF_StateIntent_Flag` usage or legacy
  `ComponentSpecParser`/YAML `intent:` parsing — ESMF has no `INOUT`
  state-intent value, and legacy coupler behavior has no inout mechanism
  to match (unlike Phase 3's exact-parity requirement, this is genuinely
  new capability, not a reproduction of existing behavior). Surface
  syntax (programmatic declaration vs. new YAML field) is a design.md
  decision, not a proposal-level commitment.
- Build/test environment for this change: NAG 7.2.41
  (`module load nag/7.2.41 mpi baselibs`). Seven existing `ctest` failures
  on this platform are pre-existing and unrelated to this change; they are
  not regressions to chase as part of this work.
