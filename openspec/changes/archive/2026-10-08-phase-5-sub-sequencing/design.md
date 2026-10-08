## Context

`docs/graph/spec/20-implementation-roadmap.md` §20.4 lists Phase 5 as three
items with no internal ordering: `18` (StateItemCharacteristic hierarchy),
`16` (ordinary inout items), and Q9 (compiled-execution optimization).
Phases 1-4 are now landed (per the archive: `component-hierarchy-foundation`
through `route-handle-value-key`), each with its own deliberately narrow
scope and explicit deferrals recorded in §20.4.1/§20.4.3. This design
decides how Phase 5 should be split into sub-changes, mirroring that same
discipline, so that a future `/opsx-propose` for any Phase 5 item starts
from an already-bounded scope instead of re-litigating readiness each time.

Two of the three items carry their own explicit implementation gate that
this design must evaluate against present state, not merely restate:

- `16` REQ-INOUT-002: "Any implementation of non-trivial (non-direct-alias)
  inout support MUST be preceded by an explicit design addendum" resolving
  4 named open points (revision authority under two producers, lazy
  direction selection, recursion, general authority rules).
- Q9 (`17-open-questions.md`): compiled-execution work is "gated on the
  reference (interpreted) implementation being correct and validated
  first" (REQ-REV-009).

`18` carries a softer caveat ("do not implement ahead of [Q11's]
resolution") — Q11 is recorded as resolved in `17-open-questions.md`'s
status table, so `18`'s hard blocker is already cleared; what remains are
ordinary `[OPEN]` naming/mechanism points (CharacteristicStatus,
CharacteristicType, ordering-delegation mechanism, Q13) of the same kind
Phase 3/4 sub-changes have routinely resolved inside their own design.md
(e.g. 4b resolved REQ-MTH-011 step (c)'s convergence algorithm the same
way, "as a planned, up-front design decision before implementation
starts").

## Goals / Non-Goals

**Goals:**
- Decide, for each of the three Phase 5 items, whether it is ready to be
  filed as its own `openspec` sub-change now, or whether it is blocked on
  a design addendum that does not yet exist.
- Decide the relative ordering/dependency relationship among whichever
  sub-changes are ready, consistent with each item's own stated gate.
- Produce the actual roadmap text (§20.4.4) that records these decisions,
  in the same form as §20.4.1/§20.4.3, so it is citable by future
  sub-change proposals the same way those sections already are.

**Non-Goals:**
- Does not write any sub-change's own proposal/specs/design/tasks for
  `18`, `16`, or Q9 themselves — that is deliberately left to whichever
  future `/opsx-propose` invocation picks up a "ready" item from §20.4.4.
- Does not resolve `18`'s own `[OPEN]` naming items (CharacteristicStatus,
  CharacteristicType, ordering-delegation mechanism) — those are correctly
  scoped to that future sub-change's own design.md, not this meta-level
  sequencing document, exactly as 4b resolved its own open point inside
  its own design.md rather than in a roadmap-level document.
- Does not write `16`'s required design addendum (REQ-INOUT-002) — that
  addendum, if and when someone chooses to pursue general inout support,
  is itself substantial design work and out of scope here.

## Decisions

### Decision 1 — Split `16` into a ready narrow slice and a blocked general case

`16` already contains exactly the same shape Phase 4 used repeatedly
(4e/4f/4g): a settled, narrow slice (REQ-INOUT-001, the direct-alias case
— "requires no transforms in either direction... settled and follows
directly from REQ-EXT-003's no-op principle") plus an explicitly-open
general case. Treat them as two sub-changes:

- **5b. Ordinary inout, direct-alias case** (REQ-INOUT-001 only) — ready
  to scope now. No design addendum needed: the direct-alias case is
  already fully settled, requires no new Transform, and REQ-INOUT-002's
  gate applies only to "non-trivial (non-direct-alias)" support.
- **5b2. Ordinary inout, general case** — NOT ready to scope. Blocked on
  the REQ-INOUT-002 design addendum (revision authority under two
  producers, lazy direction selection, recursion, general authority
  rules). Numbered `5b2` to mirror the project's existing precedent for a
  closely-related follow-up sub-change (3b/3b2, 4b/4b2) — but note the
  distinction explicitly: those were follow-ups *discovered* during their
  parent's implementation; `5b2` is a *pre-existing*, spec-declared
  blocker (REQ-INOUT-002), known before `5b` is even filed. Do not file
  `5b2`'s own `openspec` proposal until the addendum exists.

**Alternative considered:** treat `16` as a single sub-change spanning
both the direct-alias and general cases, with the general case's open
points resolved inside that one sub-change's own design.md (the pattern
used for `18`, Decision 2 below). Rejected: REQ-INOUT-002 is explicit and
stronger than an ordinary `[OPEN]` tag — it names a required addendum as a
precondition, not merely a design decision to make along the way, and the
direct-alias slice is independently useful and shippable without waiting
on it.

### Decision 2 — `18` is ready to scope as one sub-change, with its open points resolved in that sub-change's own design.md

Unlike `16`, `18` has no REQ-level language requiring an addendum to
precede implementation — its header caveat was tied to Q11, which is
already resolved. Its remaining `[OPEN]` items (CharacteristicStatus
naming §18.3, CharacteristicType naming §18.4, ordering-delegation
mechanism §18.6/Q13, absent-key-vs-INVALID representation §18.5) are
ordinary open design points of the kind Phase 3/4 sub-changes have
routinely closed inside their own design.md before writing tasks (4b's
REQ-MTH-011 step (c) convergence algorithm is the closest precedent: an
`[OPEN]` spec item resolved as "a planned, up-front design decision
before implementation starts").

- **5a. StateItemCharacteristic hierarchy** (`18`, all of §18.2-§18.8) —
  ready to scope now. That sub-change's own design.md MUST resolve the
  naming/mechanism opens above before its tasks.md is written (same
  discipline 4b followed), per this schema's own design.md requirement
  ("ambiguity that benefits from technical decisions before coding").
  Depends only on Phases 1-3 (`GraphStateItem`, `09-extension-reuse.md`'s
  existing chain-building machinery) — no dependency on `16` or Q9.

**Alternative considered:** defer `18` to a roadmap-level design addendum
the same way `16` requires, on the theory that an `[OPEN]` item is an
`[OPEN]` item regardless of document. Rejected: REQ-INOUT-002 is a
spec-author-authored MUST; `18` has no equivalent sentence anywhere in
`18-state-item-characteristics.md` or `17-open-questions.md` — the actual
spec text treats `18`'s opens as ordinary implementation-time design
decisions, not a precondition. Inventing a stronger gate than the spec
itself states would misrepresent `18`'s actual status.

### Decision 3 — Q9 is ready to scope now; its own gate is already satisfied by landed Phase 1-4 work

Q9's stated gate is that compiled-execution work come "after the reference
(interpreted) implementation being correct and validated first"
(REQ-REV-009). The interpreted implementation Q9 would compile — frozen
`ComponentGraph`, `DependencyNetwork` walk, demand-driven `update()`
(Phase 1-2), `MethodGraphNode` invocation, callback wiring, route handles
(Phase 3-4) — is landed and exercised by each sub-change's own pFUnit
suite (per the archive list). That satisfies Q9's stated precondition;
REQ-REV-009's "validated" does not require the stronger Phase 6 entry bar
(legacy `StateRegistry` retired at production scale) — Q9 only needs the
*interpreted* path to be the trusted oracle, which §20.4's own Phase 6
framing already assumes stays alive indefinitely as "a reference oracle,"
not something gated on legacy removal.

- **5c. Compiled-execution optimization** (Q9) — ready to scope now,
  following Q9's own recommended approach (walk a *frozen*
  `ComponentGraph`, emit a direct call sequence per `DependencyNetwork`,
  keep the interpreted path alive as reference oracle per REQ-REV-009).
  Independent of `16`/`18` in principle (compiles whatever
  `DependencyNetwork`/`TransformGraphNode` structure exists at freeze
  time, generically) — but sequence it **after 5a**, not before: if `18`
  lands first, its characteristic-driven `TransformGraphNode`s
  (`ConvertUnitsTransform`, the precision-conversion transform,
  `RegridTransform`) are already part of what 5c needs to compile
  correctly; landing 5c first would mean revisiting it once 5a's new
  Transform chains exist. `5b`'s direct-alias inout case adds no new
  Transform (REQ-INOUT-001), so 5c has no ordering dependency on `5b`.

**Alternative considered:** sequence Q9 first, on the theory that it is
orthogonal to characteristic/inout content and "exploits low fan-out"
regardless of what the graph contains. Rejected per the ordering
discussion above — orthogonal in principle does not mean no revisit cost
in practice, and `17-open-questions.md`'s own Q10 ordering discussion
already places compilation "deliberately last" among open items for a
similar reason (nothing above should be gated on it, but it doesn't need
to run before anything either).

### Decision 4 — Resulting order and independence

```
Phase 1-4 (landed)
   |
   +--> 5a  StateItemCharacteristic hierarchy        (ready now)
   |       |
   |       v
   +--> 5c  Compiled-execution optimization          (ready now, after 5a)
   |
   +--> 5b  Ordinary inout, direct-alias case         (ready now, independent)
             |
             v
         5b2 Ordinary inout, general case             (BLOCKED: needs REQ-INOUT-002 addendum)
```

`5a` and `5b` have no dependency on each other and MAY be filed in either
order or in parallel. `5c` should follow `5a`. `5b2` is not filed until
its design addendum exists — this document does not write that addendum.

## Risks / Trade-offs

- **[Risk]** Marking `5a`/`5c` "ready to scope now" could be read as
  pressure to implement speculative work the spec author explicitly
  flagged "do not block on these" (§20.4). → **Mitigation:** this document
  only unblocks *filing a scoped sub-change proposal*; it does not compel
  implementation, and §20.4's "do not block on these" instruction (meaning
  *other* phases should not wait on Phase 5) is unaffected — nothing above
  Phase 5 depends on any Phase 5 sub-change landing.
- **[Risk]** `5b2` remaining indefinitely blocked could cause the roadmap
  to accumulate stale sequencing text if the addendum is never written. →
  **Mitigation:** follow the same discipline as §20.4.2's "growable list"
  — `5b2` stays recorded as blocked rather than silently dropped, and
  whoever eventually writes the REQ-INOUT-002 addendum updates §20.4.4
  directly, per §20.5's cross-reference discipline ("update this document
  rather than letting the plan silently diverge from practice").
- **[Risk]** Treating Q9's "validated" gate as already satisfied by
  ordinary pFUnit coverage (rather than the stronger Phase 6 production-
  scale bar) is itself a judgment call, not something the spec text
  states numerically. → **Mitigation:** recorded explicitly as Decision 3
  with its rationale, rather than left implicit, so it can be
  re-litigated if a future reviewer disagrees — consistent with
  `17-open-questions.md` Q9's own "Confidence" framing for this kind of
  call.

## Migration Plan

Single, non-code edit: add `§20.4.4 Phase 5 sub-sequencing` to
`docs/graph/spec/20-implementation-roadmap.md`, placed after the existing
§20.4.3 (Phase 4 sub-sequencing) and before §20.5 (Cross-reference). No
rollback mechanism beyond ordinary git revert; no build, test, or runtime
impact.

## Open Questions

None — every point that would affect the sub-change split or ordering is
resolved above (Decisions 1-4). `18`'s own naming/mechanism opens and
`16`'s REQ-INOUT-002 addendum are intentionally left to their own future
sub-changes (Non-Goals), not deferred here as unresolved.
