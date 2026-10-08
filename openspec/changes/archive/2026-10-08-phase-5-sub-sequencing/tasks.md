## 1. Draft §20.4.4 section content

- [x] 1.1 Write the §20.4.4 "Phase 5 sub-sequencing" heading and intro
      paragraph, stating the same rationale as §20.4.1/§20.4.3 (Phase 5
      does not fit a single spec-driven change proposal) and referencing
      this change's proposal.md - Why for the full justification.
- [x] 1.2 Write the **5a. StateItemCharacteristic hierarchy** entry per
      design.md Decision 2: scope (`18` §18.2-§18.8), readiness (ready
      now), required up-front design.md resolution of CharacteristicStatus
      naming (§18.3), CharacteristicType naming (§18.4), ordering-
      delegation mechanism (§18.6/Q13), and absent-key-vs-INVALID
      representation (§18.5), and its dependency (Phases 1-3 only).
- [x] 1.3 Write the **5b. Ordinary inout, direct-alias case** entry per
      design.md Decision 1: scope (`16` REQ-INOUT-001 only), readiness
      (ready now, no design addendum needed), and independence from
      5a/5c.
- [x] 1.4 Write the **5b2. Ordinary inout, general case** entry per
      design.md Decision 1: explicitly BLOCKED status, the REQ-INOUT-002
      design addendum precondition (4 named open points), the
      discovered-vs-pre-existing-blocker distinction from 3b2/4b2, and an
      instruction not to file this sub-change's own openspec proposal
      until the addendum exists.
- [x] 1.5 Write the **5c. Compiled-execution optimization** entry per
      design.md Decision 3: scope (Q9's recommended approach), readiness
      (ready now), the rationale for why REQ-REV-009's gate is already
      satisfied by landed Phase 1-4 work, and the ordering dependency on
      5a (not on 5b).
- [x] 1.6 Write the ordering/dependency summary (design.md Decision 4's
      diagram or an equivalent prose/table form consistent with this
      document's existing style) showing 5a and 5b as independent
      entry points, 5c following 5a, and 5b2 blocked until its addendum
      exists.
- [x] 1.7 Add a repo/tooling note paralleling §20.4.1's and §20.4.3's own
      notes, stating where each ready sub-change's code would live (same
      repo-separation reasoning as Phase 3/4: Phase 5 items integrate with
      landed Phase 1-4 MAPL-repo code, so no repo-separation saving
      applies here either).

## 2. Insert into the roadmap document

- [x] 2.1 Insert the completed §20.4.4 section into
      `docs/graph/spec/20-implementation-roadmap.md` immediately after
      the existing §20.4.3 (Phase 4 sub-sequencing) and before §20.5
      (Cross-reference).
- [x] 2.2 Update the Phase 5 bullet in §20.4 (the one-line summary) to
      reference the new §20.4.4 for sub-sequencing detail, matching how
      the Phase 3 and Phase 4 bullets already reference §20.4.1/§20.4.3.

## 3. Verify

- [x] 3.1 Re-read the edited `docs/graph/spec/20-implementation-roadmap.md`
      end to end to confirm section numbering, cross-references, and
      Markdown heading levels are consistent with the rest of the
      document (matching §20.4.1/§20.4.2/§20.4.3's existing style).
- [x] 3.2 Confirm no other section of the roadmap (or any other spec
      file under `docs/graph/spec/`) was modified — this change is
      scoped to the single new section plus the one-line §20.4 cross-
      reference update (task 2.2).
- [x] 3.3 Confirm the new section does not assert anything beyond what
      design.md's Decisions 1-4 actually settled (no new technical
      claims introduced only at write-up time).
