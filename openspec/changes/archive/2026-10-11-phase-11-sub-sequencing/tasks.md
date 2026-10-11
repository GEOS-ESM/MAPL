## 1. Draft §20.4.10 section content

- [x] 1.1 Write the §20.4.10 "Phase 11 sub-sequencing" heading and intro
      paragraph: state the same "does not fit a single spec-driven
      change proposal without sub-sequencing" rationale as
      §20.4.1/§20.4.3/§20.4.4/§20.4.8, reference proposal.md - Why and
      the existing §20.4.9 text for the full gap inventory, and state
      explicitly that §20.4.10 extends §20.4.9's own flat two-item list
      with an explicit scope boundary and ordering — not a renumbering
      of §20.4.9.
- [x] 1.2 Write the **11a. Export-side field aliasing** entry per
      design.md's own Decisions: scope
      (`try_materialize_field_extension`'s `export_node_id` call site
      gets `MAPL_NamedAlias` treatment mirroring `populate_one`'s
      existing import-side pattern), the fixture it is demonstrable
      against (`units_mismatch_run_phase`, already-landed, one-hop
      chain), what it unlocks (a real propagated run-phase value for
      the single-characteristic case — Phase 11's own stated goal, for
      that case), and its dependency (landed
      `graph-child-state-population` D1-D6/D7 only — no dependency on
      11b).
- [x] 1.3 Write the **11b. Multi-hop extension-chain intermediate
      materialization** entry per design.md's own Decisions: scope
      (`find_or_build_extension_chain`'s own public interface changes
      to expose every intermediate `NodeId`, each call site
      materializes each intermediate node with its own
      correctly-evolved typekind/units/geom), the four existing
      `GraphBuilder.F90` call sites that need updating (named
      explicitly, not "the obvious one"), what it unlocks (any
      two-or-more-characteristic chain's `compute()` no longer fails
      with `esmf_field is not allocated`), and its dependency (landed
      Phase 1-5/10 work only — no dependency on 11a).
- [x] 1.4 Write the "11a and 11b are independent, but 11a is the
      recommended first pick" paragraph per design.md's own Decisions:
      state explicitly that neither sub-change's correctness depends on
      the other, and that the recommendation is about payoff order (11a
      alone already completes Phase 11's own goal for the
      single-characteristic case) not a technical gate.
- [x] 1.5 Write a short paragraph confirming neither sub-change reopens
      Phase 10d's own double-invocation hazard, matching design.md's own
      "state-materialization/aliasing concerns, not invocation-count
      concerns" framing.
- [x] 1.6 Write a repo/tooling note paralleling
      §20.4.1/§20.4.3/§20.4.4/§20.4.8's own notes: Phase 11 code lives in
      the MAPL repo/checkout, same as Phase 3-5/10 (no repo-separation
      saving applies — both sub-changes build on landed Phase 1-5/10
      MAPL-repo code).

## 2. Insert into the roadmap document

- [x] 2.1 Insert the completed §20.4.10 section into
      `docs/graph/spec/20-implementation-roadmap.md` immediately after
      the existing §20.4.9 (Phase 11) and before §20.5 (if present) or
      the document's own next top-level section.
- [x] 2.2 Confirm the new section's own heading number (`20.4.10`) does
      not collide with any existing subsection number in the document
      before inserting (re-check against the live document, not just
      this change's own planning-time assumption).
- [x] 2.3 Confirm §20.4.9's own existing text (the two-gap inventory) is
      left unmodified except for whatever minimal cross-reference, if
      any, points a reader forward to §20.4.10 — do not rewrite or
      duplicate §20.4.9's own gap descriptions inside §20.4.10 beyond
      what is needed to name each sub-change's own scope.

## 3. Verify

- [x] 3.1 Re-read the edited `docs/graph/spec/20-implementation-roadmap.md`
      end to end to confirm section numbering, cross-references, and
      Markdown heading levels are consistent with the rest of the
      document (matching §20.4.1/§20.4.2/§20.4.3/§20.4.4/§20.4.5/§20.4.8's
      existing style).
- [x] 3.2 Confirm no other section of the roadmap (or any other spec
      file under `docs/graph/spec/`) was modified — this change is
      scoped to the single new §20.4.10 section (plus task 2.3's minimal
      cross-reference, if added).
- [x] 3.3 Confirm the new section does not assert anything beyond what
      design.md's own Decisions actually settled (no new technical
      claims introduced only at write-up time) — in particular, confirm
      every `GraphBuilder.F90`/`ExtensionResolution.F90` line reference
      used in the new section matches the live source file, not a
      stale line number from design.md's own drafting pass.
- [x] 3.4 Confirm the new section does not silently reopen or redesign
      Phase 10d's double-invocation hazard, 3b2, or Phase 7c — none of
      the three should appear as anything other than an explicit,
      unaffected cross-reference, matching design.md's own Non-Goals.
