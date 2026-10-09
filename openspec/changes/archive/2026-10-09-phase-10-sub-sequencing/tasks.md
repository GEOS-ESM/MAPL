## 1. Draft §20.4.8 section content

- [x] 1.1 Write the §20.4.8 "Phase 10 sub-sequencing" heading and intro
      paragraph: state the same "does not fit a single spec-driven
      change proposal" rationale as §20.4.1/§20.4.3/§20.4.4, reference
      proposal.md - Why for the full gap inventory, and state explicitly
      that this phase is a prerequisite for §20.4.2's own entry
      criterion, not a renumbering of it.
- [x] 1.2 Write the **10a. State materialization + 3-mode toggle** entry
      per design.md's own Decisions: scope (ordinary-item
      materialization at `GENERIC_INIT_ACCEPT_TRANSFER`, a new
      graph-side `add_to_states` equivalent, the `registry`/`graph`/
      `both` toggle), the "no `ESMF_State` name collision by
      construction" semantics, and its dependency (Phase 1-3 only).
- [x] 1.3 Write the **10b. Generalized equivalence harness, "both"
      mode, flat fixtures** entry per design.md's own Decisions: scope
      (parameterized harness generalizing
      `Test_GraphBuilderEquivalence.pf`'s existing pattern), the new
      clean-fixture-naming note (not reusing `scenario_1`/`scenario_2`
      style), the orthogonal-capability fixture list (plain field,
      bundle/vector, 3D field, ungridded dims), and its dependency
      (10a).
- [x] 1.4 Write the **10c. `TypeKindCharacteristic` + real precision-cast
      transform** entry per design.md's own Decisions: scope (wiring
      `TYPE_KIND_CHARACTERISTIC_ID` into `build_characteristics()`,
      a real `CopyTransform`-successor implementation, one new
      two-characteristic fixture with hand-checkable values), the
      explicit note that `build_chain`'s own loop needs no change
      (confirmed by reading `ExtensionResolution.F90`), the relationship
      to the existing `invalidate/` fixture (replaces its structural
      half with clearer naming, does not reuse it), and its dependency
      (10a/10b).
- [x] 1.5 Write the **10d. Ordinary-phase `MethodGraphNode` construction
      + real run-loop wiring** entry per design.md's own Decisions:
      scope (one `MethodGraphNode` per component wrapping
      `GENERIC_RUN_USER`, reusing the already-landed
      `GridCompDriverPhaseInvoker`, following
      `build_callback_method_binding`'s own established construction
      pattern), the new run-loop call site (likely `run_user.F90`,
      mirroring `gb%run_connect_hook`'s existing precedent at
      `ACCEPT_TRANSFER`), the explicit "sequenced strictly after
      10a-10c, new mechanism not a gap-closure" framing, and its
      dependency (10a-10c, plus landed Phase 4a/4b/2).
- [x] 1.6 Write the three-mode toggle semantics as a standalone
      paragraph or table (per design.md's own "no name collision by
      construction" decision), covering `registry`/`graph`/`both` and
      explicitly stating `graph` mode is not attempted by any
      10a-10d sub-change.
- [x] 1.7 Write the explicit cross-references to still-unfiled 3b2
      (cross-component unresolved-import propagation,
      `statistics`/`history_1`/`extdata_1`-shaped fixtures) and the
      existing §20.4.5 Phase 7c (wildcard/service pattern-based
      fan-in) as named, out-of-scope prerequisites for fixture
      families this phase's own sub-changes deliberately do not
      attempt — not reopened or redesigned here.
- [x] 1.8 Write a repo/tooling note paralleling §20.4.1/§20.4.3/§20.4.4's
      own notes: Phase 10 code lives in the MAPL repo/checkout, same as
      Phase 3-5 (no repo-separation saving applies — it builds on
      landed Phase 1-5 MAPL-repo code throughout).

## 2. Insert into the roadmap document

- [x] 2.1 Insert the completed §20.4.8 section into
      `docs/graph/spec/20-implementation-roadmap.md` immediately after
      the existing §20.4.5 (Phase 7 sub-sequencing) and before §20.5
      (if present) or the document's own next top-level section.
- [x] 2.2 Add a one-line cross-reference from §20.4's own Phase 6
      bullet (legacy retirement) to the new §20.4.8, stating that
      §20.4.8 is this entry criterion's own validation suite — matching
      how the Phase 3/4/5 bullets already reference their own
      §20.4.1/§20.4.3/§20.4.4 sub-sequencing sections.
- [x] 2.3 Confirm the new section's own heading number (`20.4.8`) does
      not collide with any existing subsection number in the document
      before inserting (re-check against the live document, not just
      this change's own planning-time assumption).

## 3. Verify

- [x] 3.1 Re-read the edited `docs/graph/spec/20-implementation-roadmap.md`
      end to end to confirm section numbering, cross-references, and
      Markdown heading levels are consistent with the rest of the
      document (matching §20.4.1/§20.4.2/§20.4.3/§20.4.4/§20.4.5's
      existing style).
- [x] 3.2 Confirm no other section of the roadmap (or any other spec
      file under `docs/graph/spec/`) was modified — this change is
      scoped to the single new section plus the one-line §20.4
      cross-reference update (task 2.2).
- [x] 3.3 Confirm the new section does not assert anything beyond what
      design.md's own Decisions actually settled (no new technical
      claims introduced only at write-up time).
- [x] 3.4 Confirm the new section does not silently reopen or redesign
      3b2 or Phase 7c — both must appear only as named, cross-referenced
      prerequisites, exactly as design.md's own Non-Goals state.
