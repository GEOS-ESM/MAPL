## 1. Draft §20.4.11 (Phase 6 sub-sequencing) section content

- [x] 1.1 Write the §20.4.11 heading and intro paragraph: state the same
      "does not fit a single spec-driven change" rationale as
      §20.4.1/§20.4.3/§20.4.4/§20.4.8/§20.4.10, and state explicitly that
      this section replaces §20.4.2's own single-sentence "(a) retire...
      once validated" with a named, dependency-ordered prerequisite set
      and three concrete sub-steps — it does not renumber or restate
      §20.4.2's own existing "Purpose"/"Entry criterion" text, which
      stays in place.
- [x] 1.2 Write the **prerequisite set** per design.md's own Decisions:
      3b2, Phase 7a, Phase 7b, Phase 7c (narrowed, cross-reference
      §20.4.5's edit from task 3), Phase 12, Phase 13 — each with the
      one- or two-sentence "why this blocks Phase 6a" rationale
      design.md already worked out, not merely a bullet list of names.
- [x] 1.3 Write the **6a-1. Full-fixture `GRAPH_MODE_BOTH` parity run**
      entry: scope (extend `graph/equivalence-harness`'s parameterized
      suite to every fixture in `scenarios/` and `graph-scenarios/`, not
      only Phase 10b's own flat subset), exit criterion (every fixture
      passes under `GRAPH_MODE_BOTH`), and explicit note that this
      cannot be attempted honestly until the prerequisite set (task 1.2)
      has landed.
- [x] 1.4 Write the **6a-2. Flip the default mode; retire the toggle**
      entry: scope (change `mapl_GraphMode_mod`'s default, begin
      removing the three-way selector), and the explicit gate on Phase
      10d's own double-invocation hazard being separately resolved
      first (not assumed solved by this document).
- [x] 1.5 Write the **6a-3. Delete legacy source** entry: list the
      concrete directories/files to remove
      (`superstructure/generic/registry/`, `specs/*Aspect*.F90` +
      `AspectId.F90`, `ExtensionFamily`, `connection/*.F90`, the
      coupler-GridComp mechanism), gated on 6a-2's own burn-in period.
- [x] 1.6 Write the **Phase 7c scope narrowing — Service removed**
      paragraph per design.md's own Decisions: explicit user direction,
      Service retired outright via callbacks (not a structural analog),
      narrows 7c to Wildcard-only.
- [x] 1.7 Append the seven new Phase 6b growable-list entries per
      design.md's own Decisions (`write_formatted`, `ModelVerticalGrid`'s
      second `extend()` call site, `gridcomp_set_geometry`'s bypass, the
      dead `initialize_modify_advertised.F90` lines, the coupler-lifecycle
      call sites, Service's audit-and-delete, `MAPL_GridCompGetRegistry`'s
      compatibility check) — each with its own file:line citation from
      design.md, and confirm the existing three entries (lines ~327-374)
      are left byte-for-byte unmodified.

## 2. Draft §20.4.12 (Phase 12 scope) and §20.4.13 (Phase 13 scope) sections

- [x] 2.1 Write §20.4.12 per design.md's own Decisions: discovered-gap
      framing (matching §20.4.6/§20.4.7's own precedent), scope (wire
      5a2's seven subclasses into `build_characteristics()`), explicit
      note that real `build_transform` adaptation is not required for
      every axis (detection-wiring only; mismatches with no adaptation
      report explicit failure, matching legacy), and dependency (5a/5a2
      only — independent of 3b2/Phase 7/Phase 13).
- [x] 2.2 Write §20.4.13 per design.md's own Decisions: discovered-gap
      framing, scope (decide how `RestartMode` metadata reaches a
      graph-materialized field, likely via `VariableSpec%restart_mode`),
      exit criterion (a real read/write round-trip test against a
      graph-materialized field, not merely a metadata-presence check),
      and dependency (Phase 10a only — independent of 3b2/Phase 7/Phase
      12).

## 3. Edit §20.4.2 and §20.4.5

- [x] 3.1 Append the seven new growable-list entries (drafted in task
      1.7) to §20.4.2's existing list, immediately after the current
      third entry (the `RouteHandleKey`/`RoutehandleParam` reconciliation
      item, roadmap lines ~366-374) — do not reorder or rewrite the
      existing three entries.
- [x] 3.2 Add the one-paragraph Service-removal cross-reference (drafted
      in task 1.6) to §20.4.5's existing Phase 7c bullet (roadmap lines
      ~902-928), placed so it reads as a disposition update, not a
      rewrite of the existing Wildcard/Service joint-problem description.

## 4. Insert into the roadmap document

- [x] 4.1 Insert the completed §20.4.11 section into
      `docs/graph/spec/20-implementation-roadmap.md` immediately after
      the existing §20.4.10 (Phase 11) and before §20.5, or the
      document's own next top-level section.
- [x] 4.2 Insert §20.4.12 immediately after §20.4.11, and §20.4.13
      immediately after §20.4.12 — confirm these three new subsection
      numbers do not collide with any existing subsection number in the
      live document before inserting.
- [x] 4.3 Apply the §20.4.2 and §20.4.5 edits from tasks 3.1/3.2 at
      their respective existing locations in the live document.
- [x] 4.4 Confirm §20.4.9's own Phase 11 gap inventory, and every other
      existing section's text, is left unmodified except for the two
      explicitly-scoped edits in task 4.3.

## 5. Verify

- [x] 5.1 Re-read the edited `docs/graph/spec/20-implementation-roadmap.md`
      end to end to confirm section numbering, cross-references, and
      Markdown heading levels are consistent with the rest of the
      document (matching the existing sub-sequencing sections' own
      style).
- [x] 5.2 Confirm every file:line reference used in the three new
      sections and two edits matches the live source file, not a stale
      line number from design.md's own drafting pass (re-check each
      citation against the current repo state, the same discipline
      §20.4.10's own task 3.3 followed).
- [x] 5.3 Confirm no other section of the roadmap, and no file under
      `docs/graph/spec/` besides this one, was modified.
- [x] 5.4 Confirm the new sections do not assert anything beyond what
      design.md's own Decisions actually settled, and do not silently
      resolve 3b2, Phase 7a/7b/7c, Phase 10d's double-invocation hazard,
      or any other cross-referenced gap — each should appear only as an
      explicit, unaffected or named-prerequisite cross-reference,
      matching design.md's own Non-Goals.
- [x] 5.5 Run `openspec validate --change phase-6-sub-sequencing` (or
      the project's equivalent check) to confirm `skip_specs: true` is
      honored and the change has no unexpected validation errors before
      considering this change ready to apply.
