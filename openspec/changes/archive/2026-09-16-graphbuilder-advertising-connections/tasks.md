## 1. GraphBuilder module scaffolding

- [x] 1.1 Create `mapl_GraphBuilder_mod` as a single self-contained file
      (`superstructure/generic/GraphBuilder.F90`, top-level alongside
      `OuterMetaComponent.F90` rather than under `graph/` — resolves
      design.md's Open Question: `GraphBuilder` depends upward on
      `OuterMetaComponent`, so placing it inside `graph/` would blur that
      directory's deliberate REQ-CG-002 graph-neutral isolation), with a
      module-procedure interface (not a persistent type) per design.md
      Decisions.
- [x] 1.2 Add build-system wiring: `GraphBuilder.F90` added to
      `superstructure/generic/CMakeLists.txt`'s top-level `srcs` list
      (plain module, not a submodule set, so no
      `esma_add_fortran_submodules` entry needed).

## 2. Advertising: StateItemNode creation

- [x] 2.1 Implemented `graphbuilder_advertise(this, rc)`: enumerates
      `this%get_component_spec()%var_specs` and registers one
      `StateItemNode` per item via `register_node`, skipping items whose
      identity is already indexed (idempotent re-advertisement).
- [x] 2.2 Lookup implemented via `ComponentGraph`'s existing
      `add_resource_index`/`get_resource_index` (REQ-CG-001 "semantic
      resource indexes" — no new storage type), keyed by the public
      `item_key(state_intent, short_name)` function.
- [x] 2.3 Wired into `initialize_advertise.F90` via
      `graphbuilder_run_advertise_hook(this)`, called right after
      `self_advertise(this, _RC)` — an additional call whose own failures
      are caught/logged internally (`report_if_failed`) and never
      propagate into `initialize_advertise`'s own error path.
      **Post-implementation correction (see task 9)**: also added
      `graphbuilder_run_activate_hook(this)`, called right after
      `process_connections(this, _RC)` in the same file — this is the
      read-only, activate()-time unresolved-imports check, distinct from
      real connection resolution (task 3.4).

## 3. Ordinary connection resolution

- [x] 3.1 Implemented `graphbuilder_resolve_connections`, restricted to
      `class is (MatchConnection)` entries in `component_spec%connections`
      — reads `ComponentSpec%var_specs` directly (per component); does not
      call `Connection%activate()`/`connect()` or the subregistry/family
      machinery. **Scope note discovered during task 7**: this reads each
      component's own `var_specs` only, not `StateRegistry`'s cross-level
      `propagate_exports`/`propagate_unsatisfied_imports` view — see
      proposal.md "Also out of scope, discovered during implementation".
- [x] 3.2 Matched (export, import) pairs get a dependency edge added to
      the *calling* component's own default network (not necessarily
      "the consuming component's" network directly — see task 4.2: when
      either side is a child, a parent-local proxy stands in for it, and
      the edge connects two of the parent's own node ids).
- [x] 3.3 Unresolved imports collected into an optional `StringVector`
      output parameter (`unresolved_imports`) plus a logger warning per
      entry from the lifecycle-hook wrapper; wildcard/callback/mismatch
      connections are never entered into this loop at all (different
      `Connection` subtype).
- [x] 3.4 **Corrected mid-implementation (task 9)**: wired into
      `initialize_accept_transfer.F90` (NOT `initialize_advertise.F90` as
      originally implemented), via `graphbuilder_run_connect_hook(this)`,
      called right after `process_connections(this, _RC)` there — the
      actual point `Connection%connect()` (real wiring) runs today, per
      user correction (`initialize_advertise.F90`'s `process_connections`
      only calls `activate()`, which does not form real connections).
      Additional call, failures caught/logged internally, never alters
      existing behavior.

## 4. Public ports and child proxies

- [x] 4.1 Every advertised import/export item (not internal) is treated as
      a declared public port (no separate marker exists in
      `VariableSpec`/`ComponentSpec` today — documented interpretive
      decision in `GraphBuilder.F90`'s header/`advertise_one` comments);
      `add_import_port`/`add_export_port` called with the item's `NodeId`
      in `advertise_one`.
- [x] 4.2 `get_or_make_local_node_id` creates/reuses a parent-local proxy
      `StateItemNode` (cached via `proxy_key`, same resource-index
      mechanism) for any connection endpoint naming a child, registered
      via `add_child_port_binding`; the child's own native `NodeId` is
      used only as a read-only existence check (`get_resource_index`),
      never returned or stored as the resolved identity — verified in
      task 6.4/`test_ordinary_connection_wires_matching_and_reports_unresolved`.

## 5. Validate and freeze

- [x] 5.1/5.2 Implemented `graphbuilder_freeze(this, rc)` — thin wrapper
      around `ComponentGraph%freeze()`, which already validates before
      transitioning lifecycle state (Phase 1-2 behavior, reused as-is).
      Called from `graphbuilder_run_connect_hook` after connection
      resolution; failure is caught/logged, not swallowed silently
      (`report_if_failed`), and does not freeze the graph.
      **Post-implementation correction (task 9)**: this hook (and
      therefore freeze) now runs at `initialize_accept_transfer.F90`
      time, not advertise time — freezing before real edges exist would
      have been premature.

## 6. pFUnit coverage

- [x] 6.1 `test_advertise_creates_state_item_nodes`
      (`superstructure/generic/tests/Test_GraphBuilder.pf`).
- [x] 6.2 `test_ordinary_connection_wires_matching_and_reports_unresolved`.
- [x] 6.3 `test_non_matchconnection_is_skipped` (uses `SimpleConnection`).
- [x] 6.4 `test_public_port_recorded_and_reachable_by_name` +
      the proxy assertions in
      `test_ordinary_connection_wires_matching_and_reports_unresolved`.
- [x] 6.5 `test_freeze_succeeds_when_valid` +
      `test_freeze_fails_and_leaves_graph_unfrozen_on_conflict`.
      All 6 registered in `MAPL.generic.components`
      (`superstructure/generic/tests/CMakeLists.txt`); full suite passes
      (32/32). **Task 9 added a 7th case**,
      `test_check_unsatisfied_imports_reports_without_mutating_graph`;
      suite now passes 33/33.

## 7. Equivalence verification (Q10)

- [x] 7.1 **Deviation from literal task text, discovered during this
      task**: every existing real `connect_all` scenario fixture
      (`statistics`, `statistics_real`, `history_1`, `history_wildcard`,
      `extdata_1`) connects components whose real items live on a further
      *grandchild*, visible only via `StateRegistry` propagation —
      `GraphBuilder` doesn't consult that view (see proposal.md/design.md
      "discovered during implementation" notes). None were directly
      usable. Added a new, real (DSO-backed, full ESMF Initialize
      lifecycle), deliberately *flat* fixture instead:
      `superstructure/generic/tests/scenarios/graphbuilder_equivalence/`
      (`cap.yaml`/`comp_src.yaml`/`comp_dst.yaml`, two direct siblings
      each declaring their own export/import of `T`, connected via
      `all_unsatisfied: true` — the exact case `GraphBuilder` is scoped to
      handle).
- [x] 7.2 `superstructure/generic/tests/Test_GraphBuilderEquivalence.pf`
      (`MAPL.generic.scenarios`): runs the real init-phase sequence, then
      checks (a) the legacy-coupler oracle — `comp_dst`'s real ESMF import
      state actually contains field `T` — and (b) `GraphBuilder`'s own
      graph shows a dependency edge between the `comp_src`/`comp_dst`
      proxies for the same item.
- [x] 7.3 No divergence found for the flat case; the propagation gap
      (7.1) is the one real divergence identified and is documented as an
      explicit scope boundary, not silently patched over.
- [x] 7.4 Registered as a permanent `MAPL.generic.scenarios` pFUnit case
      (not a throwaway script) — passes under `ctest` (239/239 tests in
      that suite, including this one).

## 8. Verification

- [x] 8.1 Built MAPL with NAG (`nag` build dir, `nag-stack` module).
      Fixed one pre-existing, unrelated build break found blocking the
      build entirely: `infrastructure/geom/LatLon/LatLonGeomSpec.F90:24`
      had two `procedure ::` statements merged onto one line (missing
      newline) — a corruption from an earlier commit, unrelated to this
      change; split back into two statements. After that fix, full
      project build (`cmake --build nag -j 8`) and `build-tests` both
      succeed cleanly. Full `ctest` (75 tests): 68 passed, 7 failed — all
      7 the same pre-existing, unrelated environment gaps 3a's tasks.md
      §5.1 already documented (missing `netCDF4` Python package for
      `MAPL3G_Comp_Test_case02/11/23`; missing
      `LOCAL_REGRESSION_DATA_DIR` for `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs`).
      No regression in any `MAPL.generic.*` suite or elsewhere.
- [x] 8.2 Ran `MAPL.generic.components` (`Test_GraphBuilder.pf`, 6 new
      cases) and `MAPL.generic.scenarios` (`Test_GraphBuilderEquivalence.pf`,
      1 new case) directly with `-v`: all pass (32/32 and 239/239 in
      those suites respectively). Also ran the full `MAPL.generic.*`
      label set (graph, scenarios, transforms, vertical, aspects,
      components, core) via `ctest` — all pass.
- [x] 8.3 `openspec validate graphbuilder-advertising-connections --strict`
      reports the change valid.

## 9. Two-phase timing correction (post-implementation, user-identified)

After task 8's initial verification pass, review identified that
`initialize_advertise.F90`'s `process_connections()` only calls
`Connection%activate()` — which does NOT form real connections, only
marks which imports/exports are active (used to decide (a) whether an
unsatisfied import must bubble to the parent and (b) whether an export
needs to be allocated at all). The real wiring only happens later, via
`Connection%connect()`, called from `initialize_accept_transfer.F90`'s
own `process_connections()`. The original implementation (tasks 3.2-3.4,
5.1-5.2) incorrectly performed real graph mutation (proxies + edges +
freeze) at `initialize_advertise` time. Corrected as follows:

- [x] 9.1 Split `GraphBuilder`'s connection-side API into two: added
      `graphbuilder_check_unsatisfied_imports()` (activate()-time analog
      — read-only, no graph mutation, populates the unresolved-imports
      report only) alongside the existing `graphbuilder_resolve_connections()`
      (connect()-time analog — real proxies + edges, unchanged
      internally). Added `graphbuilder_run_activate_hook()` and
      repurposed `graphbuilder_run_connect_hook()` accordingly.
- [x] 9.2 Moved the real-resolution/freeze hook call from
      `initialize_advertise.F90` to `initialize_accept_transfer.F90`
      (alongside its real `process_connections()`/`connect()` call);
      added the new activate-time hook call to `initialize_advertise.F90`
      in place of the old (incorrect) connect-hook call.
- [x] 9.3 **Second issue found via the real end-to-end equivalence test**
      (task 7's fixture, plus previously-passing real scenarios like
      `extdata_1`, started failing after 9.2): ESMF's Provide/Accept/
      Realize transfer negotiation invokes `GENERIC_INIT_ACCEPT_TRANSFER`
      more than once per component within one `Initialize` sequence,
      causing a second `graphbuilder_resolve_connections()` call to
      attempt `add_dependency` on an already-frozen graph. Fixed by
      making `graphbuilder_resolve_connections()` idempotent: returns
      immediately (success, no-op) if the component's graph is already
      frozen — mirroring the `consumed`-flag idempotency legacy's own
      `MatchConnection%connect()`/`SimpleConnection%connect()` already
      rely on.
- [x] 9.4 Added `test_check_unsatisfied_imports_reports_without_mutating_graph`
      (task 6.5's suite) verifying the activate-time check reports the
      same unresolved import as real resolution but creates no proxy
      nodes and does not touch node count. Re-ran full verification
      (task 8's build/ctest/validate) after the fix — all green, same
      pre-existing 7 unrelated failures, no new regressions
      (`MAPL.generic.components` 33/33, `MAPL.generic.scenarios` 239/239).
- [x] 9.5 Updated proposal.md/design.md/specs/graph/graph-builder/spec.md
      to document the two-phase timing as an explicit behavioral
      contract (not just an implementation detail) and the idempotency
      requirement.
