## 1. Shared storage foundation

- [x] 1.1 Add `HierarchyId` (identity type, generated like `NodeId`),
      `HierarchyGraph` (the hierarchy-wide `NodeId → GraphNode` and
      `DependencyNetworkId → DependencyNetwork` maps plus the one
      shared `NodeIdGenerator`/`DependencyNetworkIdGenerator` pair),
      `HierarchyResources` (container holding one `HierarchyGraph`,
      shaped to hold future shared resources later), and a private,
      module-level `HierarchyId → HierarchyResources` registry with
      `request_new_hierarchy()`/`get_hierarchy_resources(id)`/
      `release_hierarchy(id)` entry points (design.md D1).
- [x] 1.2 Give `OuterMetaComponent` a `HierarchyId` field (a plain
      value, not a pointer). The root construction path calls
      `request_new_hierarchy()` once; `add_child_by_spec` passes
      `this`'s own `hierarchy_id` down to each child's construction
      (design.md D1) — no module-level bare singleton. Add one new
      optional, framework-internal `hierarchy_id` keyword argument
      through `mapl_GridCompCreate`/`GridCompCreate`/
      `create_grid_comp_primary` (absent by default; every existing
      call site is source-compatible and behaviorally unchanged) so
      `add_child_by_spec`'s own internal call for a child declaring its
      own `user_setservices` can also stay in the parent's hierarchy
      instead of starting a new one.
- [x] 1.3 Change `ComponentGraph` to hold a `HierarchyId` instead of
      its own private node/network maps and generators; resolve
      `get_hierarchy_resources(this%hierarchy_id)` internally wherever
      the shared maps/generators are needed. Keep resource index,
      ports, child-port bindings, port generator, and lifecycle state
      exactly as component-local as today (per `graph/component-graph`
      delta spec). A no-argument `ComponentGraph()` call requests its
      own fresh `HierarchyId` internally, so existing synthetic-graph
      test call sites are unchanged.
- [x] 1.4 Update node/network registration call sites
      (`register_node`, `create_network`, etc.) to write into the
      resolved `HierarchyGraph` and record the id into this instance's
      own explicit `NodeIdSet`/`DependencyNetworkIdSet` ownership sets
      (D2) at the same point.
- [x] 1.5 Update `ComponentGraph` validation ("owned nodes are valid")
      to consult the explicit per-instance ownership sets instead of
      per-instance map membership (`graph/component-graph` delta spec,
      "Ownership is explicit, not inferred from shared-map
      membership").
- [x] 1.6 Run the full existing synthetic-graph pFUnit suite (Phase 1-2
      scope, no `OuterComponent`/ESMF involved) against the refactored
      storage with no behavior change expected; fix any test that
      assumed per-instance map isolation.

## 2. Component-name uniqueness enforcement

- [x] 2.1 Add a hierarchy-wide name registry (`HierarchyResources%
      component_names`, a `StringSet`) consulted when a child component
      is added.
- [x] 2.2 Reject child-add with a clear error when the new child's name
      already exists anywhere in the hierarchy, not only among its own
      siblings (`graph/component-hierarchy` delta spec, "Component
      names are unique across the entire hierarchy").
- [x] 2.3 Add test coverage for both scenarios in that requirement:
      sibling-level collision (already covered implicitly today) and
      cross-branch/cousin collision (new).

## 3. Remove proxy mechanisms: ordinary connections (3a/3b)

- [x] 3.1 Remove `proxy_key` and `get_or_make_local_node_id`'s
      cross-graph proxy-creation branch from
      `GraphBuilder_ConnectionResolution_smod.F90`.
- [x] 3.2 Change ordinary connection resolution to add a dependency
      edge directly between the two real `NodeId`s found via each
      side's own owning component's local resource index (design.md
      D5), using `resolve_qualified_component`'s existing tree walk
      unmodified to find each owning component.
- [x] 3.3 Add the connection-endpoint-resolution-direction check
      (`graph/component-hierarchy` delta spec, "Connection endpoint
      resolution never resolves upward past its declaring component")
      at the point a connection's declared component name is resolved;
      reject resolution to anything outside the declaring component's
      own subtree.
- [x] 3.4 Update/retire the existing proxy-focused test coverage in
      `Test_GraphBuilder.pf` to assert direct-edge wiring instead;
      confirm `Test_GraphBuilderEquivalence.pf`'s flat-fixture parity
      checks still pass unchanged.

## 4. Remove proxy mechanisms: geometry push/pull (4e)

- [x] 4.1 Remove `parent_geometry_proxy_key` and `push_geometry_to_child`
      from `GraphBuilder_Geometry_smod.F90`.
- [x] 4.2 Change geometry pull/push resolution to add a dependency edge
      directly between the parent's and child's own real geometry
      nodes.
- [x] 4.3 Update `Test_GraphGeometryHook.pf` and the real-lifecycle
      geometry scenario fixtures to assert direct-edge wiring.

## 5. Remove proxy mechanisms: callback wiring (4c/4d)

- [x] 5.1 Remove the proxy-creation half of
      `resolve_qualified_export_node_id` in
      `GraphBuilder_CallbackWiring_smod.F90`, keeping
      `build_qualified_export_namespace`'s own namespace-collection walk
      unmodified (it already only computes names, not proxies).
- [x] 5.2 Change `materialize_callback_collection` to store each
      match's own real `NodeId` as the collection member directly (per
      `graph/callback-wiring` delta spec).
- [x] 5.3 Update `build_callback_state_binding` to bind directly against
      the real member `NodeId` with no owner-component proxy
      indirection.
- [x] 5.4 Update callback-wiring test coverage to assert direct member
      references.

## 6. Replace 3b2 with recursive import mirroring (revised — see design.md D3)

**Design correction, before any code was written against the prior
scope:** the original wording of this group ("implement subtree
search... search D's own subtree for an unresolved import") was wrong
on two counts — it required a connection to trigger (legacy's own
propagation is unconditional, independent of any connection naming the
propagating component) and it never materialized anything at the
intermediate level, so it could not be found by the existing,
unmodified `for_each_matching_import` single-level scan every other
connection path already relies on. See design.md D3 for the corrected
design and the reasoning. Tasks below reflect the corrected scope,
which is smaller than originally drafted: one new hook, reusing
existing procedures unmodified, with no new resolution logic, no
search, and no composite/member structure.

- [x] 6.1 Add `graphbuilder_run_propagate_hook(this)`: for each direct
      child C of `this` (unconditionally — no connection needs to name
      C), call the EXISTING, UNMODIFIED
      `graphbuilder_check_unsatisfied_imports(C, ...)` on C's behalf
      (same framework-internal "parent reaches into child" carve-out
      `get_child_component_graph`/`get_child_component_spec` already
      document) and, for each entry it reports unresolved, look up
      that entry's own real `NodeId` in C's own graph (`item_key`,
      unchanged) and call `this_graph%add_import_port(fresh_port_id,
      id)` — the same real `NodeId`, now additionally reachable through
      `this`'s own import-port table. No new node, no proxy, no
      composite wrapper (design.md D3).

      **Implementation correction (discovered during this task):**
      `graphbuilder_check_unsatisfied_imports` only ever inspects
      MatchConnections `this` itself declares — a child with zero
      connections of its own (exactly the motivating fixture's own
      `comp_mid`) would always report "nothing unresolved" from that
      procedure, not because its imports are resolved but because
      nothing was examined. This contradicts the unconditional-mirroring
      requirement (`graph/graph-builder` delta spec, "An unresolved
      import is mirrored into every ancestor's own import-port table,
      independent of any connection"). Implemented instead as a new,
      genuinely structural scan,
      `graphbuilder_unresolved_import_node_ids(this, rc)`
      (`GraphBuilder_ConnectionResolution_smod.F90`): walks `this`'s own
      full import-port table directly; an import counts as unresolved
      iff its `NodeId` has no predecessor in any of `this`'s own owned
      `DependencyNetwork`s — the structural "has anything actually
      written to this import" fact, true regardless of whether the
      resolving connection was declared on `this` itself, reached in
      from an ancestor, or was itself a previously-mirrored import.
      `graphbuilder_check_unsatisfied_imports` is unmodified and remains
      in independent use (task 6.6/7.1's own self-check caller); only
      propagation's own child-side query uses the new structural scan.

      **Separately required for this task:**
      `ComponentGraph%add_import_port` previously asserted
      `owns_node(node_id)`, rejecting the child's own real (foreign)
      `NodeId` that mirroring must insert — relaxed to the same
      "owned OR `allowed_foreign_node`" admission `add_dependency`/
      `bind_port` already use (D2 correction, Groups 3-5), since task
      6.1 is the first caller needing a foreign id as an import-port
      VALUE rather than only as a dependency-edge endpoint.
- [x] 6.2 Wire `graphbuilder_run_propagate_hook` into
      `initialize_advertise.F90`'s own recursive advertise sequence,
      immediately after `run_activate_hook` and before
      `this%registry%propagate_unsatisfied_imports(_RC)`.

      **Implementation correction (discovered during this task):** this
      task's own original wording placed the new call "before
      `process_connections`/`run_activate_hook`," claiming that
      position "mirrors legacy's own `propagate_unsatisfied_imports`
      call-site position... exactly" — that claim was checked against
      the actual file and found backwards: legacy's own call sits
      AFTER `process_connections` (activate()), not before. The
      ordering matters, not just cosmetically: legacy's `is_active()`
      check inside propagation only correctly suppresses an import
      resolved at the current level because that level's own
      `activate()` already ran moments earlier in the same pass. Placed
      before `run_activate_hook` instead, every import resolved by an
      ordinary connection declared at the current level would look
      unresolved at the moment propagation runs and get mirrored
      upward needlessly, recurring at every ancestor. Implemented with
      the call immediately after `run_activate_hook` (matching legacy's
      real position), not before it. `recurse()`'s own position is
      unchanged.

- [x] 6.3 Confirm, with a direct test, that once a child's import is
      mirrored into the parent's own import-port table,
      `for_each_matching_import`/`resolve_one` (unmodified) correctly
      resolve an ordinary connection declared at the parent naming that
      bubbled port — no new resolution code path should be needed; this
      task is confirmation, not implementation.

      **Confirmed** by
      `Test_GraphBuilderPropagation.pf::test_mirrored_import_resolves_via_unmodified_resolution`
      (three-level GRANDPARENT→MID→comp_dst fixture, MID declares no
      connection of its own; GRANDPARENT's own connection names MID as
      destination and resolves against the mirrored port with zero
      changes to `for_each_matching_import`/`resolve_one`). Passes
      under NAG (`ctest -R MAPL.generic.components`).
- [x] 6.4 Add test coverage for two or more distinct descendants
      bubbling an import of the same short name to one ancestor, each
      satisfied by the same connection, each independently taking its
      own matched/mismatched resolution branch (per the user's own
      stated requirement that MAPL3, unlike MAPL2, allows bubbled items
      of the same name to carry different specs) — confirming this
      falls out of `for_each_matching_import`'s existing multi-match
      iteration and `resolve_one`'s existing per-match mismatch
      handling with no new code, per design.md D3.

      **Confirmed** by
      `Test_GraphBuilderPropagation.pf::test_multiple_descendants_bubble_same_name`
      (child_a and child_b, same short name 'T', both mirrored into MID
      and both independently resolved against the same SRC export, two
      distinct dependency edges in GRANDPARENT's own default network).
      Passes under NAG.
- [x] 6.5 Resurrect and adapt the abandoned
      `scenarios/graphbuilder_propagation/` fixture (cap →
      comp_mid → comp_dst, comp_mid declares no connection of its own)
      as a real pFUnit scenario test exercising the corrected recursive
      mirroring end-to-end.

      **Implemented** as
      `superstructure/generic/tests/graph-scenarios/graphbuilder_propagation/`
      (cap.yaml/comp_src.yaml/comp_mid.yaml/comp_dst.yaml,
      description.md) plus
      `Test_GraphModeDemonstration.pf::test_graph_mode_mirrors_grandchild_import_recursively`,
      run through the real `GENERIC_INIT_PHASE_SEQUENCE` (not the
      direct, synthetic `gb%advertise`/`gb%run_propagate_hook` calls
      `Test_GraphBuilderPropagation.pf` uses). cap's own connection
      names `comp_mid` — not `comp_dst` — as its destination, exactly
      the shape design.md D3's postmortem says a rejected
      "search the subtree" alternative could not reproduce. Asserts:
      comp_mid's own resource index has no entry for 'T' (never
      advertised directly); comp_dst's own import NodeId is mirrored
      into comp_mid's own import-port table; cap's own default network
      wires comp_src's real export NodeId directly to comp_dst's real
      import NodeId, with no proxy at any level. Task 6.7's own real-
      `ESMF_State` population gap is explicitly out of scope and not
      exercised by this fixture (description.md says so explicitly).

      **Correction (this task's own first-pass completion note was
      WRONG, not merely incomplete):** the first attempt at this task
      claimed the `MAPL.generic.scenarios` ctest target's own 48
      failing cases (`statistics`/`statistics_real`/`history_1`/
      `history_wildcard`/`extdata_1`) were "pre-existing and
      unrelated," reproducing identically with 6.5's own fixture/test
      removed. That check was performed by reverting only the NEW
      test *file*, not the production `GraphBuilder*.F90`/
      `initialize_advertise.F90` diff task 6.1/6.2 had already landed
      (uncommitted) — proving nothing about whether THAT diff was the
      regression's actual source. Rebuilding against true `HEAD`
      (`f629872a`, in an isolated `git worktree`, plus only the two
      unrelated NAG-portability one-liners already in ARCHITECTURE.md
      §3.2) showed `MAPL.generic.scenarios` passing 100%, zero
      failures — the 40-48 failures were a REAL regression introduced
      by this change's own task 6.1/6.2 import-propagation code, not
      pre-existing. Root-caused and fixed as new task 6.8 below; see
      that task for the full postmortem and the three distinct bugs
      found and fixed. With 6.8's own fixes applied,
      `MAPL.generic.scenarios` passes 100% (confirmed 3x) and the full
      top-level `ctest` suite shows only the 4 pre-existing, unrelated
      `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` app-regression failures (matching
      ARCHITECTURE.md §4.1's own prior documentation of those four
      specifically).
- [x] 6.6 Remove the now-superseded "Unsatisfied-import determination
      happens without graph mutation" two-phase check/resolve split
      (`graph/graph-builder` delta spec, REMOVED Requirements) —
      confirm ordinary resolution's existing idempotency covers repeat
      invocation with no separate pre-check entry point. NOTE: this
      task's own premise needs re-checking against the revised D3 —
      `graphbuilder_check_unsatisfied_imports` is now also a load-
      bearing dependency of propagation itself (task 6.1), not merely
      an optional pre-check callers may skip; confirm removing the
      *two-phase split this requirement originally described* does not
      require removing the procedure propagation now depends on.

      **Confirmed, no code or spec removal performed** — this task's
      own premise does not hold once checked against the actual delta
      spec and the revised D3 design:

      1. `openspec/changes/hierarchy-wide-component-graph/specs/graph/
         graph-builder/spec.md` has no `## REMOVED Requirements`
         section (confirmed by reading the file directly — it has only
         `## MODIFIED Requirements` and `## ADDED Requirements`). The
         "Unsatisfied-import determination happens without graph
         mutation" requirement is not touched by this change's delta
         spec at all, and remains, unmodified, in
         `openspec/specs/graph/graph-builder/spec.md` (lines 35-55).
         There is no REMOVED-Requirements text for this task to action.
      2. The two-phase check/resolve split itself is not superseded —
         it is exactly as load-bearing today as before Group 6:
         `graphbuilder_check_unsatisfied_imports` (the activate()-time,
         non-mutating check) is called from THREE real sites
         (`GraphBuilder_LifecycleHooks_smod.F90`:
         `graphbuilder_run_activate_hook`,
         `graphbuilder_run_connect_hook`'s own post-freeze convergence
         check) and is the procedure
         `graphbuilder_unresolved_import_node_ids`/
         `graphbuilder_run_propagate_hook` (task 6.1) is itself built
         on top of (not a replacement for it — see
         `GraphBuilder_ConnectionResolution_smod.F90`'s own header
         comment above `graphbuilder_unresolved_import_node_ids`,
         confirming it reuses `graphbuilder_check_unsatisfied_imports`'
         existing non-mutating contract rather than duplicating it).
         `graphbuilder_resolve_connections` (the connect()-time, real-
         mutation step) is unchanged and still the only mutating entry
         point. Removing either half, or the split between them, would
         break propagation itself, not merely remove unused code — the
         exact outcome this task's own note already warned against.
      3. `test_check_unsatisfied_imports_reports_without_mutating_graph`
         (`Test_GraphBuilder.pf`) already covers the non-mutation
         contract directly; `Test_GraphBuilderPropagation.pf`'s own
         test suite (6.1-6.4) confirms the determination/mirroring/
         resolution three steps agree with each other with no new
         resolution logic. No additional test was needed for this
         confirmation-only task.
- [ ] 6.7 **Follow-up, explicitly out of scope for this task group, not
      silently assumed solved (design.md D3's own "genuinely open"
      note):** `graphbuilder_populate_state` has no branch today for
      inserting a bubbled (no-backing-`VariableSpec`) import into the
      GenericGridComp's own real `ESMF_State`, under a child-qualified
      name (`comp_dst/T`) mirroring `FieldClassAspect%add_to_state`'s
      existing manual substate-nesting for the ordinary case. File as
      a separate, explicitly named follow-up rather than extending this
      group's own scope.
- [x] 6.8 **Export propagation (symmetric to import propagation),
      discovered as a REQUIRED completion of 6.1/6.2 — not optional
      polish — while verifying 6.5's own fixture against a real-
      lifecycle regression test run (see 6.5's own correction note
      above for how the regression was first found: a full
      `ctest -R MAPL.generic.scenarios` run on true `HEAD` plus task
      6.1/6.2's own uncommitted diff showed 40-48 new failures that do
      NOT reproduce on `HEAD` alone).**

      **Root cause:** wiring `graphbuilder_run_propagate_hook` into
      the real `GENERIC_INIT_ADVERTISE` lifecycle (task 6.2) exposes a
      child's own previously-invisible import-port entries to an
      ancestor's own ordinary connection — including, for
      `history_1`/`history_wildcard`/`statistics_real`'s own real
      fixture shape, an import named with an ESMF sub-state-qualified
      short name (`A/E_A1` — "field `E_A1` inside sub-state `A`" of
      the IMPORTING component's own state, per the user's own
      ESMF-semantics correction; nothing to do with the EXPORT side's
      own naming at all). Legacy's own `propagate_exports_virtual_pt`
      (`StateRegistry_Propagation_smod.F90`) makes ordinary name
      matching work for this shape by ALSO qualifying the EXPORT's own
      name with the immediate child's name as it bubbles up
      (`comp_name == ''` → qualify with the child; already qualified →
      pass through UNCHANGED at every further ancestor level — the
      user's own confirmed rule: "Export E from component A becomes
      export A/E in the parent component P, but stays A/E in the
      grandparent component"). Graph-native resolution
      (`find_export_node`) had no analogous export-side qualification
      step at all before this task — a real, previously-unscoped gap
      in Group 6, not a documentation/test-only item like 6.3-6.6.

      **Implemented, three distinct fixes, all required together:**
      1. **Export propagation itself**
         (`graphbuilder_run_export_propagate_hook`,
         `GraphBuilder_ConnectionResolution_smod.F90` + interface in
         `GraphBuilder.F90`; wired into `initialize_advertise.F90`
         next to legacy's own `registry%propagate_exports` call;
         unconditional per direct child, no "already resolved" gate,
         matching `propagate_exports_subregistry`'s own lack of one).
         New storage: `ComponentGraph`'s `GraphPortRegistry` gains a
         sparse `PortId -> qualified_name` side-map
         (`PortIdStringMap`, new gFTL instantiation,
         `containers/PortIdStringMap.F90`, `T __CHARACTER_DEFERRED`),
         populated only for a propagated export port — a directly-
         advertised export's own real `StateItemNode%short_name`
         identity field is NEVER qualified or rewritten.
         `add_export_port`'s own node-ownership assertion relaxed the
         same way task 6.1 already relaxed `add_import_port`'s (owned
         OR `allowed_foreign_node`) — a propagated export's own
         `node_id` is, by construction, a descendant's own real id.
      2. **`find_export_node`'s own read path** (ordinary resolution's
         ONLY consumer of export-port entries) changed to match
         against each port's own EFFECTIVE name
         (`ComponentGraph%get_export_port_name` — the propagation
         override when one exists, else the node's own real
         `short_name`), not the node's own `short_name` directly —
         the one-line change that actually makes resolution find a
         propagated export's own qualified name.
      3. **A second, independently-discovered bug** in
         `find_export_node_id` (same class of mistake task 6.3 already
         corrected on the import side): once `find_export_node` found
         the right node via the qualified name, this procedure
         SEPARATELY re-derived `export_node_id` via
         `resolve_local_node_id` — a resource-index lookup keyed by
         the plain `short_name`, which does not exist under the
         qualified name on the exporting component's own ancestor.
         Fixed identically to 6.3's own precedent: use
         `export_node%get_node_id()` directly (the already-found
         node's own real id), never re-derive it.
      4. **A third, independently-discovered bug**, NOT about export
         naming at all: `graphbuilder_unresolved_import_node_ids`
         (import propagation's own source scan) was propagating an
         import whose declared item type has NO graph-native
         resolution path at all — `MAPL_STATEITEM_WILDCARD`
         (`history_wildcard`'s own `A/E_A.*` regex-class import,
         resolved entirely by legacy's own `ExtensionFamily` regex
         machinery), `MAPL_STATEITEM_SERVICE`, or
         `MAPL_STATEITEM_EXPRESSION` — all explicitly out of
         `GraphBuilder`'s own documented scope
         (`GraphBuilder.F90`'s own module header). Before import
         propagation existed, such an import simply never reached any
         ancestor's own import-port table at all; propagating it
         exposes it to an ordinary ancestor connection that can never
         resolve it either. Fixed by a new filter,
         `is_ordinary_resolvable_import` (`GraphBuilder.F90`), excluding
         these three item types from propagation's own source scan.
      5. **A fourth, independently-discovered bug**, also not about
         export naming: `statistics_real`'s own `collection_1` imports
         (`TS_avg`, `UV_avg`, ...) are ordinary `MAPL_STATEITEM_FIELD`
         imports, genuinely resolved — but by a `SimpleConnection`
         declared at `history`'s own level, a connection class
         `connection_resolves_import` (propagation's own "is this
         already resolved" check) only ever recognized
         `class is (MatchConnection)` for; `SimpleConnection`/
         `ReexportConnection` fell to `class default` and were
         silently skipped, so propagation reported a genuinely-
         resolved import as unresolved and bubbled it needlessly.
         Fixed by a new `class default` branch in
         `connection_resolves_import`: a non-`MatchConnection`
         connection whose destination names the target component by
         its own EXACT short name is treated as "spoken for" (no
         `find_export_node` re-verification — this import's own real
         producer is resolved entirely within `StateRegistry`, which
         this procedure has no business re-checking), matching
         legacy's own `is_active()` gate, which likewise does not
         discriminate by connection class.

      **New files:** `superstructure/generic/graph/containers/
      PortIdStringMap.F90`. **Tests:**
      `Test_GraphBuilderPropagation.pf`:
      `test_run_export_propagate_hook_qualifies_one_level`,
      `test_run_export_propagate_hook_passthrough_second_level`,
      `test_run_export_propagate_hook_is_idempotent`,
      `test_propagated_export_resolves_via_unmodified_resolution` — all
      pass under NAG. **Full regression confirmed:**
      `ctest -R MAPL.generic.scenarios` passes 100% (3x, including the
      previously-failing `history_1`/`history_wildcard`/
      `statistics_real`); full top-level `ctest --test-dir nag` shows
      71/75 passing, the only 4 failures being the pre-existing,
      unrelated `ll-ll`/`cs-cs`/`cs-ll`/`ll-cs` app-regression tests
      (confirmed via `apps/tests/Regrid_Util/`, nothing to do with
      graph work).

      **gfortran portability fix (separate build pass):**
      `is_ordinary_resolvable_import` (above) was declared without
      `public` in `GraphBuilder.F90` — NAG tolerates this (the only
      caller is a submodule of the same parent module, resolved via
      host association regardless of visibility) but gfortran's
      linker does not: a separate-file submodule needs the symbol
      actually exported from the compiled `.o`, and the build failed
      with `Undefined symbols ... is_ordinary_resolvable_import`.
      Fixed by adding `public :: is_ordinary_resolvable_import`
      alongside this file's existing `resolve_local_node_id`/`is_self`/
      `itemtype_name` public declarations. After this fix,
      `module load gfortran-stack && cmake --build gfortran -j 8
      --target build-tests` succeeds cleanly, and
      `ctest --test-dir gfortran` shows the identical 71/75 passing
      (same 4 pre-existing failures as NAG) — Group 6 confirmed
      portable across both compilers.

## 7. Export visibility as a query (History support)

- [x] 7.1 Confirm (or add, if missing) a hierarchy-wide query that,
      given a qualified component path, returns that component's own
      local resource index entries directly — no push/propagation
      step, per design.md D4.

      **Implemented** as `query_component_exports(root, path, rc)`
      (`GraphBuilder.F90`) — composes the existing, unmodified
      `resolve_qualified_component(root, path, rc)` (path →
      `OuterMetaComponent`, descendant-only tree walk) with a new,
      factored-out single-level helper,
      `append_own_export_index_entries(owner, path_prefix, entries, rc)`,
      extracted with no behavior change from
      `collect_qualified_export_index`'s own per-component body (the
      recursive-into-children loop that body also contained stays in
      `collect_qualified_export_index` itself, now calling the same
      extracted helper once per level — `enumerate_qualified_exports`'s
      own existing behavior/tests are therefore unaffected by this
      refactor). `query_component_exports` calls the helper exactly
      once, against only the one resolved component, with no
      recursion into its children and no push/propagation step of any
      kind — reads straight through
      `ComponentGraph%get_resource_index(item_key(...))`, exactly as
      `enumerate_qualified_exports` already does, never creating a
      proxy. Public (gfortran cross-submodule-linkage precedent, same
      reasoning as `resolve_qualified_component`/
      `enumerate_qualified_exports`).
- [x] 7.2 Add test coverage demonstrating History-style deep-hierarchy
      export visibility (e.g. a 3+-level nested fixture) resolves
      correctly through this query path with no propagation step
      involved.

      **Implemented** as two new tests, `Test_GraphBuilder.pf`:
      `test_query_component_exports_scoped_to_resolved_component`
      (two-level root→CHEM→DU fixture — confirms querying CHEM returns
      only CHEM's own export, not DU's, and querying `CHEM/DU` directly
      returns DU's own export alone — the behavior that distinguishes
      this query from `build_qualified_export_namespace`'s own
      whole-subtree flattening) and
      `test_query_component_exports_deep_hierarchy_no_propagation`
      (three-level root→MID→LEAF fixture — confirms `query_component_
      exports(root, 'MID/LEAF')` finds LEAF's own export directly, with
      no `run_propagate_hook`/`run_export_propagate_hook` call anywhere
      in the test, and that root's own query returns zero entries,
      since nothing was ever pushed/mirrored up to it). Both pass under
      gfortran (`ctest --test-dir gfortran -R MAPL.generic.components`,
      verified via `-v` for the two new test names individually) and
      under NAG (`ctest --test-dir nag -R MAPL.generic.components`).
      Full `ctest --test-dir gfortran -R MAPL.generic` (7 targets,
      including `MAPL.generic.scenarios`) and the equivalent NAG run:
      100% pass, no regression.


## 8. Composite-member non-addressability (test-only; no code change expected)

- [x] 8.1 Add test coverage confirming a composite's internal member
      cannot be named directly as a connection endpoint (`graph/
      component-hierarchy` delta spec) — confirm existing
      `advertise_one` port-table population behavior already enforces
      this with zero code change required; if it does not, file the gap
      rather than silently patching it under this change's scope.

      **Confirmed, zero code change required** — traced
      `advertise_one` (`GraphBuilder_Advertising_smod.F90`) and
      `materialize_composite`/`materialize_member`
      (`CompositeStateMaterialization.F90`) directly: `add_resource_
      index`/`add_import_port`/`add_export_port` are called exactly
      once per `advertise_one` invocation, for the top-level identity
      only; `materialize_composite`/`materialize_member` register each
      member's own `StateItemNode` (reachable solely through the
      top-level node's own `GraphStateItem%state_members` map) but call
      none of those three port/index-population procedures for it — a
      member's own `NodeId` structurally never reaches a component's
      own resource index or import/export-port table.

      Added `superstructure/generic/tests/Test_GraphBuilder.pf::
      test_composite_member_not_independently_addressable`: two
      sibling components each advertise a composite ('PHYS_STATE')
      with one member ('tracers'); asserts (1) the member's own
      `NodeId` is absent from each component's own resource index
      under either state intent, (2) the member's own `NodeId` is
      absent as a VALUE in either component's own public import/
      export-port table (only the top-level composite's own `NodeId`
      is present), and (3) an ordinary `MatchConnection` declared at
      the parent naming the member directly by its own short_name
      (`tracers`) resolves nothing at all — zero entries in both
      `unresolved_imports` and `unsupported_characteristics` — since
      `for_each_matching_import` (which drives `resolve_match_
      connection`) only ever iterates the import-port table, which has
      no entry for the member to match against. Passes under both NAG
      (`ctest --test-dir nag -R MAPL.generic.components`) and gfortran
      (`ctest --test-dir gfortran -R MAPL.generic.components`). Full
      `ctest -L ESSENTIAL` (64 tests) confirmed 100% passing on both
      compilers, zero regression from this test-only addition.

## 9. Hierarchy-wide freeze

**[RESOLVED — implemented and tested, both NAG and gfortran, this
session.]** Original blocker (quoted below, kept for the historical
record of why this was not a one-line relocation) is resolved by
option (b): a new, explicitly non-recursive `GENERIC_INIT_GRAPH_FREEZE`
phase, appended once to `GENERIC_INIT_PHASE_SEQUENCE`
(`enums/GenericPhases.F90`), dispatched by `GenericGridComp.F90`'s
`initialize()` to `OuterMetaComponent%initialize_graph_freeze`
(`OuterMetaComponent/initialize_graph_freeze.F90`). That handler is a
no-op unless `this%is_hierarchy_root` (set once, in `init_meta.F90`,
exactly when a component requests a *fresh* `HierarchyId` rather than
being handed its parent's own) — on the true root only, it calls
`GraphBuilder%run_graph_freeze_hook`
(`GraphBuilder_Freeze_smod.F90`), which performs ONE explicit,
ordinary-Fortran-recursive walk of the whole component tree
(`freeze_subtree_`), freezing every component's own `ComponentGraph`
exactly once, then a SECOND walk (`collect_unresolved_subtree_`)
collecting every still-unresolved required import anywhere in the
hierarchy and failing hard (`assert_converged`) if any remain — the
direct hierarchy-wide replacement for `graphbuilder_run_connect_hook`'s
former per-component "freeze, then immediately check that one
component's own imports" sequence.

`graphbuilder_resolve_connections`'s own second-pass idempotency gate
(the one the blocker below worried about) could NOT be left relying on
`graph%is_frozen()` once freeze moved off the per-component
`ACCEPT_TRANSFER` call site — confirmed by writing the naive version
first and watching it regress `MAPL3G_Comp_Test_case42` (History
start/stop time-average correctness) under NAG. Fixed by adding an
explicit, separate idempotency flag,
`this%connections_already_resolved()`/`mark_connections_resolved()`
(`OuterMetaComponent/connections_resolved.F90`), consulted/set directly
by `graphbuilder_resolve_connections`
(`GraphBuilder_ConnectionResolution_smod.F90`) — this component's own
graph is now correctly mutable across BOTH `ACCEPT_TRANSFER` passes of
the fixed two-pass schedule, with no accidental re-resolution on the
second pass.

**[METHODOLOGY NOTE — important for anyone resuming graph-related work
in this tree]** An earlier pass this same session chased a convincing,
fully-reproduced-looking regression in `case42` through an extensive,
seemingly rigorous bisection (enum-value-shift test, sequence-position
test, a "duplicate an existing phase" control that correctly passed,
a "registered-but-never-invoked" control that correctly passed) and
concluded the trigger was "ESMF dispatching any brand-new
`ESMF_METHOD_INITIALIZE` phase number for the first time." That
conclusion was **wrong** — confirmed wrong by (a) building the exact
same "failing" commit under gfortran, where it passed outright, and
(b) re-running the identical NAG build with `cmake --build .
--clean-first` instead of a bare incremental `cmake --build .`, which
flipped the result to passing on the exact same commit and exact same
source tree. The entire bisection chain had been built on stale
`.o`/`.mod` artifacts from an incremental parallel (`-j8`) Fortran
submodule build that was not reliably recompiling every dependent when
an enum/interface file changed — not a real NAG or ESMF defect. Lesson:
when a graph-lifecycle change (anything touching
`enums/GenericPhases.F90`, `OuterMetaComponent.F90`'s own module
interface block, or any file with Fortran submodules depending on it)
appears to cause a real-lifecycle regression, re-verify with a forced
clean rebuild (`--clean-first` or an equivalent full `rm -rf` of the
build directory) BEFORE trusting the result, especially before
concluding a compiler- or ESMF-level root cause — an incremental
rebuild in this tree is not a reliable negative-or-positive signal on
its own.

Original blocker note (historical; the actual chosen resolution is
summarized above): `graphbuilder_freeze` was called from
`graphbuilder_run_connect_hook`
(`GraphBuilder_LifecycleHooks_smod.F90`), itself invoked from
`initialize_accept_transfer.F90`'s own recursive, per-component call
site at `GENERIC_INIT_ACCEPT_TRANSFER`. That phase is NOT a one-shot
phase - `GenericPhases.F90`'s own `GENERIC_INIT_PHASE_SEQUENCE` array
lists `GENERIC_INIT_ACCEPT_TRANSFER` TWICE (the documented
realize/accept/realize two-pass schedule,
`griddedcomponentdriver-integration-lifecycle` design.md REQ-MTH-011
step (c)). `graphbuilder_resolve_connections`
(`GraphBuilder_ConnectionResolution_smod.F90:999`) relies on
`graph%is_frozen()` as its OWN idempotency gate for the second pass:

```fortran
graph => this%get_component_graph()
if (graph%is_frozen()) then
   ... return   ! second-pass no-op
end if
```

If `freeze()` is simply moved to run once, later, with no replacement
guard, the SECOND `ACCEPT_TRANSFER` pass would re-run
`resolve_match_connection`'s real body a second time while still
unfrozen - re-walking connections, re-invoking
`try_materialize_field_extension`/`resolve_callback_destination`/
extension-chain creation, each of which mints a fresh `NodeId` and
registers a new graph node on every call (no "already resolved" guard
exists on this path the way `already_mirrored` guards task 6.1's
mirroring) - silently building duplicate graph structure, not merely
redundant work. This is the same class of real, previously-invisible
regression already found once this session (task 6.8's own
export-propagation root-cause story) - not a hypothetical.

**Resolution, per user direction (2026-10-16 session) - freeze moves
to a LATER, one-shot point in the lifecycle, not a hierarchy-wide
recursive walk from the current call site:** either (a) relocate the
freeze call (still per-component, still riding the existing
`recurse()` pattern every init phase already uses) to
`GENERIC_INIT_READ_RESTART` or later - a phase that appears exactly
ONCE in `GENERIC_INIT_PHASE_SEQUENCE`, so no new idempotency gate is
needed at all - or (b) introduce a new, explicitly NON-recursive
phase (e.g. `GENERIC_INIT_GRAPH_FREEZE`), appended once to the
sequence, whose handler is invoked exactly once at the true hierarchy
root and walks the whole component tree itself in a single explicit
pass (closer to design.md D7's literal "one call, made once"
wording; needs a new enum entry in `enums/GenericPhases.F90`, a new
`ESMF_GridCompSetEntryPoint` registration, a new dispatch case in
`GenericGridComp.F90`'s `initialize()` select case, and a guard so a
non-root component's own invocation of that phase is a no-op).
Neither option has been implemented or tested yet - tasks 9.1-9.4
below are written against whichever option is chosen; re-check this
note's own "which option" decision before starting 9.1.

**[Decision made, option (b) implemented]** See the `[RESOLVED]` note
above this historical blocker for the actual landed call-site and
idempotency-fix details.

- [x] 9.1 Replace each component's own independent
      `GraphBuilder_Freeze_smod.F90` call site (currently inside
      `graphbuilder_run_connect_hook`,
      `GraphBuilder_LifecycleHooks_smod.F90`, called from
      `initialize_accept_transfer.F90` at `GENERIC_INIT_ACCEPT_TRANSFER`
      - a phase that runs TWICE per the fixed two-pass schedule, see
      the blocker note above) with one call made once, at a point the
      whole hierarchy's init-phase sequence guarantees runs only once
      (design.md D7) - either option (a) or (b) above. Remove the
      `is_frozen()`-gated idempotency reliance from
      `graphbuilder_resolve_connections` only if the chosen option
      still leaves that procedure's own second-pass call unguarded by
      anything else - confirm with a direct test before removing it,
      do not assume.

      **Done.** Option (b) landed:
      `GraphBuilder_LifecycleHooks_smod.F90`'s
      `graphbuilder_run_connect_hook` no longer calls
      `graphbuilder_freeze` nor runs the convergence check itself -
      both moved to `graphbuilder_run_graph_freeze_hook`
      (`GraphBuilder_Freeze_smod.F90`), the `GENERIC_INIT_GRAPH_FREEZE`
      phase's own entry point. Confirmed by direct test (not assumed):
      the naive version (leave `graphbuilder_resolve_connections`'s own
      `is_frozen()` gate as-is, since this component's graph is now
      still mutable across both `ACCEPT_TRANSFER` passes) was tried
      first and found, via `MAPL3G_Comp_Test_case42`, to silently
      re-resolve connections on the second pass - fixed by the new
      `connections_already_resolved()`/`mark_connections_resolved()`
      flag (`OuterMetaComponent/connections_resolved.F90`).
- [x] 9.2 Update `ComponentGraph%freeze()`'s own semantics so a single
      hierarchy-wide freeze call freezes every component's dependency
      networks across the shared storage (`graph/component-graph` delta
      spec, "Freeze is a single hierarchy-wide operation").

      **Done**, via composition rather than changing `ComponentGraph%
      freeze()`'s own body: `graph_freeze` (`ComponentGraph.F90`)
      itself is unchanged (still freezes only the one instance's own
      owned networks, exactly as before) - the hierarchy-wide property
      is provided one level up, by `GraphBuilder_Freeze_smod.F90`'s new
      `freeze_subtree_` helper, which recursively calls `graph%freeze()`
      on every component's own `ComponentGraph` instance in the whole
      tree, exactly once, from the true hierarchy root
      (`run_graph_freeze_hook` → `freeze_subtree_`). Confirmed by
      direct test: `Test_GraphBuilder_ConvergenceCheck.pf::
      test_fully_resolved_proceeds_without_error` asserts
      `is_frozen()` on the root's own graph AND on both of its direct
      children's own graphs after a single `run_graph_freeze_hook`
      call issued only on the root - the property that distinguishes
      "hierarchy-wide" from "per-component, called once more" (added
      this session; the original version of this test, task 9.4 below,
      checked only the root's own graph).
- [x] 9.3 Update frozen-structure-rejects-mutation test coverage to
      exercise attempted mutation from an arbitrary component anywhere
      in an already-frozen hierarchy, not only the component that
      issued the freeze call.

      **Done.** New test,
      `Test_GraphBuilder_ConvergenceCheck.pf::
      test_mutation_rejected_from_arbitrary_hierarchy_component` (this
      session): `run_graph_freeze_hook` is called only on the
      hierarchy root (`parent_meta`); a direct child (`child_src`,
      which never itself called the hook) is confirmed frozen, then a
      mutation attempt directly on that child's own graph
      (`next_node_id`) is confirmed to raise the same
      `ComponentGraph: next_node_id called on a frozen graph` exception
      it would raise on the root - demonstrating the frozen state is a
      real hierarchy-wide property, not merely true of whichever
      component issued the call.
- [x] 9.4 Remove or update any test/code relying on per-component
      freeze-ordering (11a/11b's own cross-component freeze-timing
      reasoning) now made moot by hierarchy-wide freeze.
      `Test_GraphBuilder_ConvergenceCheck.pf`'s own
      `test_second_call_on_frozen_graph_is_noop` (currently asserts a
      second `run_connect_hook` call on an already-frozen graph is a
      safe no-op) and
      `test_unresolved_import_after_freeze_fails_explicitly`/
      `test_fully_resolved_proceeds_without_error` (currently assert
      `graph%is_frozen()` immediately after a single `run_connect_hook`
      call) all directly depend on freeze happening at the OLD call
      site and WILL need rewriting against whichever new call site 9.1
      adopts - not merely re-run as a regression check.

      **Done.** All three tests rewritten (this session) against the
      new `run_graph_freeze_hook` call site: each now calls
      `gb%run_connect_hook` on every component first (connect()-time
      resolution only, no freeze), THEN `gb%run_graph_freeze_hook` once
      on the hierarchy root (freeze + convergence check). No
      per-component freeze-ordering reasoning remains anywhere in this
      file. Confirmed passing under both NAG and gfortran
      (`ctest -R MAPL.generic.components`), and the full top-level
      `ctest -L ESSENTIAL` (64 tests) confirmed 100% passing on both
      compilers via a forced clean rebuild (`cmake --build .
      --clean-first`) - see the `[METHODOLOGY NOTE]` above this
      group's own header for why "forced clean" matters here
      specifically.

## 10. Full regression pass

- [x] 10.1 Run the complete `superstructure/generic/tests/scenarios/`
      and `graph-scenarios/` fixture suites under existing graph modes
      (`registry`/`graph`/`both`) and confirm no behavioral regression.

      **Done.** Working tree was already clean at the start of this
      task (Groups 1-9 fully committed through `15a50ec5`) — this task
      is a verification pass over already-landed code, not new
      production changes. Ran a full `build-tests` + `ctest` pass
      under both compilers:

      - **NAG** (`nag-stack/7.2.41`): `cmake --build . --target
        build-tests -j 8` clean; `ctest -L ESSENTIAL` (64 tests)
        100% passing; full `ctest` (75 tests) 71/75 passing, the only
        4 failures being the pre-existing `ll-ll`/`cs-cs`/`cs-ll`/
        `ll-cs` Regrid_Util regression-data tests (missing
        `LOCAL_REGRESSION_DATA_DIR` — unrelated to graph work, matching
        ARCHITECTURE.md §4.1's prior documentation of exactly these
        four).
      - **gfortran** (`gfortran-stack`): identical result — `ctest -L
        ESSENTIAL` 64/64 passing, full `ctest` 71/75 passing, same 4
        pre-existing failures, confirming portability.

      `MAPL.generic.scenarios` (the `scenarios/` fixture suite) and
      `MAPL.generic.graph`/`MAPL.generic.components` (which include the
      `graph-scenarios/` fixtures, e.g.
      `test_graph_mode_mirrors_grandchild_import_recursively`,
      `test_graph_mode_populates_sibling_states`,
      `test_graph_mode_propagates_through_two_characteristic_chain` —
      each explicitly exercising `GRAPH_MODE_REGISTRY` and
      `GRAPH_MODE_GRAPH`; `GRAPH_MODE_BOTH` is covered by
      `Test_GraphBuilderMaterializeGate.pf`) all passed under both
      compilers with zero failures. No regression found.
- [x] 10.2 Update `docs/graph/spec/02-component-hierarchy.md`,
      `07-component-graph.md`, `05-identities.md`, and
      `08-graph-builder.md` in place (`[RE-EVALUATED]`, following `07`'s
      own REQ-CG-003 precedent) to match the landed design — do not
      leave the narrative spec silently diverged from implementation.

      **Done.** All four files updated:
      - `02-component-hierarchy.md`: §2.2 (REQ-HIER-004, one-graph-
        per-component) and §2.3 (REQ-HIER-005/006, proxy mechanism)
        both marked `[RE-EVALUATED]` and rewritten to describe the
        landed hierarchy-wide shared storage and direct-real-NodeId
        cross-component wiring; §2.4's speculative future-global-graph
        view annotated noting it is substantially pre-empted by the
        landed shared storage.
      - `05-identities.md`: §5.1 (REQ-ID-002, generator scope),
        §5.2 (REQ-ID-003, extended with `HierarchyId`), §5.3
        (REQ-ID-005, authoritative-map ownership) all marked
        `[RE-EVALUATED]`.
      - `07-component-graph.md`: §7.1 (REQ-CG-001, ownership list) and
        §7.3 (REQ-CG-007, freeze) both marked `[RE-EVALUATED]`.
      - `08-graph-builder.md`: §8.2's responsibility table (REQ-GB-003)
        amended in place — proxy-related rows corrected, new rows added
        for import/export propagation, hierarchy-wide freeze, and the
        Task-Group-7 export-visibility query; a new "Retired" note
        added for the removed proxy mechanism.
- [x] 10.3 Add a new phase entry (next free number) to
      `docs/graph/spec/20-implementation-roadmap.md` recording this as
      an architecture-revision phase, listing every retired mechanism
      by name, per this document's own "discovered gap... never
      previously tracked" precedent (Phases 7/10/14).

      **Done.** Added `### 20.4.16 Phase 16` — lists every retired
      proxy mechanism by name, summarizes what changed (shared
      storage, proxy removal, import/export propagation, hierarchy-wide
      freeze, name-uniqueness enforcement, export-visibility query),
      records this session's own full regression-pass verification
      (both compilers), and reports task 10.4's own line-count finding
      (below) honestly rather than asserting the predicted decrease.
- [x] 10.4 Confirm `GraphBuilder.F90`'s own total line count (currently
      1517 + 4585 across submodules) has decreased, consistent with
      proposal.md's stated expectation that most of the reduction comes
      from deleted proxy mechanisms rather than new code.

      **Checked — did NOT decrease; proposal.md's own prediction did
      not hold.** Measured via `git show <commit>:<file> | wc -l` at
      each group's own landing commit:

      | Commit | Group | `GraphBuilder.F90` | 11 submodules | Total | Δ |
      |---|---|---|---|---|---|
      | `224f5a1d` | baseline (pre-change) | 1517 | 3068 | 4585 | — |
      | `f629872a` | Groups 3-5 (proxy removal) | 1465 | 3039 | 4504 | **-81** |
      | `9fdea0fd` | Group 6 (import/export propagation) | 1640 | 3571 | 5211 | +707 |
      | `bc51fc9c` | Group 7 (export-visibility query) | 1711 | 3571 | 5282 | +71 |
      | `1ea0fdc1` | Group 8 (test-only) | 1711 | 3571 | 5282 | +0 |
      | `15a50ec5` | Group 9 (hierarchy-wide freeze) | 1732 | 3710 | 5442 | +160 |

      Net result: **4585 → 5442 lines, +857 (+19%), not a decrease.**
      Proxy removal alone (Groups 3-5) did shrink the total by 81 lines
      exactly as predicted. But Group 6's required export-propagation
      companion (task 6.8, discovered as load-bearing during this same
      session, not optional polish — three distinct bugs found and
      fixed) and Group 9's hierarchy-wide freeze (a new one-shot init
      phase plus a two-pass freeze/convergence walk, plus a new
      explicit idempotency flag required after the naive version
      regressed `case42`) each added substantial new, necessarily-
      scoped functionality that outweighed the proxy-removal savings.
      proposal.md's prediction was reasonable at the time it was
      written (proxy mechanisms were real, measurable bulk — 81 lines
      confirmed) but did not anticipate either of these two
      discovered-during-implementation requirements. Recorded in
      `docs/graph/spec/20-implementation-roadmap.md` §20.4.16 ("Did not
      land as predicted") rather than silently closing this task against
      a prediction that did not hold.
