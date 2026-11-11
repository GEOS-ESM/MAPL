## 1. Baseline facts (read-only, before code)

- [x] 1.1 Find how the `invalidate` scenario (graph mode, 2 steps, value
      changes on step 2) gets its converted value today, given no
      production code advances an export revision. Record the answer in
      design.md D5 before changing the run-phase pull.
      *Result:* only the legacy couplers (still active in graph mode)
      deliver the step-2 value; no graph export revision was ever
      advanced, so graph transforms ran once. D5 stands (recorded there).
- [x] 1.2 Run `history_1` in graph mode on HEAD and record which items
      fail; this is the deep-pull acceptance case.
      *Result:* `history_1` already passed in the pending graph/both runs
      on HEAD (legacy couplers cover the deep pull); it is now
      `graph_ready` and passes in graph mode with the forward network.

## 2. Shared storage (HierarchyGraph)

- [x] 2.1 `HierarchyGraph` creates the forward network with the hierarchy
      and exposes its id; `ComponentGraph` no longer creates a default
      network. Rename `get_default_network_id` to `get_forward_network_id`
      in every caller and test.
- [x] 2.2 Move `PortBindingTable` into `HierarchyGraph`; `bind_port` and
      `get_port_binding(s)` use it; `bind_port` accepts any transform
      registered in the hierarchy.
- [x] 2.3 Add the hierarchy-wide reuse index and `add_reuse_entry` /
      `get_reuse_entry`; move the `EXTCHAIN:`, `EXPREVAL:`,
      `CALLBACK_COLLECTION:` and `WILDCARD_COLLECTION:` keys to it;
      delete `find_owning_component_graph` and its callers' workaround.
- [x] 2.4 `add_dependency` accepts the forward network from any graph in
      the hierarchy; a second, different producer into a `StateItemNode`
      in the forward network fails naming the item; an identical re-add is
      a no-op.
      *Owner decision 2026-11-08:* in the connection pass a second producer
      is a `connection_conflicts` entry naming the item; that connection is
      skipped, the first producer stays and the pass continues (rc
      success). Legacy last-wins is a row in `docs/graph/scenario-parity.md`.
- [x] 2.5 Tests (`graph/tests`): two graphs share the forward id; a
      binding made through one graph is read through another; second
      producer rejected; identical re-add accepted; cycle across two
      graphs rejected.

## 3. Update by item

- [x] 3.1 `ComponentGraph%update(node_id, rc)` on the forward network;
      remove the network argument; update every caller.
- [x] 3.2 Test: an item pulled through the owner's graph and through the
      declaring graph reaches the same transform (executes once).

## 4. Wiring sites

- [x] 4.1 Connection resolution, expression evaluation, collections,
      inout forward edge, geometry pull and push: forward network.
      *Note:* the geometry push skips the component's own FROM_CHILD
      provider (design.md D5 note). Temporary: C4-b redesigns geometry as
      explicit import/export geoms.
- [x] 4.2 Callback: get edges into the forward network; only a put
      network is created; drop the get network id from
      `CallbackMethodBinding`; `pull_callback_get_arguments` pulls each
      bound member. Fix `Test_CallbackMethodBinding` (its hand-built
      get-net producer) and add a test where the provider's producer is a
      transform wired through another graph.
      *Done as:* `get_network_id` and `get_get_network_id` removed; only
      `get_put_network_id` remains (no accessor silently changed meaning).
- [x] 4.3 `StatePopulation`, `SetSharedCharacteristic`,
      `MethodInvocation` and `GraphExport` read the forward network.
- [x] 4.4 Test: cross-declaring-component reuse (parent and grandparent
      connect one export through the same conversion: one transform).

## 5. Validation and freeze

- [x] 5.1 Remove `validate_cross_network_writes`; simplify the allowed-id
      sets in `add_dependency`/`validate` to "registered in this
      hierarchy".
- [x] 5.2 `run_graph_freeze_hook` validates and freezes the forward
      network once after `freeze_subtree_`; each graph freezes its own
      write-back networks.

## 6. Run phase

- [x] 6.1 Per design.md D5 (as amended by 1.1): in graph mode, each
      component pulls its own imports before its user run and advances its
      own exports after a successful run; remove the after-run loop over
      children.
- [x] 6.2 Test: grandparent-declared connection with a units chain, two
      steps, changed export value reaches the grandchild on step 2.
- [x] 6.3 `history_1`: graph-ready if it now passes; otherwise report the
      remaining failure to the owner.

## 7. Specs, docs, close

- [x] 7.1 Update `docs/graph/spec/06` (REQ-DEP-003/004 rationale,
      008/008a), `07` (REQ-CG-004), `11` (REQ-REV-011), `15` (REQ-CB-018)
      to match the delta specs; mark `[RE-EVALUATED]`.
- [x] 7.2 NAG and gfortran Debug ESSENTIAL pass; every `graph_ready`
      scenario passes in graph mode.
- [x] 7.3 Status line in the plan; `PLAN_TASK_MSG` with both ctest lines.
