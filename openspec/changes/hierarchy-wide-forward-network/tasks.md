## 1. Baseline facts (read-only, before code)

- [ ] 1.1 Find how the `invalidate` scenario (graph mode, 2 steps, value
      changes on step 2) gets its converted value today, given no
      production code advances an export revision. Record the answer in
      design.md D5 before changing the run-phase pull.
- [ ] 1.2 Run `history_1` in graph mode on HEAD and record which items
      fail; this is the deep-pull acceptance case.

## 2. Shared storage (HierarchyGraph)

- [ ] 2.1 `HierarchyGraph` creates the forward network with the hierarchy
      and exposes its id; `ComponentGraph` no longer creates a default
      network. Rename `get_default_network_id` to `get_forward_network_id`
      in every caller and test.
- [ ] 2.2 Move `PortBindingTable` into `HierarchyGraph`; `bind_port` and
      `get_port_binding(s)` use it; `bind_port` accepts any transform
      registered in the hierarchy.
- [ ] 2.3 Add the hierarchy-wide reuse index and `add_reuse_entry` /
      `get_reuse_entry`; move the `EXTCHAIN:`, `EXPREVAL:`,
      `CALLBACK_COLLECTION:` and `WILDCARD_COLLECTION:` keys to it;
      delete `find_owning_component_graph` and its callers' workaround.
- [ ] 2.4 `add_dependency` accepts the forward network from any graph in
      the hierarchy; a second, different producer into a `StateItemNode`
      in the forward network fails naming the item; an identical re-add is
      a no-op.
- [ ] 2.5 Tests (`graph/tests`): two graphs share the forward id; a
      binding made through one graph is read through another; second
      producer rejected; identical re-add accepted; cycle across two
      graphs rejected.

## 3. Update by item

- [ ] 3.1 `ComponentGraph%update(node_id, rc)` on the forward network;
      remove the network argument; update every caller.
- [ ] 3.2 Test: an item pulled through the owner's graph and through the
      declaring graph reaches the same transform (executes once).

## 4. Wiring sites

- [ ] 4.1 Connection resolution, expression evaluation, collections,
      inout forward edge, geometry pull and push: forward network.
- [ ] 4.2 Callback: get edges into the forward network; only a put
      network is created; drop the get network id from
      `CallbackMethodBinding`; `pull_callback_get_arguments` pulls each
      bound member. Fix `Test_CallbackMethodBinding` (its hand-built
      get-net producer) and add a test where the provider's producer is a
      transform wired through another graph.
- [ ] 4.3 `StatePopulation`, `SetSharedCharacteristic`,
      `MethodInvocation` and `GraphExport` read the forward network.
- [ ] 4.4 Test: cross-declaring-component reuse (parent and grandparent
      connect one export through the same conversion: one transform).

## 5. Validation and freeze

- [ ] 5.1 Remove `validate_cross_network_writes`; simplify the allowed-id
      sets in `add_dependency`/`validate` to "registered in this
      hierarchy".
- [ ] 5.2 `run_graph_freeze_hook` validates and freezes the forward
      network once after `freeze_subtree_`; each graph freezes its own
      write-back networks.

## 6. Run phase

- [ ] 6.1 Per design.md D5 (as amended by 1.1): in graph mode, each
      component pulls its own imports before its user run and advances its
      own exports after a successful run; remove the after-run loop over
      children.
- [ ] 6.2 Test: grandparent-declared connection with a units chain, two
      steps, changed export value reaches the grandchild on step 2.
- [ ] 6.3 `history_1`: graph-ready if it now passes; otherwise report the
      remaining failure to the owner.

## 7. Specs, docs, close

- [ ] 7.1 Update `docs/graph/spec/06` (REQ-DEP-003/004 rationale,
      008/008a), `07` (REQ-CG-004), `11` (REQ-REV-011), `15` (REQ-CB-018)
      to match the delta specs; mark `[RE-EVALUATED]`.
- [ ] 7.2 NAG and gfortran Debug ESSENTIAL pass; every `graph_ready`
      scenario passes in graph mode.
- [ ] 7.3 Status line in the plan; `PLAN_TASK_MSG` with both ctest lines.
