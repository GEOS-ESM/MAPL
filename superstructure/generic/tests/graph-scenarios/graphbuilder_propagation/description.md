cap -> comp_mid -> comp_dst (recursive import mirroring)
==========================================================

This fixture resurrects the test shape named by
openspec/changes/hierarchy-wide-component-graph, tasks.md 6.5 and
design.md D3's own postmortem of the rejected "search the subtree"
alternative: `comp_mid` is an intermediate component that declares
*no* connection of its own, and has no advertised import of its own
either - its only child, `comp_dst`, is the one that advertises an
unresolved import (`T`). `cap` (the grandparent) declares an ordinary
`all_unsatisfied` connection naming `comp_src` as source and `comp_mid`
as destination - `comp_mid`, not `comp_dst` - exactly the shape design.md
D3 points out a subtree-search approach could not reproduce correctly
(nothing at `cap`'s level names `comp_dst` specifically; `cap`'s
connection only happens to name `comp_mid`, the component one level
closer, as its own declared destination).

For this connection to resolve, `comp_dst`'s own unresolved import
must already be mirrored into `comp_mid`'s own import-port table by
the time `cap`'s own connection resolution runs against `comp_mid` -
`comp_mid`'s own `graphbuilder_run_propagate_hook` call (run as part
of its own `GENERIC_INIT_ADVERTISE` phase, before `cap`'s own
advertise-phase work resumes) is what performs that mirroring step,
reusing the existing, unmodified `for_each_matching_import`/
`resolve_one` machinery - no search, no proxy.

Expected structural outcome, confirmed by
Test_GraphModeDemonstration.pf's own
`test_graph_mode_mirrors_grandchild_import_recursively`:
- `comp_mid`'s own graph never registers a resource-index entry for
  `T` (it never advertised one directly) - only an IMPORT-PORT entry,
  populated by mirroring, pointing at `comp_dst`'s own real `NodeId`.
- The dependency edge `cap`'s own connection creates lives in `cap`'s
  own default network, directly between `comp_src`'s own real export
  `NodeId` and `comp_dst`'s own real import `NodeId` - the same two
  real endpoints a direct (non-mirrored) connection would use, with no
  proxy node standing in for either side at any level.

Task 6.7 (explicitly out of scope here, per design.md D3's own
"genuinely open" note): inserting the mirrored import into
`comp_mid`'s own real `ESMF_State` under a child-qualified name
(`comp_dst/T`) is not implemented yet - this fixture exercises only
the graph-side structural outcome above, not a real run-phase value
flowing into `comp_mid`'s own real import state.
