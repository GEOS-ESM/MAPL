## MODIFIED Requirements

### Requirement: Graph-native payload parity with the legacy oracle
For each exact-match connection a covered fixture declares, the system
SHALL verify that the graph-native export node's own materialized
payload — read directly from the node, independent of any real
`ESMF_State` — agrees with the legacy coupler's own real field for that
same connection on: typekind, units, horizontal geometry, and shape
(rank and extents). Data-value agreement after a run phase is
explicitly **not** required under `both` mode: `both` mode never routes
graph-native materialization through `graphbuilder_populate_state`
(gated to `graph` mode only, by construction — see roadmap
§20.4.8 "Explicitly out of scope"), so a graph-native field's data
value, once allocated via `FieldCreate`/`FieldFill`, is never written
to by a `GENERIC_RUN_USER` phase — only the legacy field is. Comparing
the two values under `both` mode would therefore always fail, not by a
defect this harness could catch, but by `both` mode's own structural
design (no shared-state collision also means no shared-state write).
Value parity remains a real, open requirement for the day `graph` mode
is validated as the sole producer (Phase 6's own entry criterion,
§20.4.2) — just not one this capability, scoped to `both` mode only,
can ever demonstrate.

For a covered fixture whose connection is resolved through a
framework-created extension chain (one or more characteristic
mismatches, `graph/extension-reuse`) rather than directly, the system
SHALL additionally verify that the chain's final, framework-created
item has the typekind and units the import actually declared — not
merely that a chain of the expected length was created. This check is a
static, pre-run-phase attribute check (typekind, units), the same kind
of check the exact-match case already performs — it does not require or
imply data-value agreement after a run phase, which remains out of
scope for this capability for the same structural reason stated above.

#### Scenario: Static attributes agree before any run phase
- **WHEN** the harness compares a covered fixture's graph-native export
  payload against the legacy coupler's own real field for the same
  exact-match connection, before any run phase has executed
- **THEN** typekind, units, horizontal geometry, and shape agree between
  the two

#### Scenario: Mismatched-connection chain's final item has the import's own typekind and units
- **WHEN** the harness compares a covered fixture whose connection was
  resolved through a framework-created extension chain
- **THEN** the chain's final, framework-created item's materialized
  payload has the typekind and units the import actually declared, not
  the export's own original typekind or units

### Requirement: Covered fixture set spans plain, 3D, and ungridded-dims fields
The harness SHALL cover, at minimum, one fixture for each of: a plain
2D scalar field with no vertical dimension, a field with a vertical
dimension (3D), a field with at least one ungridded dimension beyond
the vertical, and a field whose export and import mismatch on more than
one declared characteristic simultaneously (resolved through a
multi-step extension chain, `graph/extension-reuse`). Each covered
fixture SHALL be flat (a parent with two direct-child components only,
no further nesting), since cross-level import propagation is not yet
visible to graph-native resolution.

#### Scenario: Plain scalar field fixture is covered
- **WHEN** the harness runs its plain scalar field fixture
- **THEN** the equivalence checks above both pass for that fixture's
  exact-match connection

#### Scenario: 3D (vertical-dimension) field fixture is covered
- **WHEN** the harness runs its vertical-dimension field fixture
- **THEN** the equivalence checks above both pass for that fixture's
  exact-match connection, with shape parity including the vertical
  extent

#### Scenario: Ungridded-dims field fixture is covered
- **WHEN** the harness runs its ungridded-dims field fixture
- **THEN** the equivalence checks above both pass for that fixture's
  exact-match connection, with shape parity including the ungridded
  extent

#### Scenario: Multi-characteristic mismatch fixture is covered
- **WHEN** the harness runs its fixture whose export and import
  mismatch on more than one declared characteristic simultaneously
- **THEN** the no-state-leak check passes for that fixture's connection,
  and the chain's final item has the typekind and units the import
  actually declared
