## ADDED Requirements

### Requirement: Exporter accepts an optional node-label lookup
The DOT and JSON exporters SHALL each accept an optional, pre-built
`NodeId`-keyed label lookup supplied by the caller. When the lookup
contains an entry for a node, that entry's label text SHALL be used in
place of the node's `NodeId%to_string()` in the output. When no lookup
is supplied, or a given node has no entry in it, output for that node
SHALL be unchanged from the exporter's existing `NodeId%to_string()`
behavior. The exporter itself SHALL NOT resolve labels on its own;
supplying the lookup SHALL remain the caller's responsibility (mirrors
REQ-VIZ-003's existing constraint that this layer depends only on
graph-neutral query APIs).

#### Scenario: Labeled node appears under its supplied label in DOT
- **WHEN** DOT export is requested with a label lookup containing an
  entry for a node in the graph
- **THEN** that node's DOT entry shows the supplied label rather than
  its `NodeId%to_string()` value

#### Scenario: Labeled node appears under its supplied label in JSON
- **WHEN** JSON export is requested with a label lookup containing an
  entry for a node in the graph
- **THEN** that node's JSON entry shows the supplied label rather than
  its `NodeId%to_string()` value

#### Scenario: Export without a label lookup is unchanged
- **WHEN** DOT or JSON export is requested without a label lookup
- **THEN** every node's identity is rendered exactly as
  `NodeId%to_string()`, matching existing behavior

#### Scenario: Node with no lookup entry falls back to its identity string
- **WHEN** a label lookup is supplied but has no entry for a particular
  node in the graph
- **THEN** that node's identity is rendered as `NodeId%to_string()`,
  unchanged from the no-lookup case

### Requirement: Boundary/proxy nodes are marked distinguishably
When the caller identifies a node as a boundary/proxy node, the DOT and
JSON exporters SHALL render it distinguishably from an ordinary node:
DOT SHALL use a distinct node shape or style attribute for it, and JSON
SHALL include a `"proxy": true` field for it. A node not identified as a
proxy SHALL carry no such marking.

#### Scenario: Proxy node is visually distinct in DOT
- **WHEN** a node in the graph is identified to the exporter as a
  boundary/proxy node
- **THEN** its DOT entry carries a distinct shape/style attribute not
  present on an ordinary node's entry

#### Scenario: Proxy node carries a proxy field in JSON
- **WHEN** a node in the graph is identified to the exporter as a
  boundary/proxy node
- **THEN** its JSON entry includes `"proxy": true`

#### Scenario: Ordinary node carries no proxy marking
- **WHEN** a node in the graph is not identified to the exporter as a
  boundary/proxy node
- **THEN** neither its DOT nor its JSON entry carries any proxy marking
