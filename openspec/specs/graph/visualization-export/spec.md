# Visualization Export Specification

## Purpose

Provides a graph-neutral, read-only exporter that serializes a
`ComponentGraph`'s topology and metadata to plain-text DOT and JSON for
consumption by external, non-Fortran rendering tools.

## Requirements

### Requirement: Exporter depends only on graph-neutral public query APIs
The exporter SHALL depend only on `ComponentGraph`'s and
`DependencyNetwork`'s public query operations and `NodeId%to_string()`,
and SHALL NOT depend on `OuterComponent`, `StateRegistry`, or
connection-point configuration. Nodes SHALL be labeled only by their
`NodeId%to_string()` value at this layer.

#### Scenario: Export succeeds with only synthetic graph-neutral types
- **WHEN** export is requested against a `ComponentGraph` built and
  wired entirely from synthetic nodes and values
- **THEN** export completes without any `OuterComponent`,
  `StateRegistry`, or component-hierarchy type being referenced

### Requirement: Export is explicit, read-only, and non-mutating
Export SHALL be an explicit, opt-in operation, never triggered
implicitly. Exporting SHALL NOT execute any `TransformGraphNode`,
advance any `NodeRevision`, or otherwise mutate graph state; it SHALL
be usable at least once after the graph has been frozen.

#### Scenario: Export does not change graph state
- **WHEN** export is performed against a graph containing a
  `TransformGraphNode` that has never executed
- **THEN** after export completes, the transform still reports that it
  has never executed and every node's revision is unchanged

#### Scenario: Export works on a frozen graph
- **WHEN** export is requested against a graph that has already been
  frozen
- **THEN** export completes and reports the graph's final structural
  topology

### Requirement: Exported content covers nodes, edges, and port bindings
For each node, the export SHALL include at minimum its
`NodeId%to_string()` identity and its dispatchable kind
(`StateItemNode`/`TransformGraphNode`/other `OperationGraphNode`), and,
for a `StateItemNode`, its payload kind when statically knowable. For
each dependency, the export SHALL include the source and target node
identities and the owning `DependencyNetworkId`. Where a transform
port-binding table entry exists for an edge, its port name SHALL be
attached to that edge; a missing entry SHALL be tolerated rather than
treated as an export error.

#### Scenario: Node kind and payload kind are present
- **WHEN** a graph containing a `StateItemNode` and a
  `TransformGraphNode` is exported
- **THEN** the export reports each node's dispatchable kind, and the
  `StateItemNode`'s entry additionally reports its payload kind

#### Scenario: Edge carries its owning network identity
- **WHEN** a dependency exists between two nodes in a specific
  `DependencyNetwork`
- **THEN** the corresponding exported edge identifies that
  `DependencyNetworkId`

#### Scenario: Bound port name appears as an edge label
- **WHEN** an edge connects a `StateItemNode` to a declared, bound port
  of a `TransformGraphNode`
- **THEN** the exported edge carries that port's name

#### Scenario: Missing port binding does not fail export
- **WHEN** an edge exists between a `StateItemNode` and a
  `TransformGraphNode` port with no corresponding port-binding table
  entry
- **THEN** the edge is still exported, without a port-name label

### Requirement: Export supports DOT and an equal-content JSON form
The exporter SHALL produce Graphviz DOT text as its primary output and
SHALL also support producing JSON carrying the same node/edge/metadata
content, with no image rendering performed by either path.

#### Scenario: DOT and JSON describe the same graph
- **WHEN** the same graph is exported to DOT and to JSON
- **THEN** both outputs enumerate the same set of nodes and the same
  set of edges with the same network and port-binding attribution

#### Scenario: No rendering side effect
- **WHEN** either export form is produced
- **THEN** no image file or rendered graphic is created as a side
  effect

### Requirement: Optional revision metadata never mutates state
The export MAY optionally include each `StateItemNode`'s current
`NodeRevision` as read-only metadata, using only the existing
revision-accessor operation. A structural export with no revision
metadata included SHALL remain valid on its own.

#### Scenario: Revision metadata reflects current state without advancing it
- **WHEN** revision metadata is included in an export
- **THEN** the reported value matches what `get_revision()` would
  return at that moment, and no revision is advanced as a result of
  producing the export

#### Scenario: Structural export remains valid without revision metadata
- **WHEN** export is produced without requesting revision metadata
- **THEN** the resulting output is still a complete, valid structural
  export of nodes, edges, and port bindings

### Requirement: Export never serializes payload data
The exporter SHALL NOT serialize `ESMF_Field`/`ESMF_FieldBundle`/
`ESMF_State` array contents or other bulk payload data; scope is
limited to topology and metadata.

#### Scenario: Field array contents are absent from export
- **WHEN** a graph containing a `StateItemNode` with an allocated field
  payload is exported
- **THEN** the export contains the field's classification metadata but
  no array data values

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
