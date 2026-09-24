# Visualization Enrichment Specification

## Purpose

Builds a `NodeId -> label` lookup for one component's own local graph,
from the component's own advertised item names and its cached
child-proxy nodes, so the graph-neutral exporter can render
human-readable diagrams without itself depending on `OuterComponent` or
`StateRegistry`.

## Requirements

### Requirement: Enrichment layer labels a component's own advertised items
For a given component's own local graph, the enrichment layer SHALL
produce a `NodeId`-keyed label lookup in which every item the component
itself advertised (import, export, or internal) is labeled with its own
declared name.

#### Scenario: Advertised item is labeled by its own name
- **WHEN** a component advertises an import, an export, and an internal
  item, and its graph is exported through the enrichment layer
- **THEN** each of the three items' exported label is its own declared
  name, not a raw `NodeId` string

### Requirement: Enrichment layer labels cached child-proxy nodes distinguishably
For each child whose proxy node is cached in this component's own
graph, the enrichment layer SHALL label that node with the child's name
combined with the child's own item name, and SHALL mark it as a
boundary/proxy node.

#### Scenario: Cached proxy node carries a child-qualified label
- **WHEN** a connection has created a cached proxy node in this
  component's graph standing in for a named child's item
- **THEN** that node's exported label combines the child's name and the
  child's item name, and the node is marked as a proxy in the export

### Requirement: Enrichment never fails export when a name is unavailable
If an item has no discoverable name (e.g. a framework-created extension
item, or a node kind the enrichment layer does not resolve names for),
the enrichment layer SHALL leave that node unlabeled rather than
failing the export; the exporter then falls back to its own existing
default identity rendering.

#### Scenario: Unnamed node does not abort enrichment
- **WHEN** a graph contains a node the enrichment layer has no name for
- **THEN** export completes successfully, and that node's identity is
  rendered using the exporter's own existing fallback

### Requirement: Enrichment layer does not cross into a child's own graph
Building the label lookup SHALL rely only on this component's own graph
and the plain, already-published item lists of its children. It SHALL
NOT query a child's own internal graph, `NodeId`s, or
`DependencyNetwork`.

#### Scenario: Enrichment reads only published child information
- **WHEN** the label lookup is built for a component with children
- **THEN** no child's internal graph or `NodeId`/`DependencyNetwork`
  identity is accessed — only the child's already-published item names
  and this component's own cached proxy nodes are used
