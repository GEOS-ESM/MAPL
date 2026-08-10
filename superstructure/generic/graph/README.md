COMPOSABLEGRAPH DESIGN SUMMARY
==============================

Status
------

This document describes the proposed graph architecture for hierarchical MAPL
components. It focuses on component composition, port relationships, graph
ownership, transforms, and dependency networks.

Detailed callback routing is intentionally deferred. The design preserves the
extension points required for callbacks without depending on a particular
callback-binding implementation.


1. Primary Architectural Requirement
------------------------------------

A subtree of MAPL components must behave as a single component.

A leaf component and a composite component must therefore expose the same kind
of public interface:

    ComponentInterface
        import ports
        export ports

The internal structure of a composite component must not be visible to its
parent. A parent interacts with a child subtree only through the public ports
published by the child's outer component.

A hierarchy-wide flattened graph may be generated for diagnostics or analysis,
but it is not the authoritative architectural representation.


2. Outer Components
-------------------

Every user component is wrapped by a MAPL-created outer, or meta, component.

At runtime, the outer component is a normal ESMF_GridComp. Its private state
contains an OuterComponent object.

Conceptually:

    Generic outer ESMF_GridComp
        private state:
            OuterComponent
                user ESMF_GridComp
                child outer ESMF_GridComp handles
                local ComposableGraph
                public ComponentInterface
                explicit run-sequence information

The user component may declare children, but it does not directly own or retain
references to their private implementation state.

Each declared child is itself represented by an outer ESMF_GridComp whose
private state contains another OuterComponent. This creates the recursive
component hierarchy.

The OuterComponent is the unit presented to its parent. The wrapped user
component is an internal participant in that unit.


3. One ComposableGraph Per Outer Component
------------------------------------------

Each OuterComponent owns exactly one local ComposableGraph.

The local graph expresses the composition of:

    * the wrapped user component's ports;
    * the published ports of child outer components;
    * the current outer component's public ports;
    * concrete data representations;
    * locally required transforms;
    * local dependency networks.

The graph does not contain or embed the internal graphs of child components.

A parent does not obtain a pointer to a child's ComposableGraph and does not
traverse a child's nodes or edges.


4. Components Are Not Ordinary Graph Nodes
-------------------------------------------

A user component, child component, or outer component is not required to be an
ordinary GraphNode.

Components are executable entities with interfaces and encapsulation
boundaries. In a parent's local graph, a child is represented by local proxy
ports corresponding to the child's published public ports.

A component node may be generated for visualization, but it is not necessary
for the core dataflow or composition semantics.

The authoritative relationship is:

    component instance
        publishes ports

    parent graph
        creates local proxies for those ports
        connects the proxies using local graph topology


5. Component Ports
------------------

A port is a component-relative interface occurrence.

A port has at least:

    * a stable PortId within its component interface;
    * a public or local name;
    * a VirtualConnectionPt identity, including name and direction;
    * an ItemSpec value specification;
    * a binding to a representation in the owning local graph.

A port is not necessarily the underlying data representation. It describes how
a component boundary exposes or consumes that representation.

Conceptually:

    PublicPort
        PortId
        VirtualConnectionPt
        ItemSpec

`VirtualConnectionPt` is authoritative for port name and state intent. The
graph `ItemSpec` is authoritative only for value metadata used by graph
matching and transforms. Runtime `StateItemSpec` remains the owning state
registry's payload/allocation object; it is not copied into a public port.

Port direction semantics are:

    * import: component consumes an incoming representation;
    * export: component produces a representation for consumers;
    * internal: component-local connection point, never published through a
      `ComponentInterface`.

`ComponentInterface` stores only public imports and exports. Internal points
may participate in local graph construction but must not cross a component
boundary.

The owning OuterComponent also has a private binding:

    PortBinding
        PortId
        local representation NodeId

The parent sees the PublicPort description, but it does not see the child's
private local NodeId.


6. Published Component Interfaces
---------------------------------

After constructing its local graph, an OuterComponent publishes a
ComponentInterface:

    ComponentInterface
        component identity or diagnostic name
        public import ports
        public export ports

The ComponentInterface is an interface description, not a view into the
component's graph.

It should be possible to copy or query this description without giving the
parent access to the child's internal nodes, edges, transforms, or dependency
networks.

This replaces the architectural role currently approximated by parent
StateRegistry objects holding pointers to child StateRegistry objects.


7. How a Parent Sees a Child
----------------------------

The parent retains:

    * an opaque ESMF_GridComp handle for the child outer component;
    * a published ComponentInterface for that child;
    * parent-local proxy nodes for the child's public ports;
    * bindings between those proxies and the child's public PortIds.

Conceptually:

    ChildInstance
        instance name
        child outer ESMF_GridComp handle
        published ComponentInterface

For each visible child port, the parent creates:

    ChildPortBinding
        parent-local proxy NodeId
        child instance index or identity
        child public PortId
        port direction

Example:

    Child interface:
        export PortId 4
        name = temperature
        spec = FIELD, R8, grid G1

    Parent graph:
        NodeId 17
        kind = child-export proxy
        copied spec = FIELD, R8, grid G1

    Parent binding:
        local proxy NodeId = 17
        child instance = 2
        child PortId = 4

The parent connects NodeId 17 to other nodes in the parent's graph. It does not
know which NodeId implements PortId 4 inside the child graph.


8. No Ordinary Cross-Graph Edges
--------------------------------

A GraphEdge always connects nodes owned by the same ComposableGraph.

An ordinary edge must not start in a parent graph and terminate at an internal
node in a child graph.

A component boundary is represented by a boundary binding:

    parent-local child-port proxy
        <- boundary association ->
    public port on child outer component

The complete logical data route through a hierarchy is therefore composed from:

    * local graph paths;
    * boundary bindings;
    * local graph paths in adjacent outer components.

This preserves component boundaries while still permitting the framework to
derive a hierarchy-wide route when necessary.


9. Import Propagation
---------------------

Imports are resolved locally before being exposed to a parent.

For every import belonging to the wrapped user component or a child:

    1. Search for a permitted local export.
    2. If the source representation matches, connect it directly.
    3. If adaptation is required, insert a local transform chain.
    4. If no local source exists, expose the import as an outer import port.

An unsatisfied child import therefore bubbles upward by becoming part of the
current outer component's public interface.

Conceptually:

    outer import port
        -> optional local transforms
        -> child import proxy

If several internal imports can use one incoming representation, they may share
an outer import and reusable transform intermediates when the specifications
and connection rules permit it.

At the root, any remaining unsatisfied required imports are errors.


10. Export Promotion
--------------------

Export propagation is intentionally asymmetric with import propagation.

A child export is available for local connections inside its parent's
ComposableGraph, but it does not automatically become a public export of the
parent.

An export crosses the outer boundary only when explicitly exposed according to
the user component's declarations.

Conceptually:

    child or user export proxy
        -> optional local transforms
        -> explicitly declared outer export port

This prevents adding a child from silently enlarging the public interface of an
entire subtree.


11. Representation Nodes
------------------------

A RepresentationNode describes one concrete representation of a state item.

It may contain:

    * a NodeId;
    * a resolved StateItemSpec;
    * a diagnostic label;
    * runtime state-item or allocation information;
    * constituent NodeIds for compound entities such as field bundles.

Examples of distinct representations include:

    temperature on grid G1 in R8 and Kelvin
    temperature on grid G2 in R8 and Kelvin
    temperature on grid G2 in R4 and Celsius

Representation nodes are authoritative graph objects. A separate
builder-owned Representation record should not duplicate the same information.

Import and export are port roles, not necessarily intrinsic properties of a
representation node. Multiple local ports may bind to one representation when
the semantics and storage model permit it.


12. Transform Nodes
-------------------

TransformNode is a subclass of GraphNode.

A transform is represented as an operation node rather than merely as an edge:

    RepresentationNode
        -> TransformNode
        -> RepresentationNode

This supports:

    * transform-private state;
    * multiple inputs or outputs;
    * reusable transform results;
    * explicit execution status;
    * ESMF-backed runtime execution;
    * future user-provided transforms.

A TransformNode owns or contains a Transform runtime abstraction. The Transform
will normally contain an ESMF_GridComp or a derived object wrapping an
ESMF_GridComp.

TransformNode contains graph-specific information:

    * graph identity;
    * input representation bindings;
    * output representation bindings;
    * execution policy;
    * dependency state or references.

Transform contains runtime-specific information:

    * transform ESMF_GridComp;
    * transform kind and configuration;
    * initialization state;
    * execution resources;
    * ESMF lifecycle behavior.

Transforms have component-like input and output interfaces, but differ from
ordinary user components:

    * transforms have no children;
    * transforms are normally selected and wired automatically;
    * transforms are normally dependency-triggered;
    * user components are invoked through explicit MAPL run sequencing.

Under the hood, both may use ESMF_GridComp.


13. Transform Ownership
-----------------------

A transform is owned by the lowest outer component whose local graph sees both
ends of the connection requiring the transform.

Examples:

    user component to child:
        current outer graph owns the transform

    child to user component:
        current outer graph owns the transform

    one child to a sibling:
        their parent's outer graph owns the transform

    outer input to internal child:
        current outer graph owns the transform

    internal source to outer export:
        current outer graph owns the transform

    wiring internal to a child subtree:
        child's outer graph owns the transform

Transforms are not inserted merely because a component boundary is crossed.
They are inserted only when the connected specifications require adaptation.


14. Dependency Networks
-----------------------

A ComposableGraph may own multiple DependencyNetworks.

The graph owns all GraphEdge objects. A DependencyNetwork is a selected
execution/dependency view over graph-owned edges.

Conceptually:

    DependencyNetwork
        set of EdgeIds
        incoming adjacency index
        outgoing adjacency index
        acyclicity validation state

Possible networks include:

    * a default local runtime network;
    * operation-specific networks;
    * future callback-specific networks.

An edge may belong to more than one DependencyNetwork.

A DependencyNetwork never contains an edge entering an internal node of another
ComposableGraph. Networks are strictly local to their owning OuterComponent.

Network mutation should go through ComposableGraph so the graph can verify:

    * the network exists;
    * the edge exists;
    * both endpoints belong to the local graph;
    * frozen topology is not being modified.


15. Execution Semantics
-----------------------

Graph topology and execution policy are related but distinct.

User and child component methods:

    * are invoked through explicit MAPL run sequences;
    * execute generic ESMF wrappers;
    * delegate into OuterComponent or user procedures;
    * mark their outputs updated after execution.

Transforms:

    * are triggered by data dependencies;
    * execute when their inputs are valid and their outputs are required;
    * mark their result representations updated;
    * may release downstream transforms.

The OuterComponent is responsible for coordinating explicit component method
execution with dependency-triggered graph processing.


16. Callback Extension Point
----------------------------

Detailed callback binding and callback routing are outside the immediate design
scope.

The architecture reserves support for:

    * CallbackNode as a GraphNode subclass;
    * callback-specific DependencyNetworks;
    * public callback-state ports;
    * boundary relay or binding records;
    * explicit callback invocation barriers;
    * pre-callback and post-callback transforms.

A callback route crossing multiple subtrees will not be represented by one
cross-graph edge. It will be composed from local graph segments and explicit
boundary relays.

No callback-specific design should weaken these core rules:

    * child graphs remain private;
    * dependency networks remain local;
    * graph edges do not cross component boundaries;
    * parent components invoke children through public outer-component APIs.


17. Identifier Scope
--------------------

All references to graph objects use IDs rather than persistent pointers.

The graph owns node and edge storage. Clients retain NodeIds, EdgeIds, PortIds,
and DependencyNetworkIds.

There are two acceptable allocation strategies:

    1. IDs are local to their owning ComposableGraph.
    2. IDs are allocated globally but each object still belongs to one local
       ComposableGraph.

The architecture does not depend on global uniqueness. If local IDs are used,
a NodeId is meaningful only together with its owning graph.

Cross-boundary relationships use:

    child instance identity + public PortId

They do not require a parent to store a child's internal NodeId.

IDs may be opaque 32-bit integers, monotonically allocated and never reused.
Zero may be reserved as invalid.


18. Construction Algorithm
--------------------------

An OuterComponent is constructed bottom-up.

Phase 1: Construct children

    * Create or initialize each declared child outer component.
    * Allow each child to construct and freeze its own local graph.
    * Query or copy each child's published ComponentInterface.
    * Do not retain a pointer to the child's graph.

Phase 2: Create local proxies

    * Add proxies for wrapped user-component imports and exports.
    * Add parent-local proxies for child public imports and exports.
    * Record user-port and child-port bindings.

Phase 3: Resolve local connections

    * Match permitted exports to imports.
    * Enforce at most one source for each ordinary import.
    * Insert transforms where specifications differ.
    * Reuse transform intermediates when valid.

Phase 4: Expose unresolved imports

    * Create outer import ports for required imports not satisfied locally.
    * Connect outer imports to internal targets through any needed transforms.

Phase 5: Expose declared exports

    * Promote user exports and explicitly selected child exports.
    * Do not automatically promote all child exports.
    * Insert transforms if the public representation differs.

Phase 6: Validate and freeze

    * Validate IDs and local edge endpoints.
    * Validate port bindings.
    * Validate dependency networks.
    * Validate that every internal required import is connected or exposed.
    * Validate that every public export was explicitly authorized.
    * Freeze topology against further structural mutation.

The resulting public interface is the interface of the entire subtree.


19. Principal Types and Relationships
-------------------------------------

The central ownership structure is:

    OuterComponent
        user_grid_comp
        children[]
            child outer ESMF_GridComp handle
            published ComponentInterface
        graph: ComposableGraph
        public_interface
        explicit run sequence

    ComposableGraph
        GraphNode map
            RepresentationNode
            TransformNode
            future CallbackNode
            optional local port-proxy node types
        GraphEdge map
        DependencyNetwork map
        user port bindings
        child port bindings
        outer public port bindings
        frozen state

    ComponentInterface
        public imports[]
        public exports[]

    PublicPort
        PortId
        VirtualConnectionPt
        ItemSpec

    ChildPortBinding
        parent-local proxy NodeId
        child instance identity
        child public PortId

    TransformNode extends GraphNode
        input representation NodeIds
        output representation NodeIds
        Transform runtime object
            transform ESMF_GridComp


20. Critical Invariants
-----------------------

The implementation should enforce the following invariants:

    1. Every OuterComponent owns one local ComposableGraph.

    2. Every child subtree publishes the same kind of ComponentInterface as a
       leaf component.

    3. A parent sees only child public ports and an opaque child runtime handle.

    4. A parent never connects directly to an internal child node.

    5. Every ordinary GraphEdge has both endpoints in one ComposableGraph.

    6. Every DependencyNetwork is local to one ComposableGraph.

    7. Unsatisfied required imports are promoted to outer import ports.

    8. Child exports are promoted only when explicitly declared.

    9. Transforms are owned by the graph containing the connection that
       requires them.

   10. Components and transforms may both use ESMF_GridComp at runtime without
       having identical architectural roles.

   11. Components are not required to be ordinary graph nodes.

   12. All persistent graph references use IDs rather than node pointers.

   13. A flattened hierarchy-wide graph is a derived view, not the source of
       truth.

   14. Replacing a subtree with another component having a compatible public
       interface must not require changes to the parent's internal graph
       construction logic.


21. Questions an Adversarial Review Should Test
-----------------------------------------------

A design review should attempt to find counterexamples to the following claims:

    * Can every unsatisfied import be represented at the next outer boundary
      without exposing child internals?

    * Can two sibling children be connected using only parent-local proxies?

    * Can transforms be introduced at multiple hierarchy levels without
      duplicating or bypassing component boundaries?

    * Can a child change its internal graph without invalidating its parent's
      graph when its public interface remains unchanged?

    * Can a subtree be replaced by a leaf component exposing the same ports?

    * Can reusable transform intermediates remain local to the correct
      composition level?

    * Can runtime data and validity propagate across boundaries without
      introducing ordinary cross-graph edges?

    * Can callback routing later be added using boundary relays without giving
      parents access to child graphs?

    * Can local dependency networks support nested runtime execution without
      becoming hierarchy-spanning networks?

    * Is the distinction between public port, local port proxy, and concrete
      representation sufficiently explicit to prevent accidental coupling?

Any proposed simplification that violates the critical invariants should be
treated as a change to the component architecture rather than merely an
implementation optimization.
