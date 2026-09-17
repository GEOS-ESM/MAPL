# 15. Callback Interfaces

Status: `[SETTLED]` data model and registry, and wildcard pattern syntax
(§15.9, resolved as regex); `[OPEN]` role terminology (§15.5, Q6),
ESMF-placement-vs-semantics distinction (§15.5, Q7).

## 15.1 No special node type for callbacks

**REQ-CB-001.** Callback states are real ESMF States with attached methods
via `ESMF_MethodAdd`/`ESMF_MethodExecute`. A callback State MUST be
represented by an ordinary `StateItemNode` containing a `StateValue`.
Callbacks MUST NOT require a special `GraphNode` subclass.

## 15.2 Metadata concepts

The model requires these concepts:

- `CallbackInterface` — reusable contract
- `CallbackArgumentSpec`
- `CallbackMethodSpec`
- `CallbackStateBinding`
- `CallbackMethodAttachment`
- `CallbackMethodBinding`
- `CallbackConnection`
- `CallbackInterfaceRegistry`

## 15.3 CallbackInterface

**REQ-CB-002.** `CallbackInterface` is a reusable contract. Service name is
the key in `CallbackInterfaceRegistry`'s lookup map and MUST NOT be
duplicated as a field inside `CallbackInterface` itself.

**REQ-CB-003.** `CallbackInterface` MUST contain:

- `argument name → CallbackArgumentSpec`
- `method name → CallbackMethodSpec`

**REQ-CB-004.** `CallbackMethodSpec` MUST contain `argument name →
AccessSpec`, allowing one shared argument to have different access in
different methods of the same interface.

**Example — PassiveTracer interface:**

```
argument:
  tracers, ESMF FieldBundle

get:
  tracers = OUT

put:
  tracers = IN
```

## 15.4 AccessSpec

**REQ-CB-005.** `AccessSpec` MUST be a general MAPL concept (not
callback-specific), with values analogous to: `IN`, `OUT`, `INOUT`,
`UNSPECIFIED`.

## 15.5 Handler / Invoker roles `[OPEN — terminology, not the distinction]`

Two roles MUST be distinguished, and MUST NOT be confused with "who
provides the higher-level scientific service":

- **Invoker** — calls an attached State method it does not own
- **Handler** — owns the callback state and its attached implementation

**Examples:**

| | Advection | Radiation |
|---|---|---|
| Invoker | DYN (calls callbacks) | Radiation (calls callbacks) |
| Handler | Tracer components (own callback state, private tracer data) | Aerosol components (own callback state, compute optical properties) |
| Scientific service provider | DYN provides advection | Aerosol components provide the science |

**REQ-CB-006.** Documentation and API naming MUST use `Invoker`/`Handler`
rather than ambiguous terms like "service provider," because the
mechanical caller/owner roles and the scientific-service-provider role are
orthogonal (DYN is Invoker but not the tracer-owning party; Aerosol is
Handler *and* scientific provider — these do not line up consistently, so
a single "provider" term would mislead in the Advection case).

**`[OPEN]`** Whether `Handler`/`Invoker` are themselves the best final terms
(vs. e.g. `Owner`/`Caller`, `Implementer`/`Client`) is not settled — see
`17-open-questions.md` Q6. Do not treat the *words* as final; the
*distinction* they name is settled (REQ-CB-006).

## 15.6 CallbackStateBinding

**REQ-CB-007.** `CallbackStateBinding` MUST explicitly store:

- `CallbackInterfaceId`
- `NodeId` of the callback State (the `StateItemNode` holding the
  `StateValue`)
- `argument name → member NodeId`
- `method name → method attachment`

## 15.7 CallbackInterfaceRegistry

**REQ-CB-008.** Callback interfaces MUST be shared across component graphs
(they are not per-`ComponentGraph` data). The current practical design is a
private module singleton, justified by:

- a full MAPL application generally has one component hierarchy
- children cannot access parents (REQ-HIER-003), so a root-owned registry
  with pointers threaded down to descendants is awkward
- interface registration happens primarily at construction time

**REQ-CB-009.** The registry MUST own:

- `CallbackInterfaceId → CallbackInterface`
- `service name → CallbackInterfaceId`
- a `CallbackInterfaceIdGenerator`

**REQ-CB-010.** The registry MAY be accessed through a private
module-level `get_registry()` function, or through narrower
registration/lookup wrapper procedures that hide the singleton from most
callers.

## 15.8 Physical placement vs. semantic role `[OPEN]`

**REQ-CB-011.** The physical ESMF placement of callback States cannot be
fully hidden, because user methods receive separate import and export
States as distinct arguments.

**Plausible convention (`[SPECULATIVE]`, not settled):**

- Handler callback States are placed in **export** States (the Handler
  owns/exposes the implementation)
- Invoker callback *collections* are placed in **import** States (the
  Invoker receives references to callbacks it will call)

**REQ-CB-012.** Dedicated MAPL accessor procedures SHOULD hide this
placement convention from ordinary component code, so that a future change
to the convention does not require touching every component.

**`[OPEN]`** How to express this ESMF-placement convention without
overloading `StateIntent` (`IMPORT`/`EXPORT`) in a way that becomes
misleading — since a Handler's callback State sits in *its own* export
State but is semantically "owned/written" data from the Handler's
perspective, not necessarily an "export" in the traditional coupling sense
— is not resolved. See `17-open-questions.md` Q7.

## 15.9 Qualified descendant exports and wildcard aggregation

**REQ-CB-013.** MAPL3 already makes descendant exports visible at
ancestors for History diagnostics: each descendant export State is added
as a uniquely-named substate keyed by component name, and MAPL3 flattens
these so all descendant export States appear as sibling substates,
addressable as `component/item` (e.g. `DU/tracers`, `SS/tracers`,
`MOIST/tracers`).

**REQ-CB-014.** This existing flattening mechanism MUST be reused to solve
callback aggregation — no new "export bubbling" mechanism should be
invented in parallel.

**REQ-CB-015.** A callback Invoker MAY connect to a wildcard/virtual source
connection point selecting multiple qualified exports, e.g.:

```
tracers        (tracers exported directly by the current component)
*/tracers      (tracers from flattened descendants)
```

**REQ-CB-016.** `GraphBuilder` MUST, at wiring time:

1. Expand the wildcard against the currently-known qualified export
   namespace
2. Validate that every match implements the expected `CallbackInterface`
3. Materialize a flat callback collection for the Invoker (an ESMF State
   whose members are the matched callback States or `ESMF_NamedAlias`es
   thereof)

**Example** — DYN receiving tracer callbacks:

```
DYN import State
  tracers
    DU
    SS
    SU
    NI
    BC
    TR
    MOIST
```

Each member is an actual ESMF callback State or `NamedAlias`.

**REQ-CB-017.** A user-level connection expressing this is conceptually:

```
source:
  PHYS export virtual connection point
  pattern = tracers or */tracers

destination:
  DYN import item
  name = tracers
```

This generalizes MAPL's existing qualified-export mechanism rather than
requiring every intermediate component to re-export every callback
explicitly.

**Resolved (was `[OPEN]`):** pattern syntax is **regular expression**, per
the spec author, matching existing precedent in the legacy application.
The `*/tracers`-style examples above are illustrative shorthand for "tracer
exports from any descendant," not literal glob syntax — implementations
MUST use regex matching against the flattened qualified-export namespace.
See `17-open-questions.md` Q5.

## 15.10 Callback dependency networks

**REQ-CB-018.** A callback method MAY have its own `DependencyNetwork`. For
an `INOUT` callback argument:

```
get network:  provider representation -> transforms -> callback representation
put network:  callback representation -> transforms -> provider representation
```

The same `StateItemNode`s MAY participate in both networks. Each
method-specific network MUST remain acyclic individually (REQ-DEP-004),
even though their union may be cyclic.

**REQ-CB-019.** A method-level binding MUST identify:

- `DependencyNetworkId`
- the `MethodGraphNode` to invoke
- argument source and target `NodeId`s

**REQ-CB-020.** The callback method MUST be invoked exactly once, after
*all* required argument paths across its relevant network(s) have been
prepared. It MUST NOT be triggered independently, once per argument.
