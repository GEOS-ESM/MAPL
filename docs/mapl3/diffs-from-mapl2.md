# Differences Between MAPL3 and MAPL2

This document describes the major differences between MAPL3
(`release/MAPL-v3`) and MAPL2 (`develop`).  It is intended as a
reference for developers and users migrating from MAPL2 to MAPL3, and
as an overview of the architectural goals and design decisions behind
MAPL3.

> **Note:** This is a living document.  Sections marked with
> **[REVIEW NEEDED]** require additional input from subject-matter
> experts before they should be considered authoritative.

---

## Table of Contents

1. [Component Structure](#1-component-structure)
2. [Connections Between Components](#2-connections-between-components)
3. [Variable and Field Specifications](#3-variable-and-field-specifications)
4. [Resource and Configuration Files](#4-resource-and-configuration-files)
5. [Cap and Time Loop](#5-cap-and-time-loop)
6. [History](#6-history)
7. [ExtData](#7-extdata)
8. [Statistics Component](#8-statistics-component)
9. [Clocks](#9-clocks)
10. [Build System](#10-build-system)
11. [Logging (pFlogger)](#11-logging-pflogger)
12. [Grid and Geometry Access](#12-grid-and-geometry-access)
13. [Items Requiring Peer Review](#14-items-requiring-peer-review)

**Related documents**

- [`api-changes.md`](api-changes.md) — detailed, procedure-level listing of every stubbed-out V2 API and the new MAPL3 replacements (lifecycle, child management, field specs, connectivity, resource access, timers)

---

## 1. Component Structure

### Commonalities

In both MAPL2 and MAPL3, user/model components are implemented as ESMF
gridded components that can be composed in a hierarchical manner.  Both
frameworks expect the user to register component details during
`SetServices`:

- Add child components
- Declare import/export/internal state items
- Add connectivity between components

### MAPL2 Component Model

In MAPL2, a private state of type `MAPL_MetaComp` is attached to the
ESMF gridded component to provide MAPL "generic" services.

When a user registers entry points (Initialize, Run, Finalize), two
methods are effectively added to the ESMF component: the user's
procedure and a generic "wrapper" procedure.  The wrapper instruments
the user procedure (e.g., inserting profilers).

**The key burden on the user:** The user component must explicitly call
generic initialize on each of its children during its own initialize
phase.  If the user fails to do this correctly the hierarchy is not
properly initialized.

The generic layer also implements rules such as automatically "bubbling
up" unsatisfied imports (imports with no connected export) to the
parent component.

Child components are referenced by integer IDs obtained when the child
is first added.

#### Run Phases in MAPL2

MAPL2 components can have multiple run phases (generally at most two in
practice) but support only one initialize phase and one finalize phase.

### MAPL3 Component Model

In MAPL3, user components are *wrapped* within a framework "generic"
component.  The generic component has its own private state object of
type `OuterMetaComponent` that provides framework services.  The
fundamental design principle is that the generic component acts as a
coherent, self-contained manager for an entire sub-hierarchy.

When a user component adds a child, the framework:

1. Creates a new generic component that wraps the child user component
   inside its `OuterMetaComponent`.
2. Adds this new generic component to the parent's list of children.

**The key benefit for the user:** The framework drives initialization,
execution, and finalization of the entire hierarchy automatically.
Users no longer need to explicitly call generic initialize on children.

Child components are referenced by name (string) rather than by integer
ID.

#### Initialization Phases in MAPL3

MAPL3 provides multiple, well-defined initialization phases that map
closely (though not yet completely) to NUOPC.  Compatibility with NUOPC
is a major design goal of MAPL3.  The phases are defined in
`GenericPhases.F90`:

| Phase | Purpose |
|-------|---------|
| `GENERIC_INIT_SET_CLOCK` | Set up component clock |
| `GENERIC_INIT_GEOM_A` | First geometry initialization pass |
| `GENERIC_INIT_GEOM_B` | Second geometry initialization pass |
| `GENERIC_INIT_ADVERTISE` | Advertise import/export fields |
| `GENERIC_INIT_MODIFY_ADVERTISED` | Modify advertised fields if needed |
| `GENERIC_INIT_REALIZE` | Allocate and realize fields |
| `GENERIC_INIT_READ_RESTART` | Read restart files |
| `GENERIC_INIT_USER` | User-specific initialization |

The framework automatically cycles through these phases.  User
components can customize any phase, but most components only need to
implement `GENERIC_INIT_USER`.

#### Per-Component Scheduling: `timestep`, `reference_time`, and `offset`

In MAPL3, each child component can specify its own execution timestep
and reference time independently of the parent clock.  The relevant
fields in the component spec are:

- `timestep` — the component's run interval (replaces the earlier
  `run_dt` name used during development)
- `reference_time` — an absolute time anchor for the component's alarms
- `offset` — a relative offset from `reference_time` such that
  `runTime = reference_time + offset`

The framework checks that `timestep` and `reference_time` are mutually
compatible for both the `OuterMetaComponent` and the user component.

#### generic3g and the `mapl3g_` Module Prefix

The new `generic3g` directory contains the MAPL3 reimplementation of
the generic layer.  Modules in this directory temporarily carry the
`mapl3g_` prefix to distinguish them from their MAPL2 counterparts
while the transition is in progress.  `generic3g` is intended to fully
replace the existing `generic` directory when complete.

`generic3g` also uses `ESMF_CONTEXT_PARENT_VM` by default when creating
`ESMF_GridComp` objects, which is the correct context for components
embedded within a parent VM.

#### Declarative Component Configuration: The `mapl:` Section

In MAPL3, every component's YAML config file may contain a `mapl:`
section that the framework reads before calling the user's
`SetServices`.  This is the primary mechanism for declaring a
component's geometry, children, and how the hierarchy is assembled —
without writing or recompiling Fortran.

The `mapl:` section supports the following top-level keys (all optional):

| Key | Purpose |
|-----|---------|
| `geometry:` | Declare or inherit the component's horizontal/vertical geometry |
| `children:` | Declare child components to be loaded from shared libraries |
| `connections:` | *(Advanced)* Wire fields between children; most connections are expressed in Fortran code, but can also be declared here |
| `states:` | *(Advanced)* Declare import/export/internal variables; the preferred approach is to use ACG instead |
| `misc:` | Testing and checkpoint/restart flags |

##### `geometry:`

Controls how this component's geometry is established.  The `kind:` key
selects the mode:

| `kind` | Meaning |
|--------|---------|
| `from_parent` | Inherit geometry from the parent (default when `geometry:` is absent) |
| `provider` | This component defines its own geometry via `esmf_geom:` and/or `vertical_grid:` |
| `from_child` | Inherit geometry from a named child (`provider: <child_name>`) |
| `none` | Component has no geometry (ungridded) |

##### `children:`

Declares child components that the framework loads and registers
automatically — no Fortran required.  Each entry maps a child name to a
specification:

```yaml
children:
  <child_name>:
    dso: <library_name>           # required — shared library base name
    SetServices: <procedure_name> # optional, default 'setservices_'
    config_file: <path.yaml>      # optional — child's own YAML config
    timestep: <ISO8601_duration>  # optional — overrides parent clock step
    run_time_offset: <duration>   # optional — shifts child's run phase
```

The `dso:` key accepts four synonymous spellings: `dso`, `DSO`,
`sharedObj`, `sharedobj`.  The library name is portable: `libfoo`,
`libfoo.so`, and `libfoo.dylib` all resolve correctly at load time.
Loading is delegated to ESMF, which uses `dlopen()` / `LoadLibrary()`
under the hood.

When `config_file:` is given, the named YAML is made available to the
child's own `mapl:` parser.

Both config-declared and Fortran-declared children can coexist in a
single component.  Config-declared children are registered first, then
the user's `SetServices` runs and may add further children via
`MAPL_AddChild()`.

##### Examples

Three representative cases follow.

**`DYN.yaml` — component defines its own cubed-sphere geometry:**

```yaml
mapl:
  geometry:
    kind: provider
    esmf_geom:
      class: CubedSphere
      im_world: 180
    vertical_grid:
      class: model
      fields:
        pressure: PLE
      num_levels: 72
```

**`GOCART.yaml` — component inherits geometry from its parent:**

```yaml
mapl:
  geometry:
    kind: from_parent
```

This is also the default when `geometry:` is omitted entirely.

**`AGCM.yaml` — component gets geometry from a child, and declares its
children via DSO:**

```yaml
mapl:
  geometry:
    kind: from_child
    provider: DYN

  children:
    DYN:
      dso:         libDYN_GridComp
      config_file: DYN.yaml
    PHYS:
      dso:         libPHYS_GridComp
      config_file: PHYS.yaml
    GOCART:
      dso:         libGOCART_GridComp
      config_file: GOCART.yaml
```

---

## 2. Connections Between Components

### MAPL2 Connections

In MAPL2, connections are generally only supported between state items
that share the same grid, precision, and units.  Mismatched items
cannot be automatically connected; the user is responsible for
ensuring compatibility.

### MAPL3 Connections

In MAPL3, connections between mismatched imports and exports are
supported wherever a conversion is possible.  The framework
automatically handles:

- **Regridding** — connecting fields on different grids
- **Precision conversion** — connecting fields of different
  floating-point kinds
- **Unit conversion** — connecting fields with compatible but different
  units (via udunits2); values in an `ESMF_Field` are automatically
  converted when the connected import and export carry compatible but
  different unit attributes
- **Vector-aware regridding** — correct transformation of tangent
  vector quantities such as horizontal winds

---

## 3. Variable and Field Specifications

### MAPL2 Field Specifications

In MAPL2 there are two approaches to declaring fields:

1. **Explicit API calls** — `MAPL_AddImportSpec`, `MAPL_AddExportSpec`,
   `MAPL_AddInternalSpec` called directly in `SetServices`.
2. **Automatic Code Generator (ACG)** — a Python tool that reads a
   `.rc` spec file and generates the corresponding Fortran boilerplate.

Both approaches were in common use in MAPL2.

### MAPL3 Field Specifications

MAPL3 retains both approaches (explicit API and ACG), but the ACG
format has evolved: spec files now use the `.acg` extension and the
generated code targets MAPL3 procedures.  The old `.rc`-based ACG
format is not supported in MAPL3.

The ACG for MAPL3 also gains several new columns:

| Column | Description |
|--------|-------------|
| `ALIAS` | Declare an alternative name for a field |
| `add2export` | Automatically add an internal field to the export state |
| `RESTART` | Enumerated restart behavior (replaces the old string values); validated at parse time |

#### Extended State Item Types

MAPL3 introduces a richer set of virtual state item types.  Because
MAPL remains built on ESMF, the underlying representation is still
limited to ESMF state items (`ESMF_Field`, `ESMF_FieldBundle`,
`ESMF_State`), but MAPL3 treats these differently — especially during
the connection process — through an extended set of
`ESMF_StateItem_Flag` values:

| Type | Description |
|------|-------------|
| `MAPL_STATEITEM_UNKNOWN` | Uninitialized/unknown type |
| `MAPL_STATEITEM_FIELD` | Standard scalar/vector field |
| `MAPL_STATEITEM_FIELDBUNDLE` | Bundle of fields |
| `MAPL_STATEITEM_STATE` | Nested ESMF state |
| `MAPL_STATEITEM_CALLBACK_STATE` | State used with inter-component callbacks; supports automatic regridding and conversion |
| `MAPL_STATEITEM_SERVICE` | Replaces the MAPL2 "friendly" mechanism (see below) |
| `MAPL_STATEITEM_SERVICE_PROVIDER` | Component that provides a service |
| `MAPL_STATEITEM_SERVICE_SUBSCRIBER` | Component that subscribes to a service |
| `MAPL_STATEITEM_WILDCARD` | Connects to all exports matching a name pattern; primarily used by chemistry to automatically track component export changes |
| `MAPL_STATEITEM_BRACKET` | Supports time interpolation of a field between two states; primarily used by ExtData |
| `MAPL_STATEITEM_VECTOR` | Enforces proper regridding of tangent vector quantities (e.g., horizontal winds) |
| `MAPL_STATEITEM_VECTOR_BRACKET` | Combines VECTOR and BRACKET semantics |
| `MAPL_STATEITEM_EXPRESSION` | Run-time evaluation of simple expressions among scalar fields |

#### The SERVICE Type and the Replacement of "Friendly"

In MAPL2, internal state items could be declared as "friendly" to a
particular service.  This made the internal item also available in the
export state, and ancestor components (typically `PHYS`) would
aggregate these friendly items into a single bundle that became an
import for other components.  The canonical example is tracer
advection: various physics components declare their constituents as
friendly to advection, and `PHYS` aggregates them into the bundle that
the dynamical core imports.

> **[REVIEW NEEDED — Atanas Trayanov]:** Please verify and expand the
> description of the MAPL2 "friendly" mechanism and document how
> `MAPL_STATEITEM_SERVICE` / `SERVICE_PROVIDER` / `SERVICE_SUBSCRIBER`
> replace it in MAPL3.

---

## 4. Resource and Configuration Files

### MAPL2 Resource Files

MAPL2 uses "ESMF Config" format (a key-value text format without an
explicit schema) for component resource files.  Resource sharing across
the component hierarchy is largely unregulated, which has led to some
problematic practices where child components could inadvertently
read — or even modify — configuration intended for other components.

### MAPL3 Resource Files

MAPL3 uses **ESMF HConfig**, which is based on YAML and has explicit
schema support.  Component resource files are read via `MAPL_GetResource`
calls that now accept an `ESMF_HConfig` object directly, without
requiring a separate `HConfigParams` wrapper object.

A key isolation rule: by default, children receive a *copy* of their
parent's HConfig.  Children cannot modify settings in ancestor
components or in sibling components.

> **Caveat:** The `mapl` section of HConfig is not shared with
> children.  This section is used for framework-level configuration
> such as specifying child DSOs (see [Declarative Component
> Configuration](#declarative-component-configuration-the-mapl-section)).

---

## 5. Cap and Time Loop

### MAPL2 Cap

In MAPL2, the time loop is located inside `CapGridComp`.  This
placement is now recognized as a design mistake.  Additionally, time
is advanced *before* History output, meaning output is written at the
*end* of a timestep before the clock advances — a source of confusion
in practice.

### MAPL3 Cap

In MAPL3, the time loop has been moved into `Cap` (outside
`CapGridComp`).  Time is now advanced *after* History output, which
means History correctly sees the state at the end of the completed
timestep before the clock moves forward.

The Cap is configured via a YAML file (`cap.yaml`).  A reference
example:

```yaml
cap:
  name: CAP
  restart: cap_restart.yaml

  clock:
    start: 1891-03-01T00:00:00
    stop:  2999-03-02T21:00:00
    dt:    PT900S
    segment_duration: P1H
    # repeat_duration: P1Y   # for perpetual/spinup runs

  checkpointing:
    enabled: true
    final:   true
    path:    checkpoints
    alarms:
      - {frequency: PT6H, refTime: '1891-03-01T00:00:00'}
      - {times:   ['1891-03-01T00:00:00']}
      - {offsets: ['P6H']}   # relative to segment start

  root_name:    &root    GCM
  extdata_name: &extdata EXTDATA
  history_name: &history HIST

  mapl:
    children:
      *root:
        dso:         libgcm_gc
        config_file: GCM.yaml
      *extdata:
        dso:         libextdata_gc
        config_file: extdata.yaml
      *history:
        dso:         libhistory_gc
        config_file: history.yaml

#####################################
# Global services
esmf:
  logKindFlag:      ESMF_LOGKIND_MULTI_ON_ERROR
  defaultCalKind:   ESMF_CALKIND_GREGORIAN
  logAppendFlag:    false

mapl:
  model_petcount: 1
  pflogger:       pflogger.yaml

  servers:
    pfio:
      num_nodes: 9
    model:
      num_nodes: any
```

The `cap.mapl.children` section is one instance of the general `mapl:`
config mechanism — see [Declarative Component
Configuration](#declarative-component-configuration-the-mapl-section)
in Section 1 for the full description.

#### The `GEOS.x` Universal Executable

MAPL3 ships a single universal executable, `GEOS.x`
(`gridcomps/cap3g/GEOS.F90`).  Through the combination of `cap.yaml`,
DSO loading, and ESMF's server infrastructure, `GEOS.x` can run *any*
GEOS model configuration without recompilation.  There is no longer a
need for separate model-specific executables such as `GEOSgcm.x`.

It is invoked with a single positional argument — the path to the
top-level YAML config file:

```sh
mpirun -np N GEOS.x cap.yaml
```

`cap.yaml` has four top-level sections that together drive the entire
run:

| Section | Purpose |
|---------|---------|
| `esmf:` | ESMF initialisation (`logKindFlag`, `defaultCalKind`, etc.) |
| `mapl:` | Global MAPL settings: `model_petcount`, `pflogger_cfg_file`, `servers:` (I/O server node counts and DSOs) |
| `cap:` | Clock (`start`, `stop`, `dt`, `segment_duration`), restart file, checkpointing alarms, and the root/extdata/history component names |
| `cap.mapl.children:` | The DSO declarations that select which science components to load — this is what makes `GEOS.x` universal |

By changing only `cap.yaml` — and in particular the `cap.mapl.children:`
entries — an operator can switch between entirely different model
configurations (atmosphere-only, coupled, aquaplanet, etc.) without
touching a line of Fortran or rebuilding the executable.

There is no separate generic equivalent — `GEOS.x` serves all MAPL3
applications, GEOS and non-GEOS alike.

---

## 6. History

### MAPL2 History

- Configured via the `HISTORY.rc` file (ESMF Config format).
- Used internal "cheats" to access export items from the root of the
  component hierarchy.
- Regridding to the requested output grid was performed inside History
  itself.
- Supported simple arithmetic expressions as output items.

### MAPL3 History (History3G)

- Configured via a YAML file.
- Each collection is implemented as a separate child component.
- **Clock interaction change:** Because time is now advanced *after*
  History runs, History must check the status for the *next* timestep
  when determining output.  This is a direct consequence of the
  [Cap time-loop change](#5-cap-and-time-loop).
- Regridding to the requested output grid is handled automatically by
  the framework connection mechanism, not by History itself.
- Arithmetic expressions involving multiple fields must be added as
  exports in the component hierarchy (but this can now be done via YAML
  config in any component rather than requiring code changes).
  `HistoryCollectionGridComp` can extract individual field names from
  expressions and supports collecting multiple fields from a single
  expression spec.
- Output fields can include **vertical and ungridded dimensions**,
  enabling richer output without pre-processing.
- **Time accumulation** is supported natively: fields can be declared
  with an accumulation type (average, min, max, etc.) directly in the
  field spec and History3G will accumulate accordingly.
- History extends string templates to capture the "repeat count" of a
  clock, enabling output for each pass of a perpetual/spinup cycle to
  be written to a separate file.
- **Wildcard field specifications** are supported: a collection can
  match all exports whose names satisfy a pattern, allowing chemistry
  collections to automatically adapt as components add or remove
  exported fields.

> **[REVIEW NEEDED — Ben Auer]:** Please provide a reference example
> of the History3G YAML configuration (equivalent to `HISTORY.rc`) and
> document any breaking changes from the MAPL2 History format.

---

## 7. ExtData

### MAPL2 ExtData

ExtData in MAPL2 handled reading external data files, time
interpolation, and regridding to the model grid — all within the
ExtData component itself.

### MAPL3 ExtData

MAPL3 ExtData is a complete rewrite, though the user-facing interface
is similar to MAPL2 ExtData2G.  Many responsibilities previously
handled internally by ExtData have shifted to the framework:
regridding, time interpolation, and unit conversion are now handled
by the MAPL3 connection mechanism rather than within ExtData.

> **[REVIEW NEEDED — Ben Auer]:** Please document any breaking changes
> in the ExtData YAML configuration format between MAPL2 (ExtData2G)
> and MAPL3 ExtData.

---

## 8. Statistics Component

The Statistics component is **new in MAPL3**.  It encapsulates
time-averaging and related calculations that were previously performed
inside History in MAPL2.  Supported operations:

- Minimum / Maximum
- Time average
- Variance
- Covariance

Diurnal variance and covariance are expected to be added in future
releases.

---

## 9. Clocks

### MAPL2 Clocks

In MAPL2 a single clock is shared across the entire component
hierarchy.  While simple, this creates namespace issues for
user-specified alarms and limits the ability to run sub-components at
different rates.

### MAPL3 Clocks

In MAPL3, each component has its own distinct clock with its own set of
alarms.  Furthermore, a user component has a *separate* clock from the
generic component that wraps it.  This clean separation is expected to
simplify future features such as a reformulation of "replay".

MAPL3 also supports **perpetual clocks** for scenarios such as spinups
or repeated forcings for a constant year (e.g., a fixed-SST year).
History can write data for each repeat count to a separate file using
the extended string template syntax.

---

## 10. Build System

### Always Built as a Shared Library

In MAPL2 the `BUILD_SHARED_MAPL` CMake option controlled whether MAPL
was built as a shared or static library.  In MAPL3 this option has
been removed: **MAPL3 is always built as a shared library.**  This is
required to support run-time DSO loading of components.

### ESMF Version Requirement

MAPL3 requires **ESMF 8.8.0 or later**.  (MAPL2 used
`ESMF_Attribute` calls that are being deprecated; MAPL3 has migrated
all of these to `ESMF_Info` calls.)

### Command-Line Profiling Switches

Two new command-line switches activate global profiling (both default
to off):

| Switch | Effect |
|--------|--------|
| `--enable_global_timeprof` | Enable global wall-clock time profiling |
| `--enable_global_memprof` | Enable global memory allocation profiling (via `mallinfo`) |

Profile output is written to the `./profile` directory.

### FLAP Removed

Support for the FLAP command-line parsing library has been removed.
All executables now use **fArgParse**.  The `MAPL_FargparseCLI`
interface no longer accepts the old calling convention; the result
must be of type `MAPL_CapOptions`.

---

## 11. Logging (pFlogger)

Both MAPL2 and MAPL3 use the [pFlogger](https://github.com/Goddard-Fortran-Ecosystem/pFlogger)
library for structured, hierarchical logging.  The key difference is in
how each component's logger is named.

### MAPL2 — Full Hierarchy Name

In MAPL2, each gridded component receives a logger whose name is the
**full dot-separated path** of the component within the hierarchy.
For example, a dust component nested inside several parents would have
the logger name:

```
GCM.AGCM.PHYS.CHEM.GOCART2G.DU
```

This mirrors the ESMF component hierarchy and makes it straightforward
to set the log level for an entire subtree in the pFlogger resource
file: setting the level on `GCM.AGCM` automatically affects all
descendants.

### MAPL3 — Short (Final) Name

In MAPL3, each component's logger is identified by its **short name**
(the name passed to `MAPL_AddChild` / the component's own name) rather
than the full hierarchical path.  The same dust component is simply:

```
DU
```

**Trade-offs:**

| Aspect | MAPL2 (full name) | MAPL3 (short name) |
|--------|------------------|-------------------|
| Resource file complexity | More verbose; must list full paths | Simpler; just use the short component name |
| Setting level for a subtree | Easy — set level on a common prefix | Harder — each leaf must be configured individually |
| Name collisions across hierarchies | None (paths are unique) | Possible if two unrelated components share a short name |

**Migration note:** If your `logging.yaml` (or equivalent pFlogger
resource file) references loggers by their full hierarchical names, you
will need to update it to use short names when moving to MAPL3.

---

## 12. Grid and Geometry Access

### MAPL2 — Global Resolution Parameters

In MAPL2, components routinely queried global grid resolution parameters
to drive internal logic — allocating arrays, choosing numerical scheme
parameters, or tuning resolution-dependent constants.  The canonical
APIs were:

```fortran
call MAPL_GetGlobalHorzIJIndex(gc, im_world=im, jm_world=jm, ...)
call MAPL_Get(gc, IM_WORLD=im, JM_WORLD=jm, LM=lm, ...)
```

This practice has two serious failure modes:

- **Incorrect array sizing.** Components occasionally used global
  dimensions to allocate local work arrays, producing wrong results or
  crashes on domains smaller than the full grid.
- **Fragility under non-standard grids.** Global counts are
  well-defined for a uniform lat-lon grid but meaningless or misleading
  for stretched grids, variable-resolution grids, or single-column
  configurations.  Components that branch on `IM_WORLD` silently
  produce wrong results in these cases.

### MAPL3 — Local-Domain and Geometry-Based Approach

MAPL3 actively discourages querying global resolution parameters.
Components should operate only on their local domain; the framework
handles all decomposition.

In place of global counts, MAPL3 provides geometry-based accessors
that are correct for any grid type:

```fortran
! Cell areas via geom
call GeomGet(geom, area=area_field, rc=rc)

! Cell areas via grid
call GridGet(grid, area=area_field, rc=rc)
```

For cell areas specifically, MAPL2 components typically imported an
`AREA` export from `DYN`.  The `GeomGet`/`GridGet` area accessors make
this import unnecessary for most use cases, though importing from `DYN`
remains supported.

Additional accessors giving min/max cell spacing and area on the local
domain are planned, to allow components to determine
resolution-dependent parameters without relying on global counts:

| Accessor | Description |
|----------|-------------|
| `min_spacing` | Minimum angular/linear cell spacing on the local domain |
| `max_spacing` | Maximum angular/linear cell spacing on the local domain |
| `min_area` | Minimum cell area on the local domain |
| `max_area` | Maximum cell area on the local domain |

> **[REVIEW NEEDED]** The `min_spacing`, `max_spacing`, `min_area`, and
> `max_area` accessors are not yet implemented — see issue
> [#4570](https://github.com/GEOS-ESM/MAPL/issues/4570).  This section
> should be updated with the final call signatures once they land.

### Migration Guidance

- Replace any use of `MAPL_GetGlobalHorzIJIndex` or
  `MAPL_Get(..., IM_WORLD=..., JM_WORLD=...)` with the geometry-based
  accessors above.
- If your component uses global counts only to size a local work array,
  the local array bounds are already available from the field itself —
  no geometry query is needed at all.
- If your component uses global counts to select a resolution-dependent
  parameter (e.g. a diffusion coefficient), replace it with
  `min_spacing` or `max_spacing` once those accessors are available
  (issue [#4570](https://github.com/GEOS-ESM/MAPL/issues/4570)).

---

## 13. See Also

- [`api-changes.md`](api-changes.md) — detailed, procedure-level listing of
  every stubbed-out V2 API and the new MAPL3 replacements (lifecycle, child
  management, field specs, connectivity, resource access, timers).
- [`gridcomp-template.md`](gridcomp-template.md) — minimal Fortran template
  for a MAPL3 gridded component, with ACG spec file format and CMake
  integration reference.

---

## 14. Items Requiring Peer Review

The following sections need review or additional content from
subject-matter experts before this document should be considered
complete:

| Section | Reviewer | Outstanding Item |
|---------|----------|-----------------|
| [Variable specs — SERVICE type](#the-service-type-and-the-replacement-of-friendly) | @atrayano | Verify description of MAPL2 "friendly" mechanism; document how `SERVICE`/`SERVICE_PROVIDER`/`SERVICE_SUBSCRIBER` replace it in MAPL3 |
| [History3G](#6-history) | @bena-nasa | Provide reference History3G YAML config example; document breaking changes from `HISTORY.rc` |
| [ExtData](#7-extdata) | @bena-nasa | Document any breaking changes in ExtData YAML config between MAPL2 and MAPL3 |
| [Grid and Geometry Access](#12-grid-and-geometry-access) | @tclune | Implement `min_spacing`, `max_spacing`, `min_area`, `max_area` on `GeomGet`/`GridGet` (issue [#4570](https://github.com/GEOS-ESM/MAPL/issues/4570)); update call signatures here |
