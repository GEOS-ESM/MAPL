# Core Framework API Changes: MAPL2 to MAPL3

This document covers changes to the core MAPL framework API between
MAPL2 and MAPL3.  Framework components (History, ExtData, Statistics,
etc.) have their own separate documentation.

> **Alpha caveat:** MAPL3 is in active development.  Some interfaces
> documented here may change before the stable release.  In particular,
> the new MAPL3-native APIs (Section 2) should be treated as
> provisional.

---

## Table of Contents

1. [Broken V2 API — Stubbed Out](#1-broken-v2-api--stubbed-out)
2. [New MAPL3 Framework Entry Points](#2-new-mapl3-framework-entry-points)
3. [Lifecycle Procedures](#3-lifecycle-procedures)
4. [Child Management](#4-child-management)
5. [Field and State Specifications](#5-field-and-state-specifications)
6. [Connectivity](#6-connectivity)
7. [Resource Access](#7-resource-access)
8. [Timers and Profiling](#8-timers-and-profiling)
9. [Miscellaneous Changes](#9-miscellaneous-changes)

---

## 1. Broken V2 API — Stubbed Out

The following V2 procedures are **present in MAPL3 for link
compatibility only**.  They all immediately call `_FAIL("Time to port
to MAPL3")` and will abort at run time if called.  Code that uses any
of these must be ported before it will work with MAPL3.

The stubs are defined in `generic/MAPL_Generic_Stubs.F90`.

### Framework lifecycle

| V2 Procedure | Notes |
|---|---|
| `MAPL_GenericSetServices` | Replaced — see [Section 3](#3-lifecycle-procedures) |
| `MAPL_GenericInitialize` | Replaced — see [Section 3](#3-lifecycle-procedures) |
| `MAPL_GenericRunChildren` | Framework handles this automatically |
| `MAPL_GenericFinalize` | Replaced — see [Section 3](#3-lifecycle-procedures) |
| `MAPL_GenericRunCouplers` | Framework handles this automatically |
| `MAPL_GenericRefresh` | Not yet ported |
| `MAPL_GenericRecord` | Replaced by Statistics component and History3G |
| `MAPL_GenericStateSave` | Not yet ported |
| `MAPL_GenericStateRestore` | Not yet ported |

### State and spec access

| V2 Procedure | Notes |
|---|---|
| `MAPL_Get` (via `MAPL_GenericStateGet`) | Stubbed — MAPL3 equivalent differs; see [Section 3](#3-lifecycle-procedures) |
| `MAPL_Set` (via `MAPL_GenericStateSet`) | Stubbed |
| `MAPL_InternalStateRetrieve` | Stubbed |
| `MAPL_GetObjectFromGC` (via `MAPL_InternalStateGet`) | Stubbed |
| `MAPL_InternalESMFStateGet` | Stubbed |
| `MAPL_ImportStateGet` | Returns `ESMF_FAILURE` |
| `MAPL_ExportStateGet` | Returns `ESMF_FAILURE` |
| `MAPL_StateCreateFromSpec` | Stubbed |
| `MAPL_StateCreateFromSpecNew` | Stubbed |
| `MAPL_SetStateSave` | Stubbed |
| `MAPL_DestroyStateSave` | Stubbed |
| `MAPL_GCGet` | Stubbed |

### Field specification

| V2 Procedure | Notes |
|---|---|
| `MAPL_AddImportSpec` | Stubbed — use MAPL3 ACG or `MAPL_GridCompAddFieldSpec` |
| `MAPL_AddExportSpec` | Stubbed — use MAPL3 ACG or `MAPL_GridCompAddFieldSpec` |
| `MAPL_AddInternalSpec` | Stubbed — use MAPL3 ACG or `MAPL_GridCompAddFieldSpec` |
| `MAPL_SetVarSpecForCC` | Stubbed |

### Child management

| V2 Procedure | Notes |
|---|---|
| `MAPL_AddChild` (all overloads) | Stubbed — see [Section 4](#4-child-management) |
| `MAPL_GetChildLocstream` | Returns `ESMF_FAILURE` |

### Connectivity

| V2 Procedure | Notes |
|---|---|
| `MAPL_AddConnectivity` | Stubbed — see [Section 6](#6-connectivity) |
| `MAPL_TerminateImport` | Stubbed — see [Section 6](#6-connectivity) |
| `MAPL_DoNotDeferExport` | Stubbed |
| `MAPL_DoNotAllocateImport` | Stubbed |
| `MAPL_DoNotAllocateInternal` | Stubbed |

### Friendly mechanism

| V2 Procedure | Notes |
|---|---|
| `MAPL_FriendlyGet` | Stubbed — replaced by `SERVICE` state item type |
| `MAPL_GridCompGetFriendlies` (all overloads) | Stubbed — replaced by `SERVICE` state item type |
| `MAPL_CopyFriendliness` | Stubbed |
| `MAPL_VerifyFriendly` | Stubbed |
| `MAPL_GenericMakeXchgNatural` | Stubbed |

### Timers and alarms

| V2 Procedure | Notes |
|---|---|
| `MAPL_TimerOn` | Stubbed — see [Section 8](#8-timers-and-profiling) |
| `MAPL_TimerOff` | Stubbed — see [Section 8](#8-timers-and-profiling) |
| `MAPL_TimerAdd` | Stubbed — see [Section 8](#8-timers-and-profiling) |
| `MAPL_StateAlarmAdd` | Stubbed |
| `MAPL_StateAlarmGet` | Stubbed |
| `MAPL_AddRecord` | Stubbed |
| `MAPL_DisableRecord` | Stubbed |
| `MAPL_RecordAlarmIsRinging` | Stubbed |

### Miscellaneous

| V2 Procedure | Notes |
|---|---|
| `MAPL_GetResource` (all overloads taking `MAPL_MetaComp` or `ESMF_Config`) | Stubbed — see [Section 7](#7-resource-access) |
| `MAPL_ReadForcing` | Stubbed |
| `MAPL_GridCreate` | Stubbed |
| `MAPL_GridCompSetEntryPoint` | Stubbed |
| `MAPL_MethodAdd` | Stubbed |
| `MAPL_DateStampGet` | Stubbed |
| `MAPL_ExchangeGridGet` | Stubbed |
| `MAPL_ExchangeGridSet` | Stubbed |
| `MAPL_CheckpointState` / `MAPL_ESMFStateWriteToFile` | Stubbed |
| `MAPL_ESMFStateReadFromFile` | Stubbed |
| `MAPL_AddAttributeToFields` | Stubbed |
| `MAPL_GetLogger` | Stubbed |
| `MAPL_GetAllExchangeGrids` | Stubbed |

---

## 2. New MAPL3 Framework Entry Points

### `MAPL_initialize` and `MAPL_finalize`

In MAPL2, framework initialization was done implicitly via ESMF.
MAPL3 provides explicit entry points that initialize the full framework
stack (ESMF, pflogger, profilers, udunits2, I/O servers) from a single
YAML-based configuration:

```fortran
! MAPL3
use mapl3g_MaplFramework, only: MAPL_initialize, MAPL_finalize

call MAPL_initialize(configFilenameFromArgNum=1, is_model_pet=is_model, rc=status)
! ... run model ...
call MAPL_finalize(rc=status)
```

The `configFilenameFromArgNum` argument reads the YAML config filename
from the specified command-line argument position (typically 1).

The `is_model_pet` output identifies whether the calling PET belongs to
the model communicator (as opposed to an I/O server PET).  PETs for
which `is_model_pet = .false.` should return immediately after
`MAPL_initialize`.

### `MaplFramework` type

`MaplFramework` is the underlying derived type that the module-level
`MAPL_initialize`/`MAPL_finalize` procedures operate on via a private
singleton.  Advanced use cases can instantiate `MaplFramework` directly
to avoid the singleton pattern.

---

## 3. Lifecycle Procedures

### SetServices

In MAPL2, a user component's `SetServices` called
`MAPL_GenericSetServices` to register the generic IRF entry points,
then optionally overrode individual phases.

In MAPL3, `MAPL_GenericSetServices` is stubbed out and will abort.
Instead, `SetServices` is driven by the `MaplGeneric` layer
automatically.  User components interact with the framework via the
`OuterMetaComponent` object passed through the ESMF internal state —
they do not call `MAPL_GenericSetServices` themselves.

### Initialize / Run / Finalize

In MAPL2, user components that needed to customize initialization
explicitly called `MAPL_GenericInitialize` (and similarly for run and
finalize).

In MAPL3, these calls are **stubbed and will abort**.  The framework
drives all phases automatically.  User components implement one or more
of the well-defined phase callbacks (e.g., `GENERIC_INIT_USER`) rather
than calling generic lifecycle procedures directly.

See `diffs-from-mapl2.md` Section 1 for the full list of MAPL3
initialization phases.

### `MAPL_Get` / `MAPL_Set`

In MAPL2, `MAPL_Get` and `MAPL_Set` accessed fields of `MAPL_MetaComp`
(grid dimensions, clock, orbit, alarms, etc.).  Both are stubbed in
MAPL3.

The MAPL3 equivalents are accessed through the `OuterMetaComponent`
object.  The specific replacement API is still stabilizing.

> **[REVIEW NEEDED]:** Document the MAPL3 replacement for `MAPL_Get`
> and `MAPL_Set` once the `OuterMetaComponent` accessor API stabilizes.

---

## 4. Child Management

### V2: `MAPL_AddChild` returns an integer ID

```fortran
! MAPL2
integer :: child_id
child_id = MAPL_AddChild(gc, name='MYCOMP', SS=MyComp_SetServices, rc=status)
! Later:
call MAPL_AddConnectivity(gc, child=child_id, ...)
```

### V3: Children referenced by name

In MAPL3, `MAPL_AddChild` is **stubbed and will abort**.  Children are
instead specified by name in the `mapl` section of the parent
component's HConfig YAML (for DSO-based components) or registered
through the MAPL3 component registration API.

Child references in connectivity calls use the child's string name
rather than an integer ID.

```yaml
# MAPL3: child registration in HConfig (cap.yaml or component config)
mapl:
  children:
    MYCOMP:
      dso: libmycomp_gc
      config_file: mycomp.yaml
```

---

## 5. Field and State Specifications

### V2: `MAPL_AddImportSpec`, `MAPL_AddExportSpec`, `MAPL_AddInternalSpec`

These explicit API calls are **stubbed in MAPL3 and will abort**.

### V3: `MAPL_GridCompAddFieldSpec` and ACG3

MAPL3 uses a different procedure for field registration.  The preferred
path remains the Automatic Code Generator (ACG), which now targets
MAPL3 procedures and uses `.acg` spec files.

The ACG3 spec format gains the following new columns relative to MAPL2:

| Column | Description |
|--------|-------------|
| `ALIAS` | Alternative name for the field |
| `add2export` | Automatically promote an internal field to the export state |
| `RESTART` | Enumerated restart behavior (replaces free-form string) |

The `RESTART` column now uses named enumerators (defined in
`field/RestartModes.F90`) rather than integer literals.

---

## 6. Connectivity

### `MAPL_AddConnectivity`

Stubbed in MAPL3.  Connectivity between components is now expressed
in the HConfig YAML.  The framework supports connections between
mismatched fields (regridding, unit conversion, precision conversion)
automatically.

> **[REVIEW NEEDED]:** Document the MAPL3 YAML connectivity syntax
> once it stabilizes.

### `MAPL_TerminateImport`

Stubbed in MAPL3.  Unsatisfied imports are handled by the framework
connection rules.  The MAPL3 equivalent mechanism is under development.

> **[REVIEW NEEDED]:** Document the MAPL3 equivalent for suppressing
> unsatisfied import warnings/errors.

---

## 7. Resource Access

### V2: `MAPL_GetResource` from `MAPL_MetaComp` or `ESMF_Config`

These overloads are **stubbed in MAPL3 and will abort**.

### V3: `MAPL_GetResource` from `ESMF_HConfig`

MAPL3 provides a new overload of `MAPL_GetResource` that accepts an
`ESMF_HConfig` object directly, without requiring a `HConfigParams`
wrapper:

```fortran
! MAPL3
use mapl_HConfigUtils, only: MAPL_GetResource
call MAPL_GetResource(hconfig, val=my_var, label='my_key', default=default_val, rc=status)
```

The `hconfig` is obtained from the component's YAML configuration file,
which is passed to the component via the framework.

---

## 8. Timers and Profiling

### V2: `MAPL_TimerOn`, `MAPL_TimerOff`, `MAPL_TimerAdd`

These procedures are **stubbed in MAPL3 and will abort**.

### V3: Profiling

Profiling in MAPL3 is driven by the global profiler objects initialized
in `MaplFramework`.  The distributed time and memory profilers are
started/stopped automatically around framework-controlled phases.

User components that need custom timer regions should use the
`DistributedProfiler` API directly (available from
`mapl_Profiler`), or rely on the automatic instrumentation provided
by the framework wrappers.

Profile output is written to the `./profile` directory.  Global
profiling can be activated with `--enable_global_timeprof` and
`--enable_global_memprof` command-line switches.

---

## 9. Miscellaneous Changes

### `MAPL_MetaComp` type

`MAPL_MetaComp` is still defined and exported from
`MAPL_Generic_Stubs.F90` for link compatibility, but its use as the
primary framework state object is replaced by `OuterMetaComponent` in
MAPL3.  Code that directly accesses `MAPL_MetaComp` fields must be
ported.

### `MAPL_ConnectService`, `MAPL_AdvertiseService`, `MAPL_RequestService`

These V2 service/friendly procedures are present in MAPL2's
`MAPL_Generic.F90` but are not exported from the MAPL3 stubs module.
They are replaced by the `MAPL_STATEITEM_SERVICE` /
`SERVICE_PROVIDER` / `SERVICE_SUBSCRIBER` state item types.

See `diffs-from-mapl2.md` Section 3 for background on the service
mechanism.

### `MAPL_ReadForcing`

Stubbed.  Time-interpolated forcing data is now the responsibility of
ExtData.

### `MAPL_DateStampGet`

Stubbed.  Clock and time information in MAPL3 is accessed through the
per-component `ESMF_Clock` object associated with the component's
`OuterMetaComponent`.
