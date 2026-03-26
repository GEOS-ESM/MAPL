# MAPL3 GridComp Template

This document provides a minimal template for a MAPL3 gridded component
(gridcomp) and explains the supporting tools and conventions.

---

## Table of Contents

1. [Fortran Template](#1-fortran-template)
2. [Automatic Code Generator (ACG)](#2-automatic-code-generator-acg)
3. [CMake Integration](#3-cmake-integration)

---

## 1. Fortran Template

```fortran
! Import MAPL-provided macros
#include "MAPL.h"

module MyGridComp
   use mapl  ! provides all MAPL *and* ESMF public entities
   implicit none(type,external)

   private

   public :: setServices

contains

   ! Note: ESMF interfaces for setServices, initialize, and run are
   ! now strictly enforced.
   subroutine setServices(gridcomp, rc)
      type(esmf_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: hconfig

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, initialize, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! ACG-generated calls to declare import, export, and internal fields
#include "acg_import.h"
#include "acg_export.h"
#include "acg_internal.h"

      call mapl_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      ! process resource file
      ! add connections
      ! ...

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine initialize(gridcomp, importState, exportState, clock, rc)
      type(esmf_GridComp) :: gridcomp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      ! ACG-generated pointer declarations and field assignments
#include "acg_declare.h"
#include "acg_assign.h"

      ! ... component initialization logic ...

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(esmf_GridComp) :: gridcomp
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      ! ACG-generated pointer declarations and field assignments
#include "acg_declare.h"
#include "acg_assign.h"

      ! ... component run logic ...

      _RETURN(_SUCCESS)
   end subroutine run

end module MyGridComp

! External wrapper procedure required for ESMF DSO interface.
! This free procedure is the symbol that ESMF resolves when loading
! the shared library; it simply delegates to the module procedure.
subroutine setServices(gridcomp, rc)
   use MyGridComp, only: mySetServices => setServices
   type(esmf_GridComp) :: gridcomp
   integer, intent(out) :: rc

   integer :: status
   call mySetServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
```

### Key points

- **`use mapl`** — a single `use` statement brings in all public MAPL
  and ESMF entities.  No separate `use ESMF` is needed.
- **`implicit none(type,external)`** — MAPL3 enforces this stricter
  form of `implicit none` throughout.  It requires explicit declaration
  of all external procedures in addition to all variables.
- **`#include "MAPL.h"`** — provides the error-handling macros (`_RC`,
  `_RETURN`, `_ASSERT`, etc.).  Must appear before the `module`
  statement.
- **Free `setServices` wrapper** — ESMF's DSO loader resolves a
  module-external (free) procedure named `setservices_` (lower-case,
  trailing underscore) by default.  The wrapper delegates immediately
  to the module procedure, keeping all logic inside the module.
- **`_RC` / `_RETURN(_SUCCESS)`** — see the
  [MAPL error handling guide](../../.opencode/skills/mapl-error-handling/SKILL.md)
  for the full macro reference.

---

## 2. Automatic Code Generator (ACG)

### Background

The ACG (`MAPL_GridCompSpecs_ACGv3.py`, located in `Apps/`) is not new
to MAPL3, but its use is now strongly encouraged — and in practice
**required** — when porting GEOS components to MAPL3.  The explicit
`MAPL_AddImportSpec` / `MAPL_AddExportSpec` / `MAPL_AddInternalSpec`
calls from MAPL2 are stubbed out in MAPL3 and will abort at runtime
(see [`api-changes.md`](api-changes.md)).  The ACG replaces them.

### What the ACG does

Given a spec file (`MyGridComp.acg`), the ACG generates up to six
header files that are `#include`d directly into the Fortran source:

| Header | Included in | Purpose |
|--------|-------------|---------|
| `acg_import.h` | `setServices` | Calls to declare each import field |
| `acg_export.h` | `setServices` | Calls to declare each export field |
| `acg_internal.h` | `setServices` | Calls to declare each internal field |
| `acg_declare.h` | `initialize`, `run` | Fortran pointer declarations for all fields |
| `acg_assign.h` | `initialize`, `run` | Assignments that bind each pointer to its ESMF field |

### Spec file format

A `.acg` spec file uses a pipe-delimited table with a `schema_version`
header and one `category:` block per state (IMPORT, EXPORT, INTERNAL).
Columns vary by category; the full set of supported columns is:

| Column | Applies to | Description |
|--------|-----------|-------------|
| `SHORT_NAME` | all | Field short name (used in MAPL state and as the default Fortran pointer name) |
| `UNITS` | all | Physical units string |
| `DIMS` | all | Spatial dimensions: `xy`, `z`, `xyz`, or `–` for scalar |
| `VSTAGGER` | all | Vertical stagger: `C` (center), `E` (edge), `N` (none) |
| `LONG_NAME` / `STANDARD_NAME` | all | CF-style descriptive name; prefix `*` to use as-is |
| `ALIAS` | all | Alternative Fortran pointer name for this field |
| `PREC` | EXPORT, INTERNAL | Precision: `R32` (default) or `R64` |
| `UNGRIDDED_DIMS` | all | Extra non-horizontal dimensions |
| `RESTART` | INTERNAL | Restart behavior: `true`, `false`, or `SKIP` |
| `COND` | INTERNAL | Condition expression controlling field activation |
| `STATE` | INTERNAL | State intent override |
| `add_to_export` | INTERNAL | Automatically re-export this internal field |

A minimal example (`MyGridComp.acg`):

```
schema_version: 2.0.0
component: MyGridComp

category: IMPORT
SHORT_NAME | UNITS | DIMS | VSTAGGER | STANDARD_NAME
U          | m s-1 | xyz  | C        | eastward_wind
V          | m s-1 | xyz  | C        | northward_wind

category: EXPORT
SHORT_NAME | UNITS | DIMS | VSTAGGER | STANDARD_NAME | PREC | RESTART
T          | K     | xyz  | C        | air_temperature | R32 | true
```

### Running the ACG

```sh
MAPL_GridCompSpecs_ACGv3.py MyGridComp.acg
```

This generates the `acg_*.h` files in the current directory.  In
practice the ACG is invoked automatically by CMake (see below).

---

## 3. CMake Integration

The `mapl_acg()` CMake function (defined in `cmake/mapl_acg.cmake`)
runs the ACG at build time and wires the generated headers into the
compile step automatically:

```cmake
mapl_acg(MyGridComp.acg)

target_sources(MyTarget PRIVATE MyGridComp.F90)
```

The `mapl_acg()` call adds a custom command that regenerates the
`acg_*.h` files whenever `MyGridComp.acg` changes, and adds the output
directory to the include path so the `#include "acg_*.h"` lines in the
Fortran source resolve correctly.
