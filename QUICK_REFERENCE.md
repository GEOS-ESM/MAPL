# Quick Reference: Child Component Creation Systems

## File Locations Quick Lookup

### GEOSgcm System

| Component | Type | Location |
|-----------|------|----------|
| GEOSphysics Parent | SetServices | `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOS_PhysicsGridComp.F90` (lines 150-160) |
| GWD (Gravity Wave Drag) | Child SetServices | `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/GEOS_GwdGridComp.F90` (lines 88-100) |
| GWD | Config File | `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/GWD_GridComp.rc` |
| GOCART (Dust Parent) | SetServices | `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/@GOCART/ESMF/GOCART2G_GridComp/GOCART2G_GridCompMod.F90` (lines 1310-1314) |
| Dust (DU2G) | Child SetServices | `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/@GOCART/ESMF/GOCART2G_GridComp/DU2G_GridComp/DU2G_GridCompMod.F90` (lines 54-75) |

### MAPL3 System

| Function | Location | Lines |
|----------|----------|-------|
| SetServices Main Entry | `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90` | 29-100 |
| add_children Subroutine | `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90` | 54-74 |
| run_children_setservices | `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90` | 78-98 |
| parse_children Function | `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_children.F90` | 9-45 |
| parse_child Function | `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_child.F90` | 8-70 |
| ChildSpec Type | `/Users/wdboggs/src/MAPL/superstructure/generic/specs/ChildSpec.F90` | 16-24 |
| UserSetServices Base | `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90` | 30-35 |
| DSOSetServices Type | `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90` | 71-77 |
| new_DSOSetServices | `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90` | 135-149 |
| parse_setservices | `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_setservices.F90` | 10-30 |

### Example YAML Files

| Example | Type | Location |
|---------|------|----------|
| Simple Parent | Parent | `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/parent.yaml` |
| Simple Child | Child | `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/A.yaml` |
| Parent with Geometry | Parent | `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/3d_specs/parent.yaml` |
| Child with Geometry | Child | `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/vertical_alignment_with_grid/A.yaml` |
| Parent with SetServices | Parent | `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/root.yaml` |

---

## System Comparison at a Glance

### Child Creation Method

**GEOSgcm:**
```fortran
GWD = MAPL_AddChild(GC, NAME='GWD', SS=GwdSetServices, RC=STATUS)
```

**MAPL3:**
```yaml
children:
  GWD:
    dso: libgwd_gridcomp
    config_file: GWD_GridComp.yaml
```

---

### SetServices Specification

**GEOSgcm:**
- Hard-coded in Fortran module imports
- SetServices routine name is fixed (always `SetServices`)
- Imported as: `use MODULE, only : SetServices`

**MAPL3:**
- Specified in YAML under child definition
- Routine name configurable (default: `setservices_`)
- Loaded dynamically from DSO

**MAPL3 YAML Syntax:**
```yaml
children:
  MyChild:
    dso: libmy_gridcomp          # Required: shared object
    setServices: my_custom_ss    # Optional: routine name
    config_file: MyChild.yaml    # Optional: child's config file
```

---

### Configuration Files

**GEOSgcm:**
- Component-specific .rc files
- Loaded using `ESMF_ConfigLoadFile()`
- Example: `GWD_GridComp.rc`, `DU2G_instance_DU.rc`

**MAPL3:**
- YAML files (HConfig format)
- Hierarchical: parent.yaml contains/references child.yaml
- Example: parent defines grid and children section, each child has separate YAML

---

### SetServices Signature

**GEOSgcm:**
```fortran
subroutine SetServices(GC, RC)
    type(ESMF_GridComp), intent(INOUT) :: GC
    integer, optional :: RC
end subroutine
```

**MAPL3:**
- Same signature, but:
- Routine name is configurable
- Called dynamically via DSOSetServices wrapper
- Routine is loaded from shared object at runtime

---

## Key Code Flows

### GEOSgcm Flow
1. Parent SetServices imports child SetServices as module procedure
2. Parent calls `MAPL_AddChild()` with imported SS routine
3. Each child is identified and created
4. Child module's SetServices is called during ESMF initialization

### MAPL3 Flow
1. Parent YAML specifies children in `children:` section
2. `parse_component_spec()` parses parent YAML
3. `parse_children()` extracts children map
4. `parse_child()` creates ChildSpec with:
   - `user_setservices` (DSOSetServices) with sharedObj and routine name
   - `hconfig` (loaded from child's config_file)
   - `timeStep` and `offset` (if specified)
5. `SetServices_()` calls `add_children()` to create each child
6. `run_children_setservices()` calls ESMF_GridCompSetServices on each child
7. Child's SetServices is dynamically loaded from DSO and executed

---

## YAML Parsing Key Aspects

### What parse_child Looks For

1. **DSO Name** (tries multiple key names):
   - `dso`, `DSO`, `sharedObj`, `sharedobj`
   - REQUIRED

2. **SetServices Routine** (tries multiple key names):
   - `SetServices`, `setServices`, `setservices`
   - OPTIONAL (defaults to `setservices_`)

3. **Config File**:
   - `config_file` key
   - OPTIONAL
   - If specified, loaded as ESMF_HConfig from file

4. **Timing** (via parse_timespec):
   - `timestep` key
   - `run_time_offset` key
   - Both OPTIONAL

---

## Common Child YAML Keys

| Key | Type | Required | Example |
|-----|------|----------|---------|
| `dso` | string | Yes | `libconfigurable_gridcomp` |
| `setServices` | string | No | `setservices_` (default) |
| `config_file` | string | No | `scenarios/A.yaml` |
| `timestep` | string | No | `00300` (5 minutes) |
| `run_time_offset` | string | No | `00000` |

---

## Dust Component Example: GEOSgcm vs MAPL3

### In GEOSgcm
```fortran
! GOCART2G_GridCompMod.F90
use DU2G_GridCompMod, only : DU2G_setServices => SetServices

call addChildren__ (gc, self%DU, setServices=DU2G_setServices, __RC__)

contains
    subroutine addChildren__(gc, species, setServices, rc)
        do i = 1, n
            species%instances(i)%id = MAPL_AddChild(gc, name=species%instances(i)%name, SS=SetServices, __RC__)
        end do
    end subroutine
```

### In MAPL3 (Conceptually)
```yaml
# parent.yaml
mapl:
  children:
    DU1:
      dso: libdu2g_gridcomp
      config_file: dust1.yaml
    DU2:
      dso: libdu2g_gridcomp
      config_file: dust2.yaml
      
# dust1.yaml (child config)
mapl:
  states:
    export:
      DU_conc: {...}
  setServices:
    sharedObj: libdu2g_gridcomp
    userRoutine: setservices_
```

---

## Absolute File Paths (for copy-paste)

```
GEOSgcm Physics Parent:
/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOS_PhysicsGridComp.F90

MAPL3 SetServices:
/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90

MAPL3 Parse Children:
/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_children.F90

MAPL3 Example Parent YAML:
/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/parent.yaml

MAPL3 Example Child YAML:
/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/A.yaml
```

