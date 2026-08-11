# MAPL3 Child Component Creation - Quick Reference

## 12-Step Process Overview

### Step 1: Parent YAML Structure
**File:** `parent.yaml` or similar
```yaml
children:
  ChildName:
    dso: libcomponent_gridcomp
    config_file: path/to/child.yaml
    [timestep: <time_interval>]        # Optional
    [run_time_offset: <time_interval>] # Optional
```

### Step 2: GridCompCreate() - Create Parent
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/GenericGridComp.F90:87`
- Creates outer & inner ESMF_GridComps
- Stores HConfig in OuterMetaComponent.hconfig

### Step 3: GenericSetServices() - Called by ESMF
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/GenericGridComp.F90:34`
- Gets OuterMetaComponent from GridComp
- Calls outer_meta%SetServices_()

### Step 4: SetServices_() - Main Logic
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90:29`
- Parses parent YAML → parse_component_spec()
- Calls parent's user SetServices
- Adds children → add_children()
- Runs children's SetServices → run_children_setservices()

### Step 5: parse_component_spec() - Parse YAML
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_component_spec.F90:8`
- Navigates to `mapl:` section in HConfig
- Calls parse_children()

### Step 6: parse_children() - Iterate Children
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_children.F90:9`
- Navigates to `children:` mapping
- For each child name → parse_child()
- Returns ChildSpecMap

### Step 7: parse_child() - Parse Individual Child
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_child.F90:8`

**Reads from YAML:**
- `dso` → `sharedObj` (e.g., "libconfigurable_gridcomp")
- `SetServices` → `userProcedure` (default: "setservices_")
- `config_file` → Load child YAML → `child_hconfig`
- `timestep` (optional)
- `run_time_offset` (optional)

**Creates:**
- DSOSetServices object
- ChildSpec with all parameters

### Step 8: user_setservices() - Factory
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90:135`
- Creates DSOSetServices("sharedObj", "userProcedure")

### Step 9: add_child_by_spec() - Add to Parent
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/add_child_by_spec.F90:19`
- Merges parent & child HConfigs
- Creates child GridComp → GridCompCreate()
- Stores child in parent%children map

### Step 10: GridCompCreate() - Create Child
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/GenericGridComp.F90:87`
- Creates outer GridComp "[ChildName]"
- Creates inner GridComp "ChildName"
- Creates OuterMetaComponent with merged HConfig
- Same process as Step 2 (recursive!)

### Step 11: run_children_setservices() - SetServices on Children
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90:78`
- For each child in parent%children:
  - Calls ESMF_GridCompSetServices(child, GenericSetServices)
  - **Triggers child's SetServices_ recursively**

### Step 12: run_DSOSetServices() - Load DSO & Call User SetServices
**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90:151`
- Calls ESMF_GridCompSetServices with:
  - `sharedObj` = "libconfigurable_gridcomp"
  - `userRoutine` = "setservices_"
- ESMF loads DSO and calls the user's SetServices function

---

## Key Data Structures

### ChildSpec
```fortran
type :: ChildSpec
  class(AbstractUserSetServices), allocatable :: user_setservices
  type(ESMF_HConfig) :: hconfig
  type(ESMF_TimeInterval), allocatable :: timeStep
  type(ESMF_TimeInterval) :: offset
end type ChildSpec
```

### DSOSetServices
```fortran
type, extends(AbstractUserSetServices) :: DSOSetServices
  character(:), allocatable :: sharedObj      ! "libconfigurable_gridcomp"
  character(:), allocatable :: userRoutine    ! "setservices_"
end type DSOSetServices
```

### ComponentSpec
```fortran
type :: ComponentSpec
  type(GeometrySpec) :: geometry_spec
  type(VariableSpecVector) :: var_specs
  type(ConnectionVector) :: connections
  type(ChildSpecMap) :: children    ! <-- Contains ChildSpecs
  type(MiscellaneousComponentSpec) :: misc
end type ComponentSpec
```

### OuterMetaComponent
```fortran
type :: OuterMetaComponent
  type(ESMF_HConfig) :: hconfig                  ! Parent/child YAML
  type(GriddedComponentDriverMap) :: children    ! Children map
  type(ComponentSpec) :: component_spec          ! Parsed spec
  class(AbstractUserSetServices), allocatable :: user_setservices
end type OuterMetaComponent
```

---

## File Locations Summary

| Component | File | Line |
|-----------|------|------|
| Create GridComp | `generic/GenericGridComp.F90` | 87 |
| GenericSetServices entry | `generic/GenericGridComp.F90` | 34 |
| SetServices main logic | `generic/OuterMetaComponent/SetServices.F90` | 29 |
| Add children method | `generic/OuterMetaComponent/add_child_by_spec.F90` | 19 |
| Parse component spec | `generic/ComponentSpecParser/parse_component_spec.F90` | 8 |
| Parse children section | `generic/ComponentSpecParser/parse_children.F90` | 9 |
| Parse individual child | `generic/ComponentSpecParser/parse_child.F90` | 8 |
| Parse timespec | `generic/ComponentSpecParser/parse_timespec.F90` | 9 |
| UserSetServices factory | `generic/UserSetServices.F90` | 135 |
| Run DSO SetServices | `generic/UserSetServices.F90` | 151 |
| ChildSpec type | `generic/specs/ChildSpec.F90` | 16 |

---

## YAML Key Variants

DSO name (any one):
- `dso`
- `DSO`
- `sharedObj`
- `sharedobj`

SetServices routine (any one, optional):
- `SetServices`
- `setServices`
- `setservices`
- Default: `setservices_`

---

## Call Chain (Simplified)

```
GridCompCreate(parent_name, parent_setservices, parent_hconfig)
  └─ GenericSetServices(parent_gc)
      └─ outer_meta%SetServices_()
          ├─ parse_component_spec()
          │   └─ parse_children()
          │       └─ parse_child() for each child
          │           └─ user_setservices() creates DSOSetServices
          ├─ add_children()
          │   └─ add_child_by_spec() for each child
          │       └─ GridCompCreate(child_name, child_setservices, merged_hconfig)
          │           └─ [Recursive: child's GenericSetServices]
          └─ run_children_setservices()
              └─ ESMF_GridCompSetServices(child, GenericSetServices)
                  └─ [Triggers child's GenericSetServices recursively]
                      └─ run_DSOSetServices()
                          └─ ESMF loads DSO and calls user's SetServices
```

---

## Real Example Flow: parent.yaml → Child "A"

### Input Files
**parent.yaml:**
```yaml
children:
  A:
    dso: libconfigurable_gridcomp
    config_file: scenarios/precision_extension/A.yaml
```

**A.yaml:**
```yaml
mapl:
  states:
    export:
      E_A1:
        typekind: R4
```

### Execution Flow

1. Load `parent.yaml` as HConfig
2. Create parent GridComp with HConfig
3. Call GenericSetServices on parent
4. Parse `parent.yaml`: navigate to root level (no `mapl:`)
5. Find `children:` section, iterate child names
6. Parse child "A":
   - Read `dso: libconfigurable_gridcomp`
   - Read `config_file` → load `A.yaml` into `child_hconfig`
   - Create `DSOSetServices("libconfigurable_gridcomp", "setservices_")`
   - Create `ChildSpec {user_setservices, child_hconfig}`
7. Insert into `ChildSpecMap["A"]`
8. Call `add_child("A", child_spec)`:
   - Merge HConfigs (parent + A.yaml)
   - Create child GridComp "A" with merged HConfig
   - Store in `parent%children["A"]`
9. Call SetServices on child "A":
   - Parse merged HConfig: navigate to `mapl:` section
   - Parse `states:` section → find export `E_A1`
   - Call user's SetServices from DSO
10. Child has no children → returns
11. Parent SetServices completes

---

## Key Points

1. **YAML → HConfig**: ESMF_HConfigCreate loads YAML files
2. **Two formats**: Top-level `children:` OR under `mapl:` section
3. **DSO Loading**: Happens in ESMF when user's SetServices called
4. **HConfig Merging**: Parent settings apply to child
5. **Recursive**: Each child can have children (depth-first)
6. **Default SetServices**: `setservices_` if not specified

