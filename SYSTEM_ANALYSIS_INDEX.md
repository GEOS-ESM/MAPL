# Child Component Creation System - Complete Analysis Index

This analysis explores how child gridcomponents are currently created in two systems:
1. **GEOSgcm** - Using MAPL_AddChild (old Fortran-based approach)
2. **MAPL3** - Using YAML-based configuration (new approach)

## Documents in This Analysis

### 1. CURRENT_CHILD_COMPONENT_SYSTEM.md (749 lines)
**Comprehensive reference with code snippets and examples**

Contains:
- Part 1: GEOSgcm System (MAPL_AddChild approach)
  - Parent component: GEOSphysics with 6 children
  - Child example 1: Gravity Wave Drag (GWD)
  - Child example 2: GOCART with Dust (DU2G)
  
- Part 2: MAPL3 System (YAML-based)
  - Parent YAML configurations (3 examples)
  - Child YAML configurations (3 examples)
  - YAML parsing implementation code
  - ChildSpec data structures
  - UserSetServices framework
  - SetServices parsing implementation
  
- Part 3: Key Differences Summary (comparison table)
- Part 4: Child Component YAML Format Reference
- Part 5: File Paths and Line References

**Use this document when:**
- You need detailed code examples
- You want to understand the full implementation
- You need to see side-by-side YAML examples
- You're looking for specific code locations with line numbers

### 2. QUICK_REFERENCE.md (168 lines)
**Quick lookup tables and concise comparisons**

Contains:
- File Locations Quick Lookup (tables with paths and line numbers)
- System Comparison at a Glance
- Child Creation Method comparison
- SetServices Specification comparison
- Configuration Files comparison
- SetServices Signature comparison
- Key Code Flows (both systems)
- YAML Parsing Key Aspects
- Common Child YAML Keys
- Dust Component Example (GEOSgcm vs MAPL3)
- Absolute File Paths for copy-paste

**Use this document when:**
- You need to find a specific file quickly
- You want a high-level comparison
- You need copy-paste file paths
- You're getting oriented in either system

---

## Quick Navigation

### Looking for GEOSgcm Examples?
- **Parent creation pattern**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 1.1
- **GWD component**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 1.2
- **Dust (GOCART) component**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 1.3
- **Quick comparison**: QUICK_REFERENCE.md, Dust Component Example section

### Looking for MAPL3 Examples?
- **Parent YAML examples**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 2.1
- **Child YAML examples**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 2.2
- **Parsing code**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Sections 2.3-2.6
- **YAML format reference**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 4

### Looking for File Paths?
- **All files with line numbers**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Section 5
- **Quick lookup tables**: QUICK_REFERENCE.md, sections on File Locations
- **Absolute paths for copy-paste**: QUICK_REFERENCE.md, bottom section

### Looking for Code Flows?
- **GEOSgcm flow**: QUICK_REFERENCE.md, Key Code Flows section
- **MAPL3 flow**: QUICK_REFERENCE.md, Key Code Flows section
- **Detailed MAPL3 flow with code**: CURRENT_CHILD_COMPONENT_SYSTEM.md, Sections 2.3-2.6

---

## Key Concepts at a Glance

### GEOSgcm System (Old - Fortran-Based)
```fortran
! Hard-coded in parent SetServices
GWD = MAPL_AddChild(GC, NAME='GWD', SS=GwdSetServices, RC=STATUS)
```
- SetServices routines imported as module procedures
- No YAML configuration
- Children created directly in SetServices code
- Config via .rc files (ESMF_Config)

### MAPL3 System (New - YAML-Based)
```yaml
# In parent YAML
children:
  GWD:
    dso: libgwd_gridcomp
    config_file: GWD_GridComp.yaml
    setServices: setservices_  # optional, defaults to setservices_
```
- SetServices specified in YAML
- Dynamically loaded from shared objects (DSO)
- Config in hierarchical YAML files
- Parsed by ESMF_HConfig infrastructure

---

## File Structure Reference

### GEOSgcm Hierarchy
```
GEOSphysics (parent)
├── GWD (Gravity Wave Drag)
├── MOIST
├── TURBULENCE
├── CHEMISTRY
│   └── GOCART (parent)
│       ├── DU2G (Dust)
│       ├── SS2G (Sea Salt)
│       ├── SU2G (Sulfate)
│       ├── CA2G (Carbonaceous)
│       └── NI2G (Nitrate)
├── SURFACE
└── RADIATION
```

### MAPL3 Structure (Conceptual)
```
Parent Component (parent.yaml)
├── Child A (A.yaml)
│   └── [States, SetServices, Geometry]
├── Child B (B.yaml)
│   └── [States, SetServices, Geometry]
└── [Grid, Connections, States]
```

---

## Implementation Timeline

### When to Use GEOSgcm Approach
- Legacy systems
- Static component hierarchies
- When all components are available at compile time
- Strong coupling with parent logic

### When to Use MAPL3 Approach
- New systems
- Configurable hierarchies (runtime selection)
- Pluggable components (optional at runtime)
- Dynamic loading of components
- Configuration-driven systems

---

## Key Parsing Mechanisms

### GEOSgcm
1. Parent imports child SetServices via Fortran USE statement
2. Parent calls MAPL_AddChild with routine pointer
3. ESMF initializes child during Initialize phase
4. Child loads .rc config via ESMF_ConfigLoadFile

### MAPL3
1. Parent YAML parsed by ESMF_HConfig
2. parse_component_spec extracts children section
3. For each child: parse_child creates ChildSpec with:
   - DSOSetServices (sharedObj + routine name)
   - child hconfig (from config_file)
   - timeStep/offset if specified
4. add_children creates ESMF_GridComp for each child
5. run_children_setservices dynamically loads DSO and calls routine

---

## Default Values

### MAPL3 YAML Defaults
| Field | Default | Required |
|-------|---------|----------|
| `dso` | (none) | YES |
| `setServices` routine | `setservices_` | NO |
| `config_file` | (none) | NO |
| `timestep` | (none) | NO |
| `run_time_offset` | (none) | NO |

### SetServices Routine Name
- GEOSgcm: Always `SetServices` (fixed)
- MAPL3: Default `setservices_`, but tries multiple spellings:
  - `SetServices`, `setServices`, `setservices`

### DSO Name Aliases (MAPL3)
YAML accepts multiple key names for DSO:
- `dso` (lowercase)
- `DSO` (uppercase)  
- `sharedObj` (ESMF style)
- `sharedobj` (lowercase variant)

---

## Testing and Examples

### GEOSgcm Test Components
- Primary: GEOSphysics with 6 children
- Aerosol module: GOCART with multiple instances

### MAPL3 Test Components
Located in: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/`

Key scenarios:
- `parent.yaml` - Simple 2-child system
- `3d_specs/` - Parent and children with 3D state variables
- `vertical_alignment_with_grid/` - Geometry specifications
- `statistics/` - SetServices override example

---

## Related Source Files

### Core MAPL3 Infrastructure
- ESMF_HConfig wrapper (ESMF library)
- StateRegistry (field metadata)
- OuterMetaComponent (generic component wrapper)
- GriddedComponentDriver (child management)

### Key Modules
- `mapl_ComponentSpecParser_mod` - YAML to spec conversion
- `mapl_UserSetServices_mod` - SetServices handler factory
- `mapl_OuterMetaComponent_mod` - Main component framework

---

## Troubleshooting Guide

### "Must specify a dso for hconfig of child"
- **Cause**: Child YAML missing DSO specification
- **Fix**: Add `dso: libname` or `sharedObj: libname` to child definition
- **See**: parse_child.F90 lines 39-40

### "must be mapping" error for children
- **Cause**: children section is not a YAML map (dict)
- **Fix**: Ensure children has name: config pairs, not a list
- **See**: parse_children.F90 line 29

### SetServices routine not found
- **Cause**: DSO loaded but routine name doesn't exist
- **Fix**: Check `setServices: routine_name` matches DSO export
- **See**: parse_child.F90 lines 42-54

---

## For Developers Migrating from GEOSgcm to MAPL3

### Changes Required
1. Move hard-coded MAPL_AddChild calls to YAML
2. Convert .rc files to YAML format
3. Update child SetServices to be dynamically callable
4. Update DSO exports to match configured routine names

### Compatibility Notes
- MAPL3 can still call traditional SetServices(GC, RC)
- SetServices signature is unchanged
- Only calling mechanism differs (dynamic vs imported)

### Migration Path
1. Write YAML configurations
2. Update child components if using non-standard SetServices names
3. Remove MAPL_AddChild calls from parent
4. Test with parse_component_spec

---

## Additional References

- **ESMF_HConfig documentation**: ESMF library docs
- **MAPL_Generic documentation**: See MAPL3 docs/
- **YAML format**: YAML.org specification
- **ESMF GridComponent**: ESMF reference manual

---

## Document Versions

- Generated: August 7, 2026
- MAPL repository: develop branch
- GEOSgcm: July 23, 2024 version
- Analysis type: Complete system comparison with code examples

---

## How to Use These Documents

1. **First time?** Read QUICK_REFERENCE.md for overview
2. **Need details?** Dive into CURRENT_CHILD_COMPONENT_SYSTEM.md
3. **Looking for files?** Check file path sections in either document
4. **Comparing systems?** See comparison tables in QUICK_REFERENCE.md
5. **Understanding code flow?** Read sections 2.3-2.6 in CURRENT_CHILD_COMPONENT_SYSTEM.md

---

End of Index

