# MAPL3 Child Component Creation Trace - Documentation

This directory contains comprehensive documentation of how child components are created from parent components in MAPL3 (develop branch).

## Files Included

### 1. **MAPL3_Child_Component_Creation_Trace.md** (Main Document)
The most comprehensive document. Contains:
- Complete 12-step trace with code snippets
- Actual file paths and line numbers
- YAML examples from real test cases
- Data structure definitions
- Complete call chain summary
- Detailed execution timeline with all steps
- Data flow diagram
- Key data structures explained
- File summary table
- Complete example trace for parent.yaml → Child "A"

**Best for:** Understanding the complete flow with actual code

### 2. **MAPL3_Child_Component_Quick_Reference.md** (Quick Guide)
Condensed summary. Contains:
- 12-step overview with brief descriptions
- Key file locations with line numbers
- Data structure definitions
- Simplified call chain
- Real example flow
- Key points summary

**Best for:** Quick lookup and understanding the big picture

### 3. **MAPL3_Child_Component_Flow_Diagram.txt** (Visual Diagram)
ASCII art flow diagram. Contains:
- YAML structure visualization
- Step-by-step flow with ASCII trees
- Data structure evolution
- Timing and execution order
- Error handling points
- File locations
- MAPL3 vs MAPL2 comparison

**Best for:** Visual learners and understanding execution order

## Quick Navigation

### I want to understand...

**...the complete process from start to finish**
→ Read: `MAPL3_Child_Component_Creation_Trace.md`

**...how to add a child component programmatically**
→ See: Step 9 in `MAPL3_Child_Component_Creation_Trace.md`
→ GridCompAddChild interfaces at line 155 in `MAPL_Generic.F90`

**...how YAML is parsed**
→ See: Steps 5-7 in `MAPL3_Child_Component_Creation_Trace.md`
→ Functions: `parse_component_spec()`, `parse_children()`, `parse_child()`

**...the data structures involved**
→ See: "Key Data Structures" sections in all documents
→ Main types: `ChildSpec`, `ComponentSpec`, `OuterMetaComponent`, `DSOSetServices`

**...how DSO libraries are loaded**
→ See: Step 12 in `MAPL3_Child_Component_Creation_Trace.md`
→ Function: `run_DSOSetServices()` at line 151 in `UserSetServices.F90`

**...the timing/execution order**
→ See: "TIMING IN EXECUTION" section in `MAPL3_Child_Component_Flow_Diagram.txt`

**...how parent YAML is structured**
→ See: Step 1 in `MAPL3_Child_Component_Creation_Trace.md`
→ Real examples: `parent.yaml`, `history.yaml` in test scenarios

## Key Concepts

### Parent YAML Structure
Two formats supported:
1. Top-level `children:` section
2. Under `mapl:` section with `children:` subsection

### Child Specification
Each child requires:
- `dso`: Shared object library name (required)
- `config_file`: Path to child's YAML config (required)
- `SetServices`: Routine name (optional, default: `setservices_`)
- `timestep`: Optional time interval
- `run_time_offset`: Optional time offset

### Main Process Steps
1. Load parent YAML as ESMF_HConfig
2. Create parent GridComp
3. Call GenericSetServices
4. Parse YAML for children specifications
5. Create child GridComps
6. Call SetServices on children (recursively)
7. Each child's DSO is loaded and its SetServices called

### Key Files

| Component | File | Lines |
|-----------|------|-------|
| YAML Loading & Storage | `GenericGridComp.F90` | 87-142 |
| YAML Parsing | `ComponentSpecParser/*` | Various |
| Child Parsing | `parse_child.F90` | 8-70 |
| Child Addition | `add_child_by_spec.F90` | 19-55 |
| DSO SetServices | `UserSetServices.F90` | 151-165 |
| SetServices Orchestration | `OuterMetaComponent/SetServices.F90` | 29-100 |

## Real Example in Repository

See actual implementation in test scenarios:
- **Parent**: `/superstructure/generic/tests/scenarios/parent.yaml`
- **Child A**: `/superstructure/generic/tests/scenarios/precision_extension/A.yaml`
- **Child B**: `/superstructure/generic/tests/scenarios/precision_extension/B.yaml`

Also available:
- **History**: `/superstructure/generic/tests/scenarios/statistics/history.yaml`
- **Collections**: `/superstructure/generic/tests/scenarios/statistics/collection_1.yaml`

## Data Flow Summary

```
YAML File
  ↓
ESMF_HConfig (loaded)
  ↓
ComponentSpec (parsed)
  ├─ ChildSpecMap
  │   └─ ChildSpec (for each child)
  │       ├─ DSOSetServices (dso + routine name)
  │       └─ child_hconfig (child's YAML)
  ↓
add_child_by_spec()
  ├─ merge_hconfig() (combine parent + child YAML)
  ├─ GridCompCreate() (create child GridComp)
  └─ children.insert() (store in parent)
  ↓
run_children_setservices()
  └─ ESMF_GridCompSetServices() [recursive]
      └─ run_DSOSetServices()
          └─ Load DSO & call user's SetServices
```

## Branch Information

- **Branch**: develop
- **Repository**: MAPL (NASA GEOS framework)
- **Last Verified**: August 7, 2026
- **Fortran Standard**: Fortran 2003+ with submodules

## Files Generated

1. `MAPL3_Child_Component_Creation_Trace.md` - 1800+ lines, complete trace
2. `MAPL3_Child_Component_Quick_Reference.md` - 300+ lines, quick guide
3. `MAPL3_Child_Component_Flow_Diagram.txt` - 400+ lines, ASCII diagrams
4. `README_TRACE.md` - This file

## How to Use

1. **First time understanding**: Start with `Quick_Reference.md`
2. **Need details**: Go to `Creation_Trace.md`
3. **Visual learner**: Check `Flow_Diagram.txt`
4. **Quick lookup**: Use this `README_TRACE.md` for navigation

## Key Takeaways

1. YAML files are loaded by ESMF (not Python)
2. Parsing happens depth-first through ComponentSpecParser
3. Child GridComps are created with merged HConfigs
4. SetServices is called recursively (parent → children)
5. DSO libraries are loaded lazily by ESMF
6. Two-tier component structure (outer wrapper + inner user component)

## Related Code

- ESMF Framework: HConfig module for YAML handling
- MAPL Components: `superstructure/generic/` directory
- Grid Components: `OuterMetaComponent` type system
- Parser: `ComponentSpecParser` module system

## Notes

- This trace assumes default MAPL behavior
- Some advanced features (timestepping, grid propagation) are documented
- Error handling is included for robustness
- All paths are absolute paths in `/Users/wdboggs/src/MAPL/`

