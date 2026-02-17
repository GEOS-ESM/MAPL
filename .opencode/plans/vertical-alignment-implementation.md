# MAPL3 Vertical Alignment Implementation Plan

**Date:** February 13, 2026  
**Priority:** HIGH - Required for MAPL3 release  
**Timeline:** ~2 months to MAPL3 release for GEOS integration

---

## Executive Summary

Implement support for fields with reversed vertical coordinates (e.g., upward vs. downward alignment). This is achieved by:

1. Adding `coordinate_direction` to VerticalGrid base class
2. Adding `vertical_alignment` to VerticalGridAspect
3. Enhancing VerticalRegridTransform to handle alignment mismatches via copy/flip
4. Supporting both degenerate case (same grid) and full regridding with flips

**Default behavior:** Fields aligned "with_grid", grids default to "downward" (GEOS convention)

---

## Priority Context

### HIGH: Reversed Coordinate Fields ✅ IMPLEMENTING
- **Must have** for MAPL3 release
- This entire plan addresses this priority

### MEDIUM: Conservative Regridding with Delta-Pressure ⚠️ DEFERRED
- MAPL2 kludge already in place (ignore issues, treat as conserved)
- Can enhance later if needed

### LOW: Mixing Ratios ❌ MAPL 3.1
- Deferred to version 3.1
- Requires full PRE/POST split aspect architecture

### VERY LOW: Geopotential Height ❌ IGNORE
- Explicitly not implementing

---

## Implementation Approach

### Key Design Decisions

1. **Flipping Logic:** Performed in VerticalRegridTransform
   - Calculations in "grid coordinates"
   - Flip src to grid coords (if needed)
   - Apply linear interpolation
   - Flip result to dst coords (if needed)
   - Can involve 0, 1, or 2 flips

2. **Degenerate Case First:** Same grid with different alignments
   - Simple copy or copy-flip
   - Immediate value
   - Easy to implement and test

3. **Defaults:**
   - VerticalGrid coordinate_direction: "downward" (GEOS convention)
   - Field vertical_alignment: "with_grid" (follows the grid)

4. **No New Transform:** Fold flip logic into existing VerticalRegridTransform

---

## Task Breakdown

### TASK 1: Add coordinate_direction to VerticalGrid

**Files:**
- `vertical_grid/VerticalGrid.F90`
- `vertical_grid/BasicVerticalGrid.F90`
- `generic3g/vertical/FixedLevelsVerticalGrid.F90`
- `generic3g/vertical/ModelVerticalGrid.F90`

**Changes:**
```fortran
type, abstract :: VerticalGrid
   character(len=:), allocatable :: coordinate_direction  ! "upward" | "downward" | "unspecified"
contains
   procedure :: get_coordinate_direction
   procedure :: set_coordinate_direction
end type
```

**Default:** "downward" (GEOS convention)

**Testing:**
- Unit test: `Test_VerticalGrid.pf`
- Verify get/set for each concrete type

**Estimated Effort:** 1 day

---

### TASK 2: Add vertical_alignment to VariableSpec and VerticalGridAspect

**Files:**
- `generic3g/specs/VariableSpec.F90`
- `generic3g/specs/VerticalGridAspect.F90`

**Changes:**

**VariableSpec:**
```fortran
character(len=:), allocatable :: vertical_alignment  ! "upward" | "downward" | "with_grid"
```
- Default: "with_grid"
- Update constructor
- Pass to VerticalGridAspect

**VerticalGridAspect:**
```fortran
character(len=:), allocatable :: vertical_alignment
```

**Alignment Resolution:**
```fortran
function get_resolved_alignment(this) result(alignment)
   if (this%vertical_alignment == "with_grid") then
      alignment = this%vertical_grid%get_coordinate_direction()
   else
      alignment = this%vertical_alignment
   end if
end function
```

**Testing:**
- Unit test: `Test_VerticalGridAspect.pf`
- Verify alignment resolution

**Estimated Effort:** 0.5 days

---

### TASK 3: Degenerate Case - Same Grid Copy/Flip

**File:** `generic3g/transforms/VerticalRegridTransform.F90`

**Purpose:** When src and dst grids identical, just copy (with flip if needed)

**Changes:**

1. Add components:
```fortran
character(len=:), allocatable :: src_alignment
character(len=:), allocatable :: dst_alignment
logical :: is_degenerate_case
```

2. In `initialize()`: Detect if grids identical

3. In `update()`:
```fortran
if (is_degenerate_case) then
   if (src_alignment == dst_alignment) then
      call copy_field(f_in, f_out, _RC)
   else
      call copy_field_flipped(f_in, f_out, _RC)  ! x_out(:,k,:) = x_in(:,nlev-k+1,:)
   end if
else
   ! Existing/enhanced regridding logic
end if
```

**Testing:**
- Unit test: `Test_VerticalRegridTransform_Degenerate.pf`
  - Same grid, same alignment → copy
  - Same grid, diff alignment → flip
  - Verify data correctness

**Estimated Effort:** 1 day

---

### TASK 4: Enhance VerticalGridAspect Matching

**File:** `generic3g/specs/VerticalGridAspect.F90`

**Changes:**

Update `matches()`:
```fortran
function matches(src, dst) result(match)
   if (.not. grids_match) then
      match = .false.
      return
   end if
   
   ! Same grid with different alignments still "matches"
   ! (handled by degenerate case in VerticalRegridTransform)
   match = .true.
end function
```

**Testing:**
- Unit test: Update `Test_VerticalGridAspect.pf`
  - Same grid, any alignment → matches

**Estimated Effort:** 0.5 days

---

### TASK 5: Full Vertical Regridding with Alignment

**Files:**
- `generic3g/transforms/VerticalRegridTransform.F90`
- `generic3g/vertical/VerticalLinearMap.F90`

**Changes:**

**VerticalRegridTransform update():**
```fortran
if (is_degenerate_case) then
   // Task 3 logic
else
   // Step 1: Get coords and data
   
   // Step 2: Flip src if needed (to grid coordinates)
   if (src_alignment != grid_coordinate_direction) then
      src_coords_flipped = reverse(src)
      x_in_flipped = reverse_vertical(x_in)
   end if
   
   // Step 3: Compute interpolation matrix (in grid coordinates)
   call compute_linear_map(src_coords_flipped, dst, matrix, _RC)
   
   // Step 4: Apply matrix
   x_temp = matmul(matrix, x_in_flipped)
   
   // Step 5: Flip dst if needed (from grid coords to dst alignment)
   if (dst_alignment != grid_coordinate_direction) then
      x_out = reverse_vertical(x_temp)
   else
      x_out = x_temp
   end if
end if
```

**VerticalLinearMap:**
- Add assertion: src and dst must have same monotonicity
- Optionally: support both increasing and decreasing natively

**Testing:**

**Priority: "Almost Degenerate" Case (Primary Use Case)**
- Two grids that are identical except coordinate direction is reversed
- Fields aligned with their respective grids (one UP, one DOWN)
- **Test 1:** Verify `matches()` returns false (different grids)
- **Test 2:** Verify transform correctly flips values (dst reversed from src)
- **Why prioritize:** Easiest to test, most relevant to primary use case
- **Key insight:** Since grids are reverses, regridding should be identity + flip

**Additional Test Cases:**
- Unit test: `Test_VerticalRegridTransform_Alignment.pf`
  - Different grids, various alignment combinations
  - Verify correct flip behavior
- Unit test: `Test_VerticalLinearMap_Bidirectional.pf` (if supporting both orderings)

**Estimated Effort:** 2-3 days

---

### TASK 6: ExtData Configuration Support ✅ COMPLETED

**Status:** Implemented in commit [hash pending]

**Files:**
- `gridcomps/ExtData3G/ExtDataRule.F90` - Added `vertical_alignment` field and YAML parsing

**Changes:**

1. **Added vertical_alignment field to ExtDataRule type** (line 24)
   - Allocatable string field to store alignment specification from YAML

2. **Implemented YAML parsing** (lines 123-128)
   - Parses optional `vertical_alignment` field from configuration
   - Uses same pattern as existing fields like `enable_vertical_regrid`

3. **YAML format supported:**

### TASK 6: ExtData Configuration Support

**Files:**
- `gridcomps/ExtData3G/ExtDataConfig.F90`
- `gridcomps/ExtData3G/ExtDataRule.F90`

**Changes:**

1. Extend YAML parser:
```yaml
Exports:
  FIELD_NAME:
    collection: my_collection
    variable: my_var
    vertical_alignment: upward  # "upward" | "downward" | "with_grid" (default)
```


**Testing:**
- All ExtData3G unit tests pass
- Build successful with NAG compiler

**Note:** The vertical_alignment field is now available in ExtDataRule for future use when connecting to field configuration. Direct propagation to VariableSpec via add_var_specs() was not implemented as it would require architectural changes beyond YAML parsing
2. Store in ExtDataRule
3. Pass to VariableSpec

**Testing:**
- Parse YAML with vertical_alignment
- Verify propagation

**Estimated Effort:** 1 day

---

### TASK 7: Integration Test Scenarios

**Directory:** `generic3g/tests/scenarios/`

**Scenarios:**

1. **vertical_alignment_same_grid/**
   - Same grid, different field alignments
   - Verify degenerate case works

2. **vertical_alignment_regrid/**
   - Different grids, different alignments
   - Verify full regrid with flips

3. **vertical_alignment_with_grid/**
   - Test "with_grid" default
   - Verify uses grid's coordinate_direction

**Estimated Effort:** 1-2 days

---

### TASK 8: Documentation

**Files:**

1. User Guide: ExtData YAML configuration
2. Code comments: Flip logic, alignment resolution
3. Release notes: New feature

**Estimated Effort:** 0.5 days

---

### TASK 9 (FUTURE): FlippedVerticalGrid Decorator (Optional Enhancement)

**Purpose:** Simplify handling of reversed grids without duplicating grid data

**Motivation:**
- Current approach: Compare coordinate field values to detect same grid
- Better approach: Grid IDs can identify exact same grid vs. reversed grid
- Avoids tolerance-based comparisons since grids are constructed explicitly

**Design:**

**File:** `vertical_grid/FlippedVerticalGrid.F90`

```fortran
type, extends(VerticalGrid) :: FlippedVerticalGrid
   class(VerticalGrid), allocatable :: base_grid
contains
   procedure :: get_coordinate_direction  ! Returns opposite of base_grid
   procedure :: get_coordinate_field      ! Returns reversed coordinates
   procedure :: get_base_grid            ! Access to underlying grid
   procedure :: matches                   ! Special handling for flipped grids
end type
```

**Key behaviors:**
- `FlippedVerticalGrid(grid_A)%matches(grid_A)` → true (is reverse of base)
- `FlippedVerticalGrid(grid_A)%matches(FlippedVerticalGrid(grid_A))` → true (same grid)
- ID could be `base_grid_id + "_flipped"` or similar

**Benefits:**
1. Eliminate tolerance-based coordinate comparisons
2. Explicit representation of grid relationships
3. Cleaner degenerate case detection via ID comparison
4. Memory efficient (shares base grid data)

**Changes needed:**
- Add `FlippedVerticalGrid` class
- Update `VerticalGrid%matches()` to recognize flipped grids
- Update `VerticalRegridTransform%initialize()` to use ID comparison
- Consider factory function `make_flipped_grid(base)`

**Testing:**
- Verify flipped grid behavior
- Ensure ID-based matching works
- Check coordinate field reversal

**Status:** DEFERRED - Nice to have but not critical for initial release
**Estimated Effort:** 1-2 days

---

## Total Effort Estimate: 8-10 days

---

## Suggested Schedule

### Week 1
- **Day 1:** Task 1 - coordinate_direction
- **Day 2:** Task 2 - vertical_alignment + Task 3 start
- **Day 3:** Task 3 - Degenerate case complete
- **Day 4:** Task 4 - VerticalGridAspect matching
- **Day 5:** Task 5 start - Full regridding

### Week 2
- **Day 1-2:** Task 5 complete + testing
- **Day 3:** Task 6 - ExtData config
- **Day 4:** Task 7 - Integration scenarios
- **Day 5:** Task 8 - Documentation + buffer

---

## Multi-Session Strategy (OPTION 1: TASK-BASED - RECOMMENDED)

Break into discrete, self-contained sessions:

### Session 1: Foundation
- Task 1: coordinate_direction in VerticalGrid
- Task 2: vertical_alignment in VariableSpec/VerticalGridAspect
- **Deliverable:** Compiles, tests pass, can commit

### Session 2: Degenerate Case
- Task 3: Same-grid copy/flip implementation
- Task 4: Update VerticalGridAspect matching
- **Deliverable:** Degenerate cases work, tests pass, can commit

### Session 3: Full Regridding
- Task 5: Full vertical regridding with alignment
- **Deliverable:** Complete functionality, tests pass, can commit

### Session 4: Integration
- Task 6: ExtData configuration
- Task 7: Integration scenarios
- Task 8: Documentation
- **Deliverable:** Feature complete

---

## How to Resume Sessions

**At end of each session:**
- Commit work with clear message referencing task number
- Note what's complete and what's next
- Note any deviations from plan

**At start of new session:**
- Tell me: "Working on vertical alignment, completed Tasks 1-2, starting Task 3"
- I can read files to see current state
- Continue from there

**Context Management:**
- Keep sessions focused (2-3 hour chunks)
- Commit frequently (checkpoint/restore)
- Update plan if discoveries require changes
- Use git history for reference

---

## Daily Re-evaluation Questions

1. **Yesterday's progress:** What got done? Blockers?
2. **Today's goal:** Which task(s)?
3. **Scope adjustments:** Simplifications or additions?
4. **Parallel work:** Can others help?
5. **Testing:** Tests passing for completed work?

---

## Testing Requirements (Minimum)

### Unit Tests
- New/modified aspects: get/set in ESMF Info, match logic
- New transforms: at least sanity check
- VerticalLinearMap: bidirectional support

### Scenarios
- Exercise each new/modified aspect
- End-to-end validation

---

## Open Questions

1. **Grid coordinate direction detection:** Auto-detect from values or require explicit specification?
2. **VerticalLinearMap strategy:** Support both orderings natively, normalize internally, or keep simple?
3. **Testing approach:** TDD (tests first) or implement-then-test?

---

## Key Files Reference

### Core Implementation
- `vertical_grid/VerticalGrid.F90` - Base class with coordinate_direction
- `generic3g/specs/VerticalGridAspect.F90` - Aspect with vertical_alignment
- `generic3g/specs/VariableSpec.F90` - User-facing specification
- `generic3g/transforms/VerticalRegridTransform.F90` - Main flip/regrid logic
- `generic3g/vertical/VerticalLinearMap.F90` - Interpolation matrix computation

### Configuration
- `gridcomps/ExtData3G/ExtDataConfig.F90` - YAML parser
- `gridcomps/ExtData3G/ExtDataRule.F90` - Rule storage

### Testing
- `generic3g/tests/Test_VerticalGrid.pf`
- `generic3g/tests/Test_VerticalGridAspect.pf`
- `generic3g/tests/Test_VerticalRegridTransform.pf`
- `generic3g/tests/Test_VerticalLinearMap.pf`
- `generic3g/tests/scenarios/vertical_alignment_*/`

---

## Success Criteria

- ✅ Fields can specify vertical_alignment (upward/downward/with_grid)
- ✅ Grids have coordinate_direction (upward/downward)
- ✅ Same grid with different alignments: copy with flip
- ✅ Different grids with different alignments: regrid with appropriate flips
- ✅ All tests pass
- ✅ ExtData YAML configuration supports vertical_alignment
- ✅ Documentation complete
- ✅ No breaking changes to existing functionality

---

## Notes

- Conservative regridding already uses MAPL2 kludge - no work needed
- Mixing ratios deferred to MAPL 3.1
- Geopotential height ignored
- Plan designed for incremental progress with stable checkpoints
- Each session should end with working, committable code
TODO: Add unit tests for VerticalGridAspect update_payload() and update_from_payload() to verify alignment is properly serialized/deserialized
