# Implementation Plan: TimeMin and TimeMax Extensions

## Overview
Create two new Fortran modules (`TimeMin.F90` and `TimeMax.F90`) that extend `AbstractTimeStatistic` to compute minimum and maximum values over a time period, similar to how `TimeAverage.F90` computes averages.

## Key Findings from Code Analysis

**Pattern from TimeAverage:**
- Extends `AbstractTimeStatistic` with required methods: `initialize`, `destroy`, `reset`, `update`, `compute_result`, `add_to_state`
- Stores input field (`f`) and output field (`min_f`/`max_f`)
- Maintains internal state (`counts` array to track valid samples)
- Supports both R4 (32-bit) and R8 (64-bit) floating point types
- Respects MAPL_UNDEF values (skips undefined data)
- Resets accumulator when alarm rings

## Implementation Plan

### Phase 1: Create TimeMin.F90
1. **Module Structure** - Mirror TimeAverage but for minimum computation:
   - Store `min_f` field (instead of `sum_f`)
   - Initialize with `MAPL_UNDEF` or large value
   - Track counts per element

2. **Core Methods**:
   - `initialize`: Clone input field for `min_f` storage
   - `reset`: Fill `min_f` with `MAPL_UNDEF` or `huge(real)`
   - `update`: Compare each value with current minimum (ignoring MAPL_UNDEF)
   - `compute_result`: Already computed during updates
   - `add_to_state`: Add `min_f` to state when alarm rings

3. **Type-Specific Routines**: Create `update_r4`, `update_r8`, `compute_result_r4`, `compute_result_r8`

### Phase 2: Create TimeMax.F90
1. **Module Structure** - Identical to TimeMin but for maximum:
   - Store `max_f` field
   - Initialize with negative infinity (`-huge(real)`)
   
2. **Core Methods**: Same as TimeMin but with max comparison logic

### Phase 3: Integration
1. Update `CMakeLists.txt` to include both new modules in build
2. Verify build dependencies (AbstractTimeStatistic must be built first)

## Design Decisions

1. **Initialization Value for Min/Max**:
   - Initialize with MAPL_UNDEF, only set to real value after first valid sample
   - This is safer and matches undefined semantics

2. **Handling MAPL_UNDEF**:
   - Skip undefined values entirely (don't include in comparison)
   - Continue counting only valid samples
   - Mark result as MAPL_UNDEF if no valid samples found

3. **Code Structure**:
   - Mirror TimeAverage pattern closely for consistency
   - Support both R4 and R8 types
   - Include counts array to track valid data points

## File Changes Summary

| File | Action | Purpose |
|------|--------|---------|
| `TimeMin.F90` | Create new | Minimum time statistic implementation |
| `TimeMax.F90` | Create new | Maximum time statistic implementation |
| `CMakeLists.txt` | Modify | Add TimeMin.F90 and TimeMax.F90 to build sources |
