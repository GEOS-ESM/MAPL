# MAPL Orbital Grid Component Test Suite

This directory contains unit and integration tests for the MAPL Orbital Grid Component (`MAPL_OrbGridCompMod.F90`).

## Overview

The test suite validates orbital masking functionality for satellite instrument coverage simulation in GEOS atmospheric models. Tests verify both geometric helper functions and full component integration with ESMF grids.

## Test Organization

### Test Files

1. **Test_OrbHelperFunctions.pf**
   - **Status**: Placeholder (tests disabled until refactoring)
   - Will test pure geometric and mathematical utility functions
   - Functions to test: `pnt_in_rect`, `ijsearch`, `orb_edges_1d`, `flatten_latlon`, `flatten_xy`, `cube_xy`, `check_face`
   - Currently disabled because helper functions are not public in MAPL_OrbGridCompMod
   - Will be enabled when functions are extracted to testable modules during refactoring

2. **Test_OrbMasking.pf**
   - **Status**: Placeholder (tests disabled until refactoring)
   - Will test masking algorithms for track and swath modes
   - Functions to test: `orb_mask_lonlat`, `orb_mask_xy`, `orb_swath_mask_lonlat`, `orb_halo`
   - Currently disabled because masking functions are not public
   - Will be enabled during refactoring phase
   - Includes basic test verifying mock orbit module functionality

3. **Test_OrbGridComp.pf**
   - **Status**: Active integration tests
   - Tests component lifecycle: SetServices, Initialize, Run
   - Grid types: Lat-lon and cubed-sphere
   - Parallel execution tests with domain decomposition
   - Uses `ESMF_TestMethod` for ESMF-aware testing
   - These tests work with the public API and are ready to run

### Support Files

- **mock_nominal_orbits.F90**: Mock implementation of `MAPL_NominalOrbitsMod`
  - Provides deterministic orbit patterns for reproducible testing
  - Patterns: EQUATORIAL, POLAR, DIAGONAL, DATELINE_CROSSING, PRIME_MERIDIAN, ARCTIC, ANTARCTIC
  - Implements both `orbits_track` (nadir-only) and `orbits_swath` (cross-track)

- **MAPL_OrbGridComp.rc**: Test configuration file
  - Defines test instruments: TEST_INST1 (equatorial, no swath), TEST_INST2 (polar, 100km swath)
  - Used by component integration tests

## Building and Running

### Prerequisites

- MAPL framework with ESMF
- pFUnit testing framework
- MPI for parallel tests

### Build

From MAPL build directory:
```bash
cmake --build . --target OrbGridComp.tests
```

### Run Tests

```bash
ctest -R OrbGridComp.tests -V
```

Run specific test:
```bash
ctest -R OrbGridComp.tests.Test_OrbHelperFunctions -V
```

Run parallel tests:
```bash
ctest -R OrbGridComp.tests.Test_OrbGridComp -V
```

## Test Strategy

### Current State

The test suite is structured in three layers:

1. **Helper Function Tests** (Test_OrbHelperFunctions.pf): Currently disabled with placeholders
   - Will be enabled once internal functions are made public during refactoring
   - Provides foundation for testing grid-agnostic geometric operations

2. **Masking Algorithm Tests** (Test_OrbMasking.pf): Currently disabled with placeholders
   - Will be enabled as masking functions are extracted and made testable
   - Includes working test of mock orbit module functionality

3. **Component Integration Tests** (Test_OrbGridComp.pf): **Active and ready**
   - Tests the public API (SetServices, Initialize, Run)
   - Validates component behavior on different grid types
   - Provides regression protection for refactoring

### Phase 1: Baseline Validation (Current)
- Component integration tests verify existing behavior through public API
- Mock orbit module provides deterministic test data
- Tests establish behavioral baseline before refactoring begins
- Known bugs will be documented in top-level `CHANGELOG.md` as discovered

### Phase 2: Evolution During Refactoring  
- As internal functions are made public/testable, corresponding tests will be enabled
- Helper function tests activated when functions extracted to separate modules
- Masking tests activated when algorithms made grid-agnostic
- New tests added for ESMF grid operations and halo exchanges
- Performance regression monitoring if needed

## Known Issues Being Tested

1. **Halo Boundary Clipping** (lines 1563-1578 in MAPL_OrbGridCompMod.F90)
   - Current code clips halo at subdomain boundaries
   - Should use ESMF halo exchange instead
   - Test: `test_orb_halo_boundary_clipping` documents current behavior

2. **Grid Type Detection** (lines 267-295)
   - Cubed-sphere detection via ESMF_AttributeGet
   - Should be more grid-agnostic
   - Test: `test_Initialize_CubedSphere` validates detection

3. **Code Duplication**
   - Separate `DoMasking_` and `DoMasking_CS` implementations
   - Should use common grid-agnostic algorithms
   - Tests provide safety net for consolidation

## Adding New Tests

Follow pFUnit conventions:

```fortran
@test
subroutine test_new_feature()
   ! Arrange
   ! Act
   ! Assert using @assertEqual, @assertGreaterThan, etc.
end subroutine test_new_feature
```

For ESMF tests, extend `ESMF_TestMethod`:

```fortran
@test(npes=[1,2], type=ESMF_TestMethod)
subroutine test_parallel_feature(this)
   class(OrbGridCompTest), intent(inout) :: this
   ! Test code with setUp/tearDown managing ESMF resources
end subroutine test_parallel_feature
```

## References

- MAPL Documentation: https://github.com/GEOS-ESM/MAPL
- pFUnit Documentation: https://github.com/Goddard-Fortran-Ecosystem/pFUnit
- ESMF Documentation: https://earthsystemmodeling.org/docs/
