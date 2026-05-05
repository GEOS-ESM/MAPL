---
name: pfunit-troubleshooting
description: Debug and fix pFUnit test failures in MAPL
compatibility: opencode
---

# pFUnit Troubleshooting

This skill provides comprehensive guidance for debugging and fixing pFUnit test failures in MAPL. pFUnit is the parallel Fortran unit testing framework used throughout MAPL.

## Quick Reference

### Common pFUnit Commands

```bash
# Run single test with diagnostics
./TestName.x -d

# Run filtered tests (substring match)
./TestName.x -f substring

# Run with verbose output
./TestName.x -v

# List all tests
./TestName.x -l

# Run specific test by exact name
./TestName.x -f exact_test_name
```

### Fast Generic3g Test Workflow

```bash
# From MAPL build directory (e.g., ~/swdev/VS/MAPL/nag)
cd generic3g/MAPL_cfio/MAPL_cfio_r4.tests

# Set library path (required for NAG)
export DYLD_LIBRARY_PATH=/usr/local/nag/lib:$DYLD_LIBRARY_PATH

# Run tests (< 1 minute)
./generic3g.x
```

## Understanding pFUnit Test Structure

### Test Executable Naming

MAPL uses consistent naming for test executables:
- `generic3g.x` - Generic 3D grid tests
- `TestName.x` - Other component tests

### Test Organization

Tests are organized by component:
```
build-dir/
├── base/
│   └── tests/
│       └── TestSomething.x
├── generic3g/
│   └── MAPL_cfio/
│       └── MAPL_cfio_r4.tests/
│           └── generic3g.x
├── gridcomps/
│   └── tests/
└── ...
```

### pFUnit Test File Extensions

- `*.pf` - pFUnit test source files (preprocessed to `.F90`)
- `*.F90` - Generated or manual Fortran test files
- `*.x` - Compiled test executables

## Debugging Test Failures

### Step 1: Identify the Failure

When `ctest` reports a failure:

```bash
# Run ctest to see which tests fail
ctest

# Example output:
# 99% tests passed, 1 tests failed out of 100
# The following tests FAILED:
#     42 - generic3g (Failed)
```

### Step 2: Run Test Directly with Diagnostics

Navigate to test directory and run with diagnostics:

```bash
# Find the test executable
find . -name "generic3g.x" -type f

# Navigate to test directory
cd generic3g/MAPL_cfio/MAPL_cfio_r4.tests

# Run with diagnostics
./generic3g.x -d
```

**The `-d` flag shows:**
- Test names as they execute
- Assertion failures with line numbers
- Expected vs actual values
- Stack traces (if available)

### Step 3: Run Specific Failing Test

Use `-f` to filter to specific test:

```bash
# If test is named "test_vertical_interpolation"
./generic3g.x -f vertical_interpolation

# Multiple matches possible (substring matching)
./generic3g.x -f interpolation
```

**IMPORTANT:** The `-f` flag does substring matching, NOT wildcard/glob matching. Do not use `*` or `?`.

### Step 4: Examine Test Output

**Common failure patterns:**

#### Assertion Failures
```
Location: test_vertical_interpolation.pf:45
 Expected: 1.23456
 Actual:   1.23457
 Difference exceeds tolerance
```

**What to check:**
- Is the tolerance too strict?
- Is there a numerical precision issue?
- Did algorithm change affect expected values?

#### Segmentation Faults
```
Program received signal SIGSEGV: Segmentation fault
```

**What to check:**
- Array bounds violations
- Uninitialized pointers/allocatables
- Memory corruption

#### Hangs/Timeouts
Test runs but never completes.

**What to check:**
- Infinite loops
- Deadlocks in parallel code
- MPI issues

## Common pFUnit Issues and Solutions

### Issue: "No such test"

**Symptom:**
```bash
./generic3g.x -f my_test
# No tests run
```

**Solution:**
List all available tests:
```bash
./generic3g.x -l
```

Check exact test name and use correct substring.

### Issue: Library Loading Errors (macOS)

**Symptom:**
```
dyld: Library not loaded: @rpath/libnag_nag.dylib
```

**Solution (NAG on macOS):**
```bash
export DYLD_LIBRARY_PATH=/usr/local/nag/lib:$DYLD_LIBRARY_PATH
./generic3g.x
```

**Solution (gfortran on macOS):**
```bash
export DYLD_LIBRARY_PATH=/usr/local/gfortran/lib:$DYLD_LIBRARY_PATH
./generic3g.x
```

**IMPORTANT:** This is required for direct test execution. Not needed for `ctest` if properly configured.

### Issue: Test Passes Locally, Fails in CI

**Possible causes:**
1. **Compiler differences** - CI may use different compiler
2. **Timing issues** - CI may be slower/faster
3. **Environment differences** - Different library versions
4. **Uninitialized variables** - Different compilers handle differently

**Debugging approach:**
1. Check which compiler CI uses
2. Build locally with same compiler
3. Run tests with that compiler
4. Compare results

### Issue: Numerical Tolerance Failures

**Symptom:**
```
Expected: 1.2345678
Actual:   1.2345679
```

**Common causes:**
- Compiler optimization differences
- Floating-point rounding in different orders
- Platform differences (x86 vs ARM)

**Solutions:**

1. **Relax tolerance** (if difference is negligible):
   ```fortran
   @assertEqual(expected, actual, tolerance=1.0e-5)
   ```

2. **Use relative tolerance**:
   ```fortran
   @assertEqual(expected, actual, tolerance=abs(expected)*1.0e-6)
   ```

3. **Investigate algorithm** if difference is significant

### Issue: MPI-Related Failures

**Symptom:**
Test fails only when run with multiple processes.

**Debugging approach:**

1. **Run with single process:**
   ```bash
   mpirun -np 1 ./TestName.x
   ```

2. **Run with multiple processes:**
   ```bash
   mpirun -np 4 ./TestName.x
   ```

3. **Check for:**
   - Race conditions
   - Incorrect MPI communication patterns
   - Uninitialized data on some ranks

### Issue: Test Executable Not Found

**Symptom:**
```bash
./TestName.x
bash: ./TestName.x: No such file or directory
```

**Solution:**
Test may not have been built. Rebuild:
```bash
cd <build-directory>
make -j8 install
```

If still not found, test may be disabled or have build errors. Check CMake output.

### Issue: Tests Fail After Code Changes

**Debugging workflow:**

1. **Verify change didn't break assumptions:**
   - Did you change function signatures?
   - Did you modify behavior that tests depend on?

2. **Update tests if behavior intentionally changed:**
   ```fortran
   ! Update expected values
   @assertEqual(new_expected_value, actual)
   ```

3. **Fix code if tests revealed bugs:**
   - Tests are correct, code is wrong
   - This is the most common case!

4. **Check for stale build artifacts:**
   ```bash
   make clean
   make -j8 install
   ctest
   ```

## Writing Effective pFUnit Tests

### Basic Test Structure

```fortran
@test
subroutine test_my_function()
   use pfunit_mod
   use MyModule
   implicit none
   
   integer :: result
   integer :: expected
   
   ! Setup
   expected = 42
   
   ! Execute
   result = my_function(input)
   
   ! Verify
   @assertEqual(expected, result)
   
end subroutine test_my_function
```

### Common Assertions

```fortran
! Equality assertions
@assertEqual(expected, actual)
@assertEqual(expected, actual, tolerance=1.0e-6)
@assertEqual(expected, actual, message="Custom error message")

! Boolean assertions
@assertTrue(condition)
@assertFalse(condition)

! Exception/error assertions
@assertExceptionRaised("Expected error message")

! Array assertions
@assertEqual(expected_array, actual_array)
@assertEqual(expected_array, actual_array, tolerance=1.0e-6)
```

### Setup and Teardown

```fortran
@before
subroutine setUp()
   ! Initialize test fixtures
   ! Allocate arrays
   ! Open files
end subroutine setUp

@after
subroutine tearDown()
   ! Clean up test fixtures
   ! Deallocate arrays
   ! Close files
end subroutine tearDown

@test
subroutine test_something()
   ! setUp() called automatically before this
   ! Test code here
   ! tearDown() called automatically after this
end subroutine test_something
```

### Testing with Floating-Point Numbers

**CRITICAL:** Never use exact equality for floating-point:

```fortran
! BAD - fragile, compiler-dependent
@assertEqual(1.23456, result)

! GOOD - uses tolerance
@assertEqual(1.23456, result, tolerance=1.0e-5)

! BETTER - relative tolerance for large numbers
@assertEqual(expected, result, tolerance=abs(expected)*1.0e-6)
```

### Testing Arrays

```fortran
@test
subroutine test_array_operation()
   use pfunit_mod
   implicit none
   
   real, allocatable :: expected(:)
   real, allocatable :: result(:)
   integer :: i
   
   allocate(expected(10))
   allocate(result(10))
   
   ! Setup expected values
   do i = 1, 10
      expected(i) = real(i) * 2.0
   end do
   
   ! Call function under test
   call compute_array(result)
   
   ! Verify
   @assertEqual(expected, result, tolerance=1.0e-6)
   
   deallocate(expected)
   deallocate(result)
   
end subroutine test_array_operation
```

## Debugging Techniques

### Using Print Statements

**Quick debugging in pFUnit tests:**

```fortran
@test
subroutine test_with_debug_output()
   use pfunit_mod
   implicit none
   
   integer :: result
   
   result = my_function(42)
   
   print *, "DEBUG: result =", result
   print *, "DEBUG: expected =", 84
   
   @assertEqual(84, result)
   
end subroutine test_with_debug_output
```

Run test to see debug output:
```bash
./TestName.x -d -f test_with_debug_output
```

### Using GDB with pFUnit

```bash
# Compile with debug symbols (should already be in Debug build)
cmake .. -DCMAKE_BUILD_TYPE=Debug
make -j8 install

# Run test under GDB
gdb ./TestName.x

# In GDB:
(gdb) run -f failing_test
(gdb) bt  # backtrace when it crashes
(gdb) print variable_name
```

### Using LLDB (macOS)

```bash
lldb ./TestName.x

# In LLDB:
(lldb) run -f failing_test
(lldb) bt  # backtrace
(lldb) frame variable  # show local variables
```

### Compiler Debug Flags

Different compilers offer different debugging capabilities:

**NAG:**
```bash
# Already has excellent runtime checking in Debug mode
cmake .. -DCMAKE_BUILD_TYPE=Debug
```

**gfortran:**
```bash
# Add extra runtime checks
cmake .. -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_Fortran_FLAGS="-g -fbacktrace -fcheck=all"
```

**Intel:**
```bash
# Add runtime checks
cmake .. -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_Fortran_FLAGS="-g -traceback -check all"
```

## Performance Testing

### Measuring Test Execution Time

```bash
# Time entire test suite
time ./TestName.x

# Time specific test
time ./TestName.x -f specific_test
```

### Identifying Slow Tests

```bash
# Run with verbose output to see timing
ctest -V

# Or use ctest's built-in timing
ctest --output-on-failure --verbose
```

If a test is unexpectedly slow:
1. Check for I/O operations (file reads/writes)
2. Check for large array allocations
3. Check for nested loops with high iteration counts
4. Consider if test is actually a performance test (intentionally slow)

## Test Maintenance Best Practices

### When to Update Tests

1. **Function behavior changed intentionally** → Update test expectations
2. **Bug was found in test** → Fix the test
3. **Implementation optimized but behavior unchanged** → Tests should still pass
4. **Test is flaky** → Fix test to be deterministic

### When to Disable Tests

**CRITICAL:** Disabling tests should be rare and temporary.

Valid reasons:
- Test depends on external resource that's unavailable
- Test is known to be flaky (GitHub issue filed)
- Feature being tested is deprecated

**How to disable:**
```fortran
@test(ifdef=SOME_FEATURE_FLAG)
subroutine test_to_disable()
   ! This test only runs if SOME_FEATURE_FLAG is defined
end subroutine test_to_disable
```

Or remove from CMakeLists.txt temporarily (prefer ifdef approach).

### Test Naming Conventions

Good test names are descriptive:

```fortran
! GOOD
@test
subroutine test_vertical_interpolation_linear_profile()

@test  
subroutine test_grid_create_with_invalid_dimensions_should_fail()

! BAD (too vague)
@test
subroutine test1()

@test
subroutine test_grid()
```

## Integration with GitHub Workflow

### Before Creating PR

```bash
# Run full test suite locally
cd <build-directory>
ctest

# If any failures, debug and fix
cd path/to/failing/test
./TestName.x -d

# Fix code or test, rebuild, retest
make -j8 install
ctest
```

**CRITICAL:** All tests must pass before creating PR.

### Responding to CI Test Failures

If tests pass locally but fail in CI:

1. **Check CI logs** for exact failure
2. **Note compiler/platform** used in CI
3. **Reproduce locally** with same compiler
4. **Fix and push** updated code

See `github-workflow` skill for PR process.

## Related Skills

- **mapl-testing** - General MAPL testing workflows and strategies
- **mapl-build** - Building MAPL (required before testing)
- **fortran-style** - Fortran coding standards (affects test code too)
- **mapl-error-handling** - Error handling in code being tested
- **github-workflow** - CI/CD and PR process

## Advanced Topics

### Parallel Test Debugging

When debugging MPI-parallel tests:

```bash
# Run under MPI with debugger
mpirun -np 4 xterm -e gdb ./TestName.x
```

This opens separate debugger windows for each MPI rank.

### Memory Debugging with Valgrind

```bash
# Check for memory leaks
valgrind --leak-check=full ./TestName.x -f specific_test

# Check for uninitialized memory
valgrind --track-origins=yes ./TestName.x -f specific_test
```

**Note:** Valgrind may report false positives with MPI and OpenMP.

### Testing Private Module Procedures

pFUnit tests can't directly test private procedures. Options:

1. **Test through public interface** (preferred)
2. **Make procedure public for testing** (acceptable if documented)
3. **Use test-only wrapper** (complex, rarely needed)

### Parameterized Tests

For testing multiple similar cases:

```fortran
@test(cases=[1, 2, 3, 4, 5])
subroutine test_with_parameter(n)
   integer, intent(in) :: n
   
   ! Test using parameter n
   @assertEqual(n * 2, my_double_function(n))
   
end subroutine test_with_parameter
```

---

**When to use this skill:**
- Debugging test failures
- Understanding pFUnit output
- Writing new tests
- Fixing flaky tests
- Responding to CI failures
