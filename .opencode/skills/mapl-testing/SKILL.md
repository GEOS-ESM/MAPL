---
name: mapl-testing
description: Run and debug MAPL tests with pFUnit framework
compatibility: opencode
---

## What I Do

Guide you through running and debugging MAPL tests, including:
- Standard full test suite with ctest
- Fast generic3g-only workflow for rapid iteration
- Test debugging flags and techniques
- Platform-specific test handling
- Common test failure troubleshooting

## When to Use Me

Use this skill when:
- Running tests after building MAPL
- Debugging test failures
- Developing new features (need fast test feedback)
- Working specifically on generic3g code
- Encountering mysterious test crashes
- CI/CD test setup

## Two Testing Workflows

MAPL provides two testing approaches depending on your needs:

### 1. Standard Workflow (Full Test Suite)

**Use for:**
- Final verification before committing
- Testing changes across entire codebase
- CI/CD pipelines
- Release validation

**Pros:** Comprehensive
**Cons:** Slow (rebuilds everything)

### 2. Fast Workflow (generic3g Only)

**Use for:**
- Rapid iteration during generic3g development
- Quick test-fix-test cycles
- Debugging specific generic3g issues

**Pros:** Very fast (< 1 minute for changes to generic3g)
**Cons:** Only tests generic3g component

## Standard Testing Workflow (ctest)

### Run All Tests

```bash
cd <build-dir>  # e.g., cd nag or cd gfortran
ctest --output-on-failure
```

**IMPORTANT:** Running `ctest` at the top level rebuilds everything, which can be slow.

### Test Logging (Recommended)

**IMPORTANT:** Always log test output to track progress and diagnose issues:

```bash
cd <build-dir>
ctest --output-on-failure 2>&1 | tee ctest.log
```

**Why log:**
- `tail` can hide progress issues from user
- Full log allows reviewing test output later
- Logs persist in build directory for later review
- Standard location: `<build-dir>/ctest.log`

**AI Agents:** When running tests, ALWAYS use `tee` to create logs in the build directory.

### Run Tests in Parallel

```bash
ctest --output-on-failure -j 4  # Use 4 parallel jobs
```

### Run Specific Test

```bash
ctest -R <test-pattern> --output-on-failure

# Examples:
ctest -R generic3g --output-on-failure
ctest -R GridComp --output-on-failure
```

### Verbose Output

```bash
ctest -V  # Very verbose
ctest --output-on-failure  # Show output only on failure (recommended)
```

## Fast Workflow (generic3g Only)

**Problem:** Running `ctest` at top level is too slow during development.

**Solution:** Build and run generic3g tests directly from their directory.

### Important Setup Requirements

**CRITICAL:** 
- You MUST run from within `$BUILD/generic3g/tests` directory
- You MUST set `DYLD_LIBRARY_PATH` to include gridcomps directory
- gridcomps directory also contains NAG compiler license information

### Step-by-Step Fast Workflow

```bash
# 1. Navigate to generic3g tests directory
cd $BUILD/generic3g/tests

# Example paths:
cd ~/swdev/VS/MAPL/nag/generic3g/tests
cd ~/swdev/VS/MAPL/gfortran/generic3g/tests

# 2. Build just generic3g tests
make

# 3. Set library path
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH

# 4. Run tests
mpirun -np 1 ./MAPL.generic3g.tests
```

### Fast Workflow Test Logging (Recommended)

**IMPORTANT:** Always log test output:

```bash
# Build with logging
cd $BUILD/generic3g/tests
make 2>&1 | tee build.log

# Run tests with logging
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./MAPL.generic3g.tests 2>&1 | tee test-run.log
```

**Why log:**
- `tail` can hide progress issues from user
- Full log allows reviewing test output later
- Logs persist in tests directory for later review
- Standard locations: `$BUILD/generic3g/tests/build.log` and `test-run.log`

**AI Agents:** When running generic3g tests, ALWAYS use `tee` to create logs.

### Why This Works

- **make** rebuilds only generic3g tests, not entire MAPL
- **gridcomps/** contains compiled test components (shared libraries)
- **DYLD_LIBRARY_PATH** tells system where to find those libraries
- **gridcomps/** also has NAG license info (critical for NAG builds)

### Common Mistake

```bash
# WRONG - running from top-level build directory
cd ~/swdev/VS/MAPL/nag
export DYLD_LIBRARY_PATH=$PWD/generic3g/tests/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./generic3g/tests/MAPL.generic3g.tests  # Will fail!

# CORRECT - running from within tests directory
cd ~/swdev/VS/MAPL/nag/generic3g/tests
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./MAPL.generic3g.tests  # Works!
```

## Test Debugging Flags

When running `./MAPL.generic3g.tests`, use these flags AFTER the executable name:

### -d Flag: Diagnostic Output

```bash
mpirun -np 1 ./MAPL.generic3g.tests -d
```

**Use when:**
- Test crashes without error message
- Framework can't report which test failed
- Need to identify crash location

**Logging diagnostic output:**
```bash
mpirun -np 1 ./MAPL.generic3g.tests -d 2>&1 | tee test-diagnostic.log
```

**Output:**
```
Starting test: Test_GridComp_create
Ending test: Test_GridComp_create
Starting test: Test_GridComp_run
# <crash happens here - no "Ending" message>
```

This shows Test_GridComp_run started but never finished.

### -f Flag: Filter Tests

```bash
mpirun -np 1 ./MAPL.generic3g.tests -f ComponentDriver
```

**Use when:**
- Want to run only specific tests
- Debugging a particular failure
- Focusing on one component

**IMPORTANT:** Filter uses simple substring matching:
- ✓ **Correct:** `-f GridComp`
- ✓ **Correct:** `-f ComponentDriver`
- ✗ **Wrong:** `-f Grid*` (wildcards don't work)
- ✗ **Wrong:** `-f .Grid.` (regex doesn't work)
- ✗ **Wrong:** `-f Grid.*Comp` (regex doesn't work)

### Combining Flags

```bash
mpirun -np 1 ./MAPL.generic3g.tests -d -f GridComp
```

This runs only GridComp tests with diagnostic output. Perfect for focused debugging.

### Examples

```bash
# Run all tests with diagnostics
mpirun -np 1 ./MAPL.generic3g.tests -d

# Run only ComponentDriver tests
mpirun -np 1 ./MAPL.generic3g.tests -f ComponentDriver

# Debug GridComp tests specifically
mpirun -np 1 ./MAPL.generic3g.tests -d -f GridComp

# Run tests matching "create" in name
mpirun -np 1 ./MAPL.generic3g.tests -f create
```

## Platform-Specific Tests

Some tests only run on specific platforms and are automatically excluded on others.

### Test_MemInfoWrite (Linux Only)

**What:** Tests memory information reporting
**Requires:** `/proc/self/status` (Linux-specific file system)
**Status on macOS:** Automatically excluded via CMake configuration

**Location:** `utilities/tests/CMakeLists.txt`

**If you see this "fail" on macOS:** This is expected. The test is disabled via CMake on non-Linux platforms.

## Common Test Issues

### Library Load Errors

**Symptoms:**
```
dyld: Library not loaded: @rpath/libSomeComponent.dylib
```

**Problem:** DYLD_LIBRARY_PATH not set or incorrect

**Solution:**
```bash
# Must include gridcomps directory
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH

# Verify you're in correct directory
pwd  # Should end in: .../generic3g/tests
```

### NAG License Error

**Symptoms:**
```
NAG Fortran compiler: License error
```

**Problem:** gridcomps directory not in DYLD_LIBRARY_PATH

**Solution:**
The gridcomps directory contains NAG license information. Ensure DYLD_LIBRARY_PATH includes it:
```bash
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
```

This is the same fix as library load errors above.

### Test Crashes Without Error

**Symptoms:**
- Tests run, then sudden crash
- No error message
- Don't know which test failed

**Problem:** Framework couldn't report failure location

**Solution:** Use `-d` flag
```bash
mpirun -np 1 ./MAPL.generic3g.tests -d
```

Look for test that "started" but never "ended".

### Filter Not Working

**Symptoms:**
- Using `-f` but getting unexpected results
- No tests run
- All tests run despite filter

**Problem:** Trying to use wildcards or regex

**Solution:** Use simple substring matching only
```bash
# WRONG
mpirun -np 1 ./MAPL.generic3g.tests -f "Grid*"

# CORRECT
mpirun -np 1 ./MAPL.generic3g.tests -f Grid
```

The filter matches any test name containing that substring.

### Tests Pass Locally But Fail in CI

**Possible causes:**
1. **Platform differences** - Test may be Linux-specific
2. **Environment** - Missing modules or environment variables
3. **MPI configuration** - Different MPI versions or settings
4. **Timing** - Race conditions that only appear under load

**Debugging:**
1. Check if test is platform-specific
2. Compare loaded modules (local vs CI)
3. Try running with same MPI configuration as CI
4. Check for use of module variables or saved state (thread-safety issue)

### Full ctest Too Slow During Development

**Problem:** Need quick test feedback during generic3g development

**Solution:** Use fast workflow (see above)
```bash
cd $BUILD/generic3g/tests
make
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./MAPL.generic3g.tests
```

This typically takes < 1 minute vs many minutes for full ctest.

## Debugging Workflow

When a test fails:

1. **Identify failing test**
   ```bash
   # If crash with no error, use -d
   mpirun -np 1 ./MAPL.generic3g.tests -d
   ```

2. **Isolate the test**
   ```bash
   # Run only that test
   mpirun -np 1 ./MAPL.generic3g.tests -f FailingTestName
   ```

3. **Add diagnostics**
   ```bash
   # Combine filter and diagnostics
   mpirun -np 1 ./MAPL.generic3g.tests -d -f FailingTestName
   ```

4. **Check environment**
   ```bash
   # Verify modules loaded
   module list
   
   # Verify library path
   echo $DYLD_LIBRARY_PATH
   
   # Verify in correct directory
   pwd  # Should be in generic3g/tests
   ```

5. **Try different compiler** (if applicable)
   ```bash
   # Maybe NAG-specific issue, try gfortran
   cd ~/swdev/VS/MAPL/gfortran/generic3g/tests
   make
   export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
   mpirun -np 1 ./MAPL.generic3g.tests -f FailingTestName
   ```

## Test Development

When writing new tests:

1. **Add test to appropriate CMakeLists.txt**
2. **Use pFUnit framework** (see existing tests for examples)
3. **Test on multiple compilers** (NAG and gfortran minimum)
4. **Consider platform differences** (macOS vs Linux)
5. **Use fast workflow for iteration**
6. **Final verification with full ctest before committing**

## Quick Reference

### Standard Full Test Suite
```bash
cd <build-dir>
ctest --output-on-failure 2>&1 | tee ctest.log
```

### Fast generic3g Only
```bash
cd $BUILD/generic3g/tests
make 2>&1 | tee build.log
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./MAPL.generic3g.tests 2>&1 | tee test-run.log
```

### Debug Specific Test
```bash
cd $BUILD/generic3g/tests
export DYLD_LIBRARY_PATH=$PWD/gridcomps:$DYLD_LIBRARY_PATH
mpirun -np 1 ./MAPL.generic3g.tests -d -f TestName 2>&1 | tee test-debug.log
```

## Related Skills

- **`pfunit-troubleshooting`** - Deep dive into pFUnit-specific debugging
- **`mapl-build`** - Building MAPL before testing
- **`compiler-switching`** - Testing with different compilers

## Summary Checklist

- [ ] Know which workflow to use (full ctest vs fast generic3g)
- [ ] For fast workflow: in correct directory (generic3g/tests)
- [ ] For fast workflow: DYLD_LIBRARY_PATH set correctly
- [ ] Understand test debugging flags (-d, -f)
- [ ] Know how to isolate failing tests
- [ ] Aware of platform-specific tests
- [ ] Using diagnostic output when tests crash mysteriously
