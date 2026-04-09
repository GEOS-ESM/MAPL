# MAPL Code Coverage with gcov/lcov

This document describes how to build MAPL with coverage instrumentation, run the tests,
and generate an HTML coverage report locally on macOS (or Linux).

## Prerequisites

- **gfortran** from Homebrew (tested with gfortran-15)
- **gcov** matching your gfortran version (e.g. `gcov-15`; `/usr/bin/gcov` on macOS is
  Apple LLVM and **cannot** read gfortran `.gcda` files)
- **lcov** ≥ 2.0 (`brew install lcov`)
- **genhtml** (bundled with lcov)

Verify:

```bash
gcov-15 --version
lcov --version
genhtml --version
```

## Step 1 — Configure

Use the `Coverage` CMake preset (defined in `CMakePresets.json`):

```bash
cmake --preset Coverage
```

This sets `CMAKE_BUILD_TYPE=Coverage`, which activates `-O0 -g --coverage` for all
Fortran sources and links every target with `--coverage`.  The build tree is placed in
`build-Coverage/` and the install tree in `install-Coverage/`.

If you need to pass extra CMake arguments (e.g. pointing at Baselibs):

```bash
cmake --preset Coverage -DBASEDIR=/path/to/Baselibs/Darwin -DCMAKE_Fortran_COMPILER=gfortran-15
```

## Step 2 — Build

Build only the targets needed for testing (avoids building optional executables):

```bash
cmake --build build-Coverage --target build-tests --parallel 6
```

## Step 3 — Run the tests

Run the `ESSENTIAL`-labeled tests.  Use `--parallel 1` to keep `.gcda` file writes
serialised (parallel writes to the same `.gcda` file can corrupt coverage data):

```bash
ctest --test-dir build-Coverage -L ESSENTIAL --parallel 1 --output-on-failure
```

After this step, every compiled source file will have a `.gcda` file next to its `.gcno`
file somewhere under `build-Coverage/`.

## Step 4 — Collect coverage data

```bash
lcov --capture \
     --directory build-Coverage \
     --gcov-tool gcov-15 \
     --ignore-errors inconsistent,inconsistent \
     --rc geninfo_unexecuted_blocks=1 \
     --output-file coverage.info
```

Key flags:
- `--gcov-tool gcov-15` — use the gcov that matches your gfortran version
- `--ignore-errors inconsistent,inconsistent` — suppresses lcov 2.x false positives
  caused by compiler-generated procedures and pFUnit line-number mappings
- `--rc geninfo_unexecuted_blocks=1` — include un-hit basic blocks in the report

## Step 5 — Filter out noise

Remove system headers, third-party libraries, and the install tree from the report:

```bash
lcov --remove coverage.info \
     '/usr/*' \
     '*/pfunit/*' \
     '*/ESMF/*' \
     '*/install-Coverage/*' \
     '/Users/mathomp4/installed/*' \
     --ignore-errors inconsistent,inconsistent,unused \
     --output-file coverage-filtered.info
```

Adjust the Baselibs path (`/Users/mathomp4/installed/*`) to match your installation.
On Linux the path would be something like `/discover/nobackup/...` or wherever your
Baselibs are installed.

The `unused` error suppression is needed because some patterns (e.g. `*/ESMF/*`) do not
match any file on macOS where the library lives elsewhere.

## Step 6 — Generate the HTML report

```bash
genhtml coverage-filtered.info \
        --output-directory coverage-html \
        --title "MAPL Coverage" \
        --show-details \
        --legend \
        --ignore-errors range
```

The `--ignore-errors range` flag suppresses errors from `.pf` source files whose line
counts in the coverage data exceed their actual on-disk length (a pFUnit preprocessing
artefact).

Open the report:

```bash
# macOS
open coverage-html/index.html

# Linux
xdg-open coverage-html/index.html
```

## Typical results

With the `ESSENTIAL` test suite on macOS arm64 with gfortran-15:

| Metric    | Covered | Total  | Percentage |
|-----------|--------:|-------:|-----------:|
| Lines     |  41 278 | 88 766 |      46.5% |
| Functions |   5 417 | 17 864 |      30.3% |

## CDash / CTestDashboard workflow

To run coverage as part of the full CDash dashboard submission:

```bash
ctest \
  -D model=Experimental \
  -D build_type=Coverage \
  -D jobs=6 \
  -S CTestDashboard.cmake \
  -V
```

The `generator` parameter defaults to `Ninja` in `CTestDashboard.cmake`.  If you
configured with `cmake --preset Coverage` (Unix Makefiles) rather than
`cmake --preset Coverage-Ninja`, pass `-D generator="Unix Makefiles"` explicitly.

> **Note:** `CTestDashboard.cmake` calls `ctest_coverage()` automatically after the test step and
> includes `Coverage` in the submitted `PARTS`.  The coverage data is uploaded to CDash
> at <https://my.cdash.org/index.php?project=MAPL>.
> `CTestCustom.cmake` contains `CTEST_CUSTOM_COVERAGE_EXCLUDE` patterns that strip
> system headers, Baselibs, pFUnit internals, ESMF, and install trees from the CDash
> report, so only MAPL source files are counted.

## Cleaning up

The generated files are already in `.gitignore`:

```
coverage.info
coverage-filtered.info
coverage-html/
```

To remove them manually:

```bash
rm -f coverage.info coverage-filtered.info
rm -rf coverage-html/
```

To do a completely clean coverage rebuild:

```bash
rm -rf build-Coverage install-Coverage
cmake --preset Coverage
```
