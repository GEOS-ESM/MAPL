# Quickstart: Propagate Error Macros

## Prerequisites

- Foundation issue #5324 available in current branch history.
- Configured NAG or gfortran MAPL build.
- Required compiler stack loaded in every build/test shell.

## Current Validation Environment

- NAG Fortran 7.2.41 from `nag-stack/default`.
- OpenMPI 5.0.5, baselibs 9.9.0, and GFTL 1.17 supplied by `nag-stack/default`.
- CMake 3.31.1 at `/opt/homebrew/bin/cmake`.
- Feature-local build directory: `nag/`.
- `cmake --build nag -j 8` passed.
- `ctest --test-dir nag -L ESSENTIAL --output-on-failure` passed 65/65.

## Batch Workflow

1. Update `error-code-inventory.md` for one subsystem batch.
2. Approve canonical groups and context types.
3. Add new code/template to `utils/Constants/mapl_error_codes.yaml` if needed.
4. Migrate sites to code-bearing/context macro forms.
5. Build affected targets.
6. Run:

```text
ctest --test-dir build-dir -L ESSENTIAL --output-on-failure
```

7. Record batch status before starting next subsystem.

## Expected Outcomes

- Existing codes and `rc` behavior remain unchanged.
- Migrated diagnostics use catalog prose and include supplied string/integer values.
- Legacy sites remain explicitly inventoried until approved and verified.
- Unsupported context types remain unimplemented until human approval.
