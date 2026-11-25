# include/ — preprocessor headers for components

**Audience:** coding agents and developers working on GEOS gridded components and on MAPL.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`; this directory is `include/`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

## Purpose and build

Headers only. `include/CMakeLists.txt` copies the six public headers
(`MAPL_ErrLog.h`, `MAPL_ErrLogMain.h`, `MAPL_Exceptions.h`, `MAPL_Generic.h`,
`NUOPC_ErrLog.h`, `unused_dummy.H`) to `${esma_include}/MAPL/`
(`configure_file ... COPYONLY`). Library targets add
`${MAPL_SOURCE_DIR}/include` (for example `generic/CMakeLists.txt`).
Existing document: `include/README.md` (usage guide for the macros).

## Which header defines what

| Header | Defines |
|---|---|
| `MAPL_Generic.h` | umbrella for components: includes `MAPL_Exceptions.h`, `MAPL_ErrLog.h`, `unused_dummy.H`; `GET_POINTER`, `MAPL_GetPointer` -> `ESMFL_StateGetPointerToData`, `MAPL_FieldGetPointer` -> `ESMFL_FieldGetPointerToData`; `MAPL_real` (by `GR4`/`GR8`); `MAPL_SSX` |
| `MAPL_ErrLog.h` | old-style `RETURN_`, `VERIFY_`, `ASSERT_`; new-style `_RETURN`, `_RETURN_IF`, `_RETURN_UNLESS`, `_VERIFY`, `_ASSERT`, `_ASSERT_RC`, `_FAIL`, `_RC`, `_USERRC`, `_STAT`, `_IERROR`, `_IOSTAT`, `_HERE`, `_SUCCESS` (0), `_FAILURE` (1), `_FILE_` |
| `MAPL_Exceptions.h` | `__Iam__(name)` (declares `STATUS` and `Iam`), `_Iam_`, `__RC__`, `__STAT__`, `__try__/__catch__/__except__/__endtry__`, `__Try__/__endTry__` (named), `__raise__`; return codes `MAPL_RC_ERROR` 2000 ... `MAPL_RC_FORTRAN_ARRAY_NOT_FOUND` 2004; includes `MAPL_ErrLog.h` |
| `MAPL_ErrLogMain.h` | redefines `VERIFY_`/`ASSERT_` to `call MAPL_abort` for main programs |
| `NUOPC_ErrLog.h` | `VERIFY_ESMF_`, `VERIFY_ALL_ESMF_`, `VERIFY_NUOPC_` (ESMF log based) |
| `unused_dummy.H` | `_UNUSED_DUMMY(x)` (flang-specific form under `__flang__`) |

## Macro behaviour (`MAPL_ErrLog.h`)

- `_RC` expands to `rc=status); _VERIFY(status`: put it last in a call's
  argument list; requires local `integer :: status` and dummy `rc`.
  `_USERRC` does the same with `user_status` for `userRC`.
- `_VERIFY(A)` calls `MAPL_Verify(A, file, line, rc)` and returns on error;
  `_RETURN(A)` calls `MAPL_Return` and returns; `_ASSERT(cond, msg)` calls
  `MAPL_Assert` (status 1); `_FAIL(msg)` is `_ASSERT(.false., msg)`. These
  procedures live in `../shared` (`MAPL_ExceptionHandling.F90`,
  `MAPL_ErrorHandling.F90`); see `../shared/ARCHITECTURE.md`.
- Old-style `VERIFY_(A)`, `RETURN_(A)`, `ASSERT_(A)` need a local `Iam`
  and call `MAPL_VRFY/RTRN/ASRT`.
- If `I_AM_MAIN` is defined, `__return` becomes `call MAPL_abort()` and the
  `rc` argument is dropped; with `I_AM_FUNIT` / `I_AM_PFUNIT`, `_VERIFY`
  becomes a pFUnit assertion.
- Components start with `#include "MAPL_Generic.h"` (usually via
  `#include "MAPL_ErrLog.h"` in older code); `MAPL_Generic.F90` itself
  includes `MAPL_Exceptions.h` and `MAPL_ErrLog.h`.

## Known traps

- Each header `#undef`s its macros first, so include order decides which
  definition of `VERIFY_`/`ASSERT_` wins (`MAPL_ErrLogMain.h` guards with
  `MAPL_ErrLogMain_DONE`).
- `_RETURN`, `_ASSERT`, `_FAIL` use the name `rc`; `_RC` additionally
  needs `status`.

## Observations (noted, not fixed)

- `MAPL_Exceptions.h` documents the `__try__` family in a protex block
  that is compiled out (`__PROTEX__`).
- `NUOPC_ErrLog.h` `VERIFY_NUOPC_` checks `rc`, not its argument.
