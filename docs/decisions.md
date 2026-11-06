# MAPL (MAPL2 line) — previous architectural decisions

**Audience:** coding agents and developers working on MAPL.
**Describes:** the MAPL2 line, branch `release/v2`.
**Verified against:** `MAPL` @ `aea13cf6` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): key `MAPL`, checked out at `src/Shared/@MAPL`.
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

### Two ExtData generations side by side
- Pattern: `gridcomps/ExtData/` and `gridcomps/ExtData2G/` are both built by default; Cap selects one at run time from `USE_EXTDATA2G` (`gridcomps/CMakeLists.txt`, `gridcomps/Cap/MAPL_CapGridComp.F90`, `initialize_gc`). [Verified]
- Likely reason: let a run switch generations without rebuilding, during migration. [Inferred]
- Question for maintainers: when is the first generation planned to be removed?

### Macro layer for error handling
- Pattern: components use preprocessor macros (`_RC`, `_VERIFY`, `_ASSERT`, `__RC__`) rather than routines (`include/MAPL_ErrLog.h`, `include/MAPL_Exceptions.h`). [Verified]
- Likely reason: macros capture file and line of the failure and return from the caller's scope. [Inferred]

### Generic component as one large module
- Pattern: `MAPL_GenericMod` holds the IRF methods, specs, children, connections, resources and timers in one file of about 11 thousand lines (`generic/MAPL_Generic.F90`). [Verified]
- Likely reason: one `use` gives components everything; history of growth. [Inferred]

### pFIO client-server separation
- Pattern: model ranks run pFIO clients and dedicated ranks run servers; Cap splits the communicator for them (`pfio/`, `gridcomps/Cap/MAPL_Cap.F90`). [Verified]
- Likely reason: overlap output with computation. [Inferred]

### MAPL_cfio kept although the README calls it deprecated
- Pattern: `MAPL_cfio` is built twice (`MAPL_cfio_r4`, `MAPL_cfio_r8`) and `MAPL.base` depends on `MAPL_cfio_r4` (`base/CMakeLists.txt`, `MAPL_cfio/CMakeLists.txt`). [Verified]
- Likely reason: existing code still calls it. [Inferred]
- Question for maintainers: which callers keep it needed?

### Umbrella module re-exporting everything
- Pattern: the module `MAPL` uses the umbrella modules of most sub-libraries (`MAPLBase_mod`, `MAPL_GenericMod`, `pFIO`, `MAPL_GridCompsMod`, ...) mostly without `only:` lists, so components write `use MAPL` (`MAPL/MAPL.F90`). [Verified]
- Likely reason: simple component code and a stable entry point as libraries are split. [Inferred]
