# MAPL Repository

This is a random edit

[![GEOS-ESM](https://circleci.com/gh/GEOS-ESM/MAPL.svg?style=svg)](https://app.circleci.com/pipelines/github/GEOS-ESM/MAPL)

[![DOI](https://zenodo.org/badge/195083467.svg)](https://zenodo.org/badge/latestdoi/195083467)

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/GEOS-ESM/MAPL)

MAPL (Modeling Analysis and Prediction Layer) is a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF).   MAPL fills in missing capabilities of ESMF, provides higher-level interfaces for common boiler-plate logic, and enforces various componentization conventions across ESMF gridded components within GEOS.

MAPL has 10 primary subdirectories for Fortran source code:

1. **shared** - low level utilities that are used throughout the remainder of MAPL.
2. **profiler** - time and memory profiling utility
3. [**pfio**](https://github.com/GEOS-ESM/MAPL/tree/main/pfio) - high-performance client-server I/O layer
4. **base** (formerly MAPL_Base) - legacy core of MAPL.   This layer will gradually evaporate under further refactoring.
5. **generic** (under construction) - new home for MAPL extension of ESMF framework.
6. **oomph** - next gen generic will eventually disappear
7. **gridcomps** - Cap, [History](https://github.com/GEOS-ESM/MAPL/tree/main/gridcomps/History), and [ExtData](https://github.com/GEOS-ESM/MAPL/tree/main/gridcomps/ExtData2G) gridcomps used by all GEOS configurations.
8. **MAPL_cfio** - this is a deprecated lower-level I/O layer that is generally replaced by GMAO_pFIO.    Not all of the strings have been cut yet.  Sometime soon, this directory will be eliminated.
9. **griddedio** - layer between ESMF container and pfio library
10. **field_utils** - utilities for manipulating data on ESMF fields in a rank- and typekind-agnostic way


MAPL also has a variety of other auxiliary directories:

1. **include** - include files used by external gridded components.
2. **Apps** - various Python and Perl scripts used by gridded components.
3. **Python** - beginnings of a run-time scripting framework for GEOS configurations
4. **cmake** - CMake build macros
5. **Tests** - miscellaneous standalone drivers.
6. **pflogger_stub** - workaround for apps that wish to avoid a dependency on pFlogger
7. **pfunit** - pFUnit (unit testing framework) extensions for ESMF components
8. **benchmarks** - miscellaneous benchmarking scripts
9. **docs** - documentation

## Installing MAPL

Please see the [INSTALL.md](INSTALL.md) file for instructions on how to install
MAPL. This also contains information on how to install the required dependencies
including subrepositories MAPL expects.

## Using MAPL

You can find simple examples on how to use MAPL components in ESMF applications at:

[MAPL Tutorial](https://github.com/GEOS-ESM/MAPL/blob/main/docs/tutorial/README.md)

A [MAPL User's Guide](https://github.com/GEOS-ESM/MAPL/blob/main/docs/user_guide/README.md) is also available to have an in depth description of MAPL components.

## Contributing

Please check out our [contributing guidelines](CONTRIBUTING.md).

## License

All files are currently licensed under the Apache-2.0 license, see [`LICENSE`](LICENSE).

Previously, the code was licensed under the [NASA Open Source Agreement, Version 1.3](LICENSE-NOSA).
