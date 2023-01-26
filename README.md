# MAPL Repository

[![Scc Count Badge](https://sloc.xyz/github/GEOS-ESM/MAPL/?category=code)](https://github.com/GEOS-ESM/MAPL/)
[![Scc Count Badge](https://sloc.xyz/github/GEOS-ESM/MAPL/?category=comments)](https://github.com/GEOS-ESM/MAPL/)
[![Scc Count Badge](https://sloc.xyz/github/GEOS-ESM/MAPL/?category=blanks)](https://github.com/GEOS-ESM/MAPL/)
[![Scc Count Badge](https://sloc.xyz/github/GEOS-ESM/MAPL/?category=lines)](https://github.com/GEOS-ESM/MAPL/)

[![GEOS-ESM](https://circleci.com/gh/GEOS-ESM/MAPL.svg?style=svg)](https://app.circleci.com/pipelines/github/GEOS-ESM/MAPL)

[![DOI](https://zenodo.org/badge/195083467.svg)](https://zenodo.org/badge/latestdoi/195083467)

MAPL is a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF).   MAPL fills in missing capabilities of ESMF, provides higher-level interfaces for common boiler-plate logic, and enforces various componentization conventions across ESMF gridded components within GEOS.

MAPL has 10 primary subdirectories for Fortran source code:

1. shared - low level utilities that are used throughout the remainder of MAPL.
2. profiler - time and memory profiling utility
3. pfio - high-performance client-server I/O layer
5. base (formerly MAPL_Base) - legacy core of MAPL.   This layer will gradually evaporate under further refactoring.
6. generic (under construction) - new home for MAPL extension of ESMF framework.
7. oomph - next gen generic will eventually disappear
8. gridcomps - Cap, History, and ExtData gridcomps used by all GEOS configurations.
9. MAPL_cfio - this is a deprecated lower-level I/O layer that is generally replaced by GMAO_pFIO.    Not all of the strings have been cut yet.  Sometime soon, this directory will be eliminated.
10. griddedio - layer between ESMF container and pfio library


MAPL also has a variety of other auxiliary directories:

1. include - include files used by external gridded components.
2. Apps - various Python and Perl scripts used by gridded components.
3. Python - beginnings of a run-time scripting framework for GEOS configurations
4. cmake - CMake build macros
5. MAPL_pFUnit - implements extensions of pFUnit unit testing framework that enable unit tests of grid comp run methods.   This layer should eventually be migrated into pFUnit itself.
6. Tests - miscellaneous standalone drivers.
7. pflogger_stub - workaround for apps that wish to avoid a dependency on pFlogger
8. pfunit - pFUnit (unit testing framework) extensions for ESMF components

## Contributing

Please check out our [contributing guidelines](CONTRIBUTING.md).

## License

All files are currently licensed under the Apache-2.0 license, see [`LICENSE`](LICENSE).

Previously, the code was licensed under the [NASA Open Source Agreement, Version 1.3](LICENSE-NOSA).
