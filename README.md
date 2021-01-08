# MAPL Repository

[![GEOS-ESM](https://circleci.com/gh/GEOS-ESM/MAPL.svg?style=svg)](https://app.circleci.com/pipelines/github/GEOS-ESM/MAPL)

MAPL is a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF).   MAPL fills in missing capabilities of ESMF, provides higher-level interfaces for common boiler-plate logic, and enforces various componentization conventions across ESMF gridded components within GEOS.

MAPL has 7 primary subdirectories for Fortran source code.
1. shared - low level utilities that are used throughout the remainder of MAPL.
2. profiler - time and memory profiling utility
3. pfio - high-performance client-server I/O layer
4. generic (under construction) - new home for MAPL extension of ESMF framework.
5. base (formerly MAPL_Base) - legacy core of MAPL.   This layer will gradually evaporate under further refactoring.
6. MAPL_cfio - this is a deprecated lower-level I/O layer that is generally replaced by GMAO_pFIO.    Not all of the strings have been cut yet.  Sometime soon, this directory will be eliminated.

MAPL also has a variety of other auxiliary directories:
1. include - include files used by external gridded components.
2. Apps - various Python and Perl scripts used by gridded components.
3. Python - beginnings of a run-time scripting framework for GEOS configurations
4. cmake - CMake build macros
5. MAPL_pFUnit - implements extensions of pFUnit unit testing framework that enable unit tests of grid comp run methods.   This layer should eventually be migrated into pFUnit itself.
6. Tests - miscellaneous standalone drivers.



