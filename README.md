# MAPL Repository

MAPL is a foundation layer of the GEOS architecture, whose original purpose is to supplement the Earth System Modeling Framework (ESMF).   MAPL fills in missing capabilities of ESMF, provides higher-level interfaces for common boiler-plate logic, and enforces various componentization conventions across ESMF gridded components within GEOS.

MAPL has 3 primary subdirectories:
1. MAPL_Base - pretty much the core of MAPL as described above.
2. GMAO_pFIO - this is a high level I/O layer that supports input and output as well as checkpointing for GEOS.   The layer heavily uses object oriented Fortran and is intended to be highly extensible.    The layer includes client-server capabilities that allow using a dedicated set of nodes to cache output before writing to disk, and is highly parallel.
3. MAPL_cfio - this is a deprecated lower-level I/O layer that is generally replaced by GMAO_pFIO.    Not all of the strings have been cut yet.  Sometime soon, this directory will be eliminated.

