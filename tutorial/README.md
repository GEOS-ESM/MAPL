# MAPL Tutorial - User's Guide

This document describes MAPL, a software layer that
establishes usage standards and software tools for building
[ESMF](http://www.earthsystemmodeling.org)
compliant components. 
This package:

1. Facilitates the porting of existing codes to ESMF
2. Provides tools and a straightforward recipe for building
   new ESMF components, and
3. Provides much greater interoperability between compliant components 
   than between current ESMF compliant components (!?!?!).

## 1 [Introduction](docs/mapl_Introdction.md)

## 2 [ESMF- A Review of Aspects relevant to MAPL](esmf_review.md)

## 3 Description of MAPL

### 3.1 [Overview](docs/mapl_overview.md)

### 3.2 [Building a MAPL Gridded Component: MAPL_Core](docs/mapl_core.md)

### 3.3 [Building complex applications: MAPL_Connect](docs/mapl_connect.md)

### 3.4 [Doing Diagnostics: History](../gridcomps/History/HistoryGridComp.md)

### 3.5 [Connecting Import Fields to Data on File: ExtData](../gridcomps/ExtData2G/ExtDataGridComp.md)

### 3.6 [Performing Arithmetic Operations on Fields](../base/ArthParser.md)

### 3.7 [Doing I/O: PFIO](../pfio/pfio.md)

### 3.8 [Miscellaneous Features: MAPL_Utils](docs/mapl_other_features.md)

## 4 [Running Examples](docs/mapl_running_examples.md)
