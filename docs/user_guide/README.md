# MAPL User's Guide

This document describes the Modeling Analysis and Prediction Layer (MAPL), a software layer and set of conventions standardize the use of [ESMF](http://www.earthsystemmodeling.org).
MAPL seats on to of ESMF to simplify the creation and use of ESMF gridded components
in a hierarchical architecture.
It:

1. Facilitates the porting of existing codes to ESMF.
2. Provides tools and a straightforward recipe for building
   new ESMF components.
3. Provides much greater interoperability between compliant components
   than between current ESMF compliant components.

## 1 [Introduction](docs/mapl_Introduction.md)

## 2 [ESMF- A Review of Aspects relevant to MAPL](docs/esmf_review.md)

## 3 Description of MAPL

### 3.1 [Overview](docs/mapl_overview.md)

### 3.2 [Building a MAPL Gridded Component: MAPL_Core](docs/mapl_core.md)

### 3.3 [Building complex applications: MAPL_Connect](docs/mapl_connect.md)

### 3.4 [Doing Diagnostics: History](../../gridcomps/History/HistoryGridComp.md)

### 3.5 [Connecting Import Fields to Data on File: ExtData](../../gridcomps/ExtData2G/ExtDataGridComp.md)

### 3.6 [Performing Arithmetic Operations on Fields](../../base/ArthParser.md)

### 3.7 [Doing I/O: PFIO](../../pfio/pfio.md)

### 3.8 [Miscellaneous Features: MAPL_Utils](docs/mapl_other_features.md)

### 3.9 [Automatic Code Generator](docs/mapl_code_generator.md)

<!--
## 4 [Demos](../tutorial/README.md)
-->
