## MAPL Automatic Code Generator

Any ESMF gridded component typically requires an Import State and an Export State (if necessary an Internal State too).
Each of the states contains member variables (Fields, Bundles) that need to be registered  before they are used.
The number of the those variables can be large and make the declaration process cumbersome
(possibly missing fields) and the declaration section in the code extremely long.

MAPL has a utility tool (named [MAPL_GridCompSpecs_ACG.py
](https://github.com/GEOS-ESM/MAPL/blob/main/Apps/MAPL_GridCompSpecs_ACG.py)) that simplifies and facilitates the registration and access of member variables of the various states (Export, Import, and Internal) of gridded components.
The tool relies on a formatted ASCII file (`spec`` file) to autmatically generate, at compilation time, include files that have the necessary code segments for defining and accessing the expected state member variables.
In this document, we describe the [steps](https://github.com/GEOS-ESM/MAPL/wiki/Setting-Up-MAPL-Automatic-Code-Generator) to follow to use the tool.

To simplify this documents, we use the words _Imports_, _Exports_ and _Internals_ to refer to member variables of the Import, Export and Internal states, respectively.

### Understanding the Issue

Consider for instance the `MOIST` gridded component which code is available in the file [GEOS_MoistGridComp.F90](https://github.com/GEOS-ESM/GEOSgcm_GridComp/blob/develop/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSmoist_GridComp/GEOS_MoistGridComp.F90). 
It has over fifty (50) _Imports_ and over five hundred (500) _Export_.
Registering (with `MAPL_AddImportSpec` and `MAPL_AddExportSpec` calls) each of them in the `SetServices` routine, requires at least seven (7) lines for the code to be readble. For instance, assume that we have:
- `PLE`, `ZLE`, and `T` as _Imports_, and
- `ZPBLCN` and `CNV_FRC` as _Exports_.

The `SetServices` routine will then have the calls:

```fortran
call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'PLE',                                  &
    LONG_NAME  = 'air_pressure',                         &
    UNITS      = 'Pa',                                   &
    DIMS       = MAPL_DimsHorzVert,                      &
    VLOCATION  = MAPL_VLocationEdge,                     &
    AVERAGING_INTERVAL = AVRGNINT,                       &
    REFRESH_INTERVAL   = RFRSHINT,            RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'ZLE',                                  &
    LONG_NAME  = 'geopotential_height',                  &
    UNITS      = 'm',                                    &
    DIMS       =  MAPL_DimsHorzVert,                     &
    VLOCATION  =  MAPL_VLocationEdge,                    &
    AVERAGING_INTERVAL = AVRGNINT,                       &
    REFRESH_INTERVAL   = RFRSHINT,            RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'T',                                    &
    LONG_NAME  = 'temperature',                          &
    UNITS      = 'K',                                    &
    DIMS       = MAPL_DimsHorzVert,                      &
    VLOCATION  = MAPL_VLocationCenter,                   &
    AVERAGING_INTERVAL = AVRGNINT,                       &
    REFRESH_INTERVAL   = RFRSHINT,            RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddExportSpec(GC,                              &
    SHORT_NAME='ZPBLCN',                                 &
    LONG_NAME ='boundary_layer_depth',                   &
    UNITS     ='m'   ,                                   &
    DIMS      = MAPL_DimsHorzOnly,                       &
    VLOCATION = MAPL_VLocationNone,           RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddExportSpec(GC,                              &
    SHORT_NAME='CNV_FRC',                                &
    LONG_NAME ='convective_fraction',                    &
    UNITS     =''  ,                                     &
    DIMS      = MAPL_DimsHorzOnly,                       &
    VLOCATION = MAPL_VLocationNone,           RC=STATUS  )
VERIFY_(STATUS)

```
Having such statements for over five hundred fifty (550) fields leads to more than thirty five hundred (3500) lines of code. 
In addition, in the `Run` subroutine, we need to explicitely declare the necessary multi-dimensional arrays and access the memory location of each member variable through a `MAPL_GetPointer` call:

```fortran
real, pointer, dimension(:,:,:) :: PLE
real, pointer, dimension(:,:,:) :: ZLE
real, pointer, dimension(:,:,:) :: T
real, pointer, dimension(:,:)   :: ZPBLCN
real, pointer, dimension(:,:)   :: CNV_FRC
...
...
call MAPL_GetPointer(IMPORT, PLE,  'PLE', RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, ZLE,  'ZLE', RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, T,     'T' , RC=STATUS); VERIFY_(STATUS)

call MAPL_GetPointer(EXPORT, ZPBLCN,  'ZPBLCN' , ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(EXPORT, CNV_FRC, 'CNV_FRC', ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
```
This is at least eleven hundred (1100) lines of code.
Basically, most (over 80%) of the source code of the `MOIST` gridded component is mainly on ESMF state variable registration and access.
We want to move all the calls (`MAPL_AddImportSpec`, `MAPL_AddExportSpec`, and `MAPL_GetPointer`) and the explicit array declarations into include files to facilitate the code readability and also avoid any omission.


### Create the Specification (`spec`) ASCII File

The [MAPL_GridCompSpecs_ACG.py
](https://github.com/GEOS-ESM/MAPL/blob/main/Apps/MAPL_GridCompSpecs_ACG.py) tool takes as input an ASCII specification file that has three main sections:

1. `category: IMPORT`: for listing the _Imports_
2. `category: EXPORT`: for listing the _Expports_
3. `category: INTERNAL`: for listing the _Internals_

Each category is orgazined as a tabular data: a set of rows and columns where each row is associated with a unique field. 
Columns are labelled and a column is listed only if at least one field used it.
The mandatory columns are:

- `NAME`: name of the field as it is delared in the gridded component
- `UNIT`: unit of the field
- `DIMS`: dimensions of the field with any of the three options
     - `z`: corresponding to `MAPL_DimsVertOnly`
     - `xy`: corresponding to `MAPL_DimsHorzOnly`
     - `xyz`: corresponding to `MAPL_DimsHorzVert`
- `VLOC`: vertical location with any of the three options: 
     - `C`: corresponding to `MAPL_VlocationCenter`
     - `E`: corresponding to `MAPL_VlocationEdge`
     - `N`: corresponding to `MAPL_VlocationNone`
- `LONG NAME`:  the long name of the field (this particular column is typically the last one on the right)

We can also add for the sake of our example here, the optional column:

- `RESTART`: (optional and only needed for Import fileds) can have the options:
     - `OPT`: `MAPL_RestartOptional`
     - `SKIP`: `MAPL_RestartSkip`
     - `REQ`: `MAPL_RestartRequired`
     - `BOOT`: `MAPL_RestartBoot`
     - `SKIPI`: `MAPL_RestartSkipInitial`

#### Remark:
The dimensions of a field appearing in the `DIMS` column, can be listed using either the short name, say `z`, or the corresponding MAPL name, say `MAPL_DimsVertOnly`.

More column options are listed in the file: [MAPL_GridCompSpecs_ACG.py
](https://github.com/GEOS-ESM/MAPL/blob/main/Apps/MAPL_GridCompSpecs_ACG.py).

Assume that we create such a file (that we name `MyComponent_StateSpecs.rc`) and include the fields used in the previous section.
`MyComponent_StateSpecs.rc` looks like:


```
component: MyComponent

category: IMPORT
#----------------------------------------------------------------------------
#  VARIABLE            | DIMENSIONS  |          Additional Metadata
#----------------------------------------------------------------------------
     NAME   | UNITS    | DIMS | VLOC | RESTART | LONG NAME
#----------------------------------------------------------------------------
 ZLE        | m        | xyz  | E    |         | geopotential_height
 T          | K        | xyz  | C    | OPT     | air_temperature
 PLE        | Pa       | xyz  | E    | OPT     | air_pressure

category: EXPORT
#---------------------------------------------------------------------------
#  VARIABLE             | DIMENSIONS  |          Additional Metadata
#---------------------------------------------------------------------------
 NAME       | UNITS     | DIMS | VLOC |  LONG NAME
#---------------------------------------------------------------------------
 ZPBLCN     | m         | xy   | N    |  boundary_layer_depth
 CNV_FRC    |           | xy   | N    |  convective_fraction

category: INTERNAL
#---------------------------------------------------------------------------
#  VARIABLE    | DIMENSION   |          Additional Metadata
#---------------------------------------------------------------------------
  NAME | UNITS | DIMS | VLOC | ADD2EXPORT | FRIENDLYTO | LONG NAME
#---------------------------------------------------------------------------


#********************************************************
#
# Legend
#
#------------------------------------------------------------------
# Column label | MAPL keyword/interpretation |  Default
#--------------|---------------------------------------------------
# NAME         | short_name                  |
# UNITS        | units                       |
# DIMS         | dims                        |
# VLOC         | VLocation                   | MAPL_VLocationNone
# LONG NAME    | long_name                   |
# COND         | if (<logical-expr>) then    |  .FALSE.
# NUM_SUBTILES | num_subtiles
# ...
#------------------------------------------------------------------
#
#--------------------------------------------
# Entry alias  | Column | MAPL keyword/interpretation
#--------------|-----------------------------
# xyz          | DIMS   | MAPL_HorzVert
# xy           | DIMS   | MAPL_HorzOnly
# z            | DIMS   | MAPL_VertOnly  (plus ungridded)
# C            | VLOC   | MAPL_VlocationCenter
# E            | VLOC   | MAPL_VlocationEdge
# N            | VLOC   | MAPL_VlocationNone
#--------------------------------------------
```

Running `MAPL_GridCompSpecs_ACG.py` on the file `MyComponent_StateSpecs.rc` generates at compilation time four (4) includes files:

1. `MyComponent_Export___.h` for the `MAPL_AddExportSpec` calls in the `SetServices` routine:


```
call MAPL_AddExportSpec(GC,                              &
    SHORT_NAME='ZPBLCN',                                 &
    LONG_NAME ='boundary_layer_depth',                   &
    UNITS     ='m'   ,                                   &
    DIMS      = MAPL_DimsHorzOnly,                       &
    VLOCATION = MAPL_VLocationNone,           RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddExportSpec(GC,                              &
    SHORT_NAME='CNV_FRC',                                &
    LONG_NAME ='convective_fraction',                    &
    UNITS     =''  ,                                     &
    DIMS      = MAPL_DimsHorzOnly,                       &
    VLOCATION = MAPL_VLocationNone,           RC=STATUS  )
VERIFY_(STATUS)
```

2. `MyComponent_Import___.h` for the `MAPL_AddImportSpec` calls in the `SetServices` routine:

```fortran
call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'PLE',                                  &
    LONG_NAME  = 'air_pressure',                         &
    UNITS      = 'Pa',                                   &
    DIMS       = MAPL_DimsHorzVert,                      &
    VLOCATION  = MAPL_VLocationEdge,                     &
    AVERAGING_INTERVAL = AVRGNINT,                       &
    REFRESH_INTERVAL   = RFRSHINT,            RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'ZLE',                                  &
    LONG_NAME  = 'geopotential_height',                  &
    UNITS      = 'm',                                    &
    DIMS       =  MAPL_DimsHorzVert,                     &
    VLOCATION  =  MAPL_VLocationEdge,                    &
    AVERAGING_INTERVAL = AVRGNINT,                       &
    REFRESH_INTERVAL   = RFRSHINT,            RC=STATUS  )
VERIFY_(STATUS)

call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'T',                                    &
    LONG_NAME  = 'temperature',                          &
    UNITS      = 'K',                                    &
    DIMS       = MAPL_DimsHorzVert,                      &
    VLOCATION  = MAPL_VLocationCenter,                   &
    AVERAGING_INTERVAL = AVRGNINT,                       &
    REFRESH_INTERVAL   = RFRSHINT,            RC=STATUS  )
VERIFY_(STATUS)
```

3. `MyComponent_DeclarePointer___.h` contains all the multi-dimensional array (associated with each field used the the various states) delarations in the `Run` method (the `#include MyComponent_DeclarePointer___.h` statement should be the line of the local declaration variable declarion section):

```fortran
real, pointer, dimension(:,:,:) :: PLE
real, pointer, dimension(:,:,:) :: ZLE
real, pointer, dimension(:,:,:) :: T
real, pointer, dimension(:,:)   :: ZPBLCN
real, pointer, dimension(:,:)   :: CNV_FRC
```

4. `MyComponent_GetPointer___.h` contains all the `MAPL_GetPointer` calls in the `Run` method (the `#include MyComponent_GetPointer___.h` statement needs to be placed well before any field is accessed):

```fortran
call MAPL_GetPointer(IMPORT, PLE,     'PLE'     , RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, ZLE,     'ZLE'     , RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, T,       'T'       , RC=STATUS); VERIFY_(STATUS)

call MAPL_GetPointer(EXPORT, ZPBLCN,  'ZPBLCN' , ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(EXPORT, CNV_FRC, 'CNV_FRC', ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
```
 


### Edit the Source Code

In the `SetServices` routine, all the `MAPL_AddExportSpec` and `MAPL_AddImportSpec` calls for the variables listed in the `MyComponent_StateSpecs.rc` need to be removed and replaced with the two lines just after the declaration of the local variables:
```
...
#include "MyComponent_Export___.h"
#include "MyComponent_Import___.h"
...
```

Similarly in the  `Run` routine, the array declaration section and the `MAPL_GetPointer` calls are removed and replaced with the lines:
```
...
#include "MyComponent_DeclarePointer___.h"
...
#include "MyComponent_GetPointer___.h"
...
```

### Edit the `CMakeLists.txt` File

The following lines need to be added in the  `CMakeLists.txt` file:

```
mapl_acg (${this}   MyComponent_StateSpecs.rc
          IMPORT_SPECS EXPORT_SPECS INTERNAL_SPECS
          GET_POINTERS DECLARE_POINTERS)
```

Note, if in your case, there is no Internal state, `INTERNAL_SPECS` needs not to be added in the above command. But there is no harm including it.  

### Future Work

A future version of the tool will support a YAML specification file.
