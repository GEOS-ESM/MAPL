MAPL Automatic Code Generator
===
The MAPL automatic code generator (**MAPL\_GridCompSpecs\_ACG.py**) automatically generates the appropriate MAPL calls to specify import/export/internal variables in a GEOS gridcomp.    This automation reduces manual coding effort, minimizes human error, increases efficiency, maintains consistency across software components (having the same coding standard), and improves the code quality.

The GEOS code can be seen as a collection of ESMF components working together to run specified simulations.
The components need to exchange variables among themselves during the model integration.
The process of defining those variables and accessing them can be tedious and subject to errors.
Using a code generator tool can alleviate such a burden and speed up code development.

Any ESMF gridded component typically requires an Import State and an Export State (if necessary an Internal State too).
Each of the states contains member variables (Fields, Bundles) that need to be registered before they are used.
The number of variables can be large and make the declaration process cumbersome
(possibly missing fields) and the declaration section in the code extremely long.

The automatic code generator relies on a formatted ASCII file (`spec` file) to automatically generate include files at compilation time containing the code segments necessary to define and access the expected state member variables.
In this document, we describe the necessary [steps](https://github.com/GEOS-ESM/MAPL/wiki/Setting-Up-MAPL-Automatic-Code-Generator) (click on the link for more detailed information) to follow to use the tool.

To simplify this document, we use the words _Imports_, _Exports_ and _Internals_ to refer to member variables of the Import, Export and Internal states, respectively.

Understanding the Issue
---

Consider the `MOIST` gridded component for example. It has over fifty (50) _Imports_ and over five hundred (500) _Exports_.
Registering them (with `MAPL_AddImportSpec` and `MAPL_AddExportSpec` calls) in the `SetServices` routine requires at least seven (7) lines of Fortran statements for each Field. 
For instance, assume that we have:
- `PLE`, `ZLE`, and `T` as _Imports_, and
- `ZPBLCN` and `CNV_FRC` as _Exports_.

The `SetServices` routine will have the calls:

<details>
<summary><font color="green">SetServices source code</font></summary>
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

</details>

Such statements for over five hundred fifty (550) fields leads to more than thirty five hundred (3500) lines of code. 
In addition, we must declare the necessary multi-dimensional arrays and access the memory location of each member variable through a `MAPL_GetPointer` call in the `Run` subroutine:

<details>
<summary><font color="green">Sample code in Run method </font></summary>

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

</details>

This is at least eleven hundred (1100) lines of code.
Basically, most (over 80%) of the source code of the `MOIST` gridded component is for the ESMF state variable registration and access.
We want to move all the calls (`MAPL_AddImportSpec`, `MAPL_AddExportSpec`, and `MAPL_GetPointer`) and the explicit array declarations into include files to facilitate the code readability and to avoid any omission.


Create the Specification (`spec`) ASCII File
---

The automatic code generator takes as input an ASCII specification file that has three main sections:

1. `category: IMPORT`: for listing the _Imports_
2. `category: EXPORT`: for listing the _Exports_
3. `category: INTERNAL`: for listing the _Internals_

Each category is organized as a data table: a set of rows and columns where each row is associated with a unique field. The first row is column names.
The mandatory columns are:

- `NAME`: name of the field as it is declared in the gridded component
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

We can add, for the sake of our example here, the optional column:

- `RESTART`: (optional and only needed for Import fields) can have the options:
     - `OPT`: `MAPL_RestartOptional`
     - `SKIP`: `MAPL_RestartSkip`
     - `REQ`: `MAPL_RestartRequired`
     - `BOOT`: `MAPL_RestartBoot`
     - `SKIPI`: `MAPL_RestartSkipInitial`

> __Note__  
> The dimensions of a field appearing in the `DIMS` column, can be listed using either the short name, say `z`, or the corresponding MAPL name, say `MAPL_DimsVertOnly`.
>

Assume that we create such a file (that we name `MyComponent_StateSpecs.rc`) and include the fields used in the previous section.
`MyComponent_StateSpecs.rc` looks like:

<details>
<summary><font color="green"> Sample spec file content</font></summary>

```
schema_version: 2.0.0
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

</details>

> __Important__  
> The header of two lines starting with `schema_version` and `component` is required. The format of these lines is *`variable_name`*: *`variable_value`* where the column and space are necessary. The schema version is `2.0.0`, currently, and the component is name of the GridComp, which in this example is `MyComponent`.


Running the automatic code generator on the file `MyComponent_StateSpecs.rc` generates four (4) include files at compilation time:

1. `MyComponent_Export___.h` for the `MAPL_AddExportSpec` calls in the `SetServices` routine:

<details>
<summary><font color="green"> Sample include file for Exports</font></summary>

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

</details>

2. `MyComponent_Import___.h` for the `MAPL_AddImportSpec` calls in the `SetServices` routine:

<details>
<summary><font color="green"> Sample include file for Imports</font></summary>

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

</details>

3. `MyComponent_DeclarePointer___.h` for all the multi-dimensional array declartions associated with the fields for all the states) in the `Run` method (The `#include MyComponent_DeclarePointer___.h` statement should be in the local declaration variable section.):

<details>
<summary><font color="green">Sample include file for pointer declarations</font></summary>

```fortran
real, pointer, dimension(:,:,:) :: PLE
real, pointer, dimension(:,:,:) :: ZLE
real, pointer, dimension(:,:,:) :: T
real, pointer, dimension(:,:)   :: ZPBLCN
real, pointer, dimension(:,:)   :: CNV_FRC
```

</details>

4. `MyComponent_GetPointer___.h` for all the `MAPL_GetPointer` calls in the `Run` method (The `#include MyComponent_GetPointer___.h` statement needs to be placed well before any field is accessed.):

<details>
<summary><font color="green">Sample include file for MAPL_GetPointer calls</font></summary>

```fortran
call MAPL_GetPointer(IMPORT, PLE,     'PLE'     , RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, ZLE,     'ZLE'     , RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, T,       'T'       , RC=STATUS); VERIFY_(STATUS)

call MAPL_GetPointer(EXPORT, ZPBLCN,  'ZPBLCN' , ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(EXPORT, CNV_FRC, 'CNV_FRC', ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
```
 
 </details>


Edit the Source Code
---

In the `SetServices` routine, the `MAPL_AddExportSpec`, `MAPL_AddImportSpec`, `MAPL_AddInternalSpec` calls for the all the variables listed in the `MyComponent_StateSpecs.rc` must be removed and replaced with the two lines just after the declaration of the local variables:

```
...
#include "MyComponent_Export___.h"
#include "MyComponent_Import___.h"
#include "MyComponent_Internal___.h"
...
```

Similarly, the array declaration section and the `MAPL_GetPointer` calls in the  `Run` routine must be are removed and replaced with the lines:

```
...
#include "MyComponent_DeclarePointer___.h"
...
#include "MyComponent_GetPointer___.h"
...
```

Edit the `CMakeLists.txt` File
---

The following lines need to be added to the  `CMakeLists.txt` file:

```
mapl_acg (${this}   MyComponent_StateSpecs.rc
          IMPORT_SPECS EXPORT_SPECS INTERNAL_SPECS
          GET_POINTERS DECLARE_POINTERS)
```

If there is no Internal state, `INTERNAL_SPECS` is not required in the above command, but there is no harm in including it. 

Additional features
---

The document [Setting Up MAPL Automatic Code Generator](https://github.com/GEOS-ESM/MAPL/wiki/Setting-Up-MAPL-Automatic-Code-Generator) lists more features of the tool.
We want to highlight two here.


### Use of asterisk to expand names

The values in the `NAME` and `LONG NAME` columns can be preceded by an asterisk (`*`). 
When the tool processes the `spec` file , the `*` is substituted with the component name.

For instance the `spec` file:

```
schema_version: 2.0.0
component: MyComponent

category: IMPORT
#-------------------------------------------------------------------------------
#  FIELD                        | DIMENSIONS  |  Additional Metadata
#-------------------------------------------------------------------------------
     NAME          | UNITS      | DIMS | VLOC | UNGRIDDED | LONG NAME
#-------------------------------------------------------------------------------
 *MASS             | kg kg-1    | xyz  | C    |           | * Mass Mixing Ratio
#-------------------------------------------------------------------------------
```

will lead to the source code:

```fortran
   call MAPL_AddImportSpec(GC,           &
       SHORT_NAME = 'MyComponentMASS',                &
       LONG_NAME  = 'MyComponent Mass Mixing Ratio',  &
       UNITS      = 'kg kg-1',               &
       DIMS       =  MAPL_DimsHorzVert,      &
       VLOCATION  =  MAPL_VLocationCenter,   &
       RC=STATUS  )

...
   real, pointer :: mycomponentmass(:,:,:)
...
   call MAPL_GetPointer(IMPORT, mycomponentmass, "MyComponentMASS", rc=status)
...
```
Note the addition of `MyComponent` to the short and long names. 
This feature can be important if we want to use the content of a `spec` file
across several instances of a component.

### Aliases

By default, the tool takes the value of the column `NAME` as the name of the
pointer variable associated with the field. 
For instance if `MASS` is the value in the `spec` file, then the created
pointer variable would be  `mass`.
It is possible to overload this variable name by adding a new column
(labelled `ALIAS`) in the `spec` file.

For instance, if we have the following in the `spec` file:

```
category: IMPORT
#---------------------------------------------------------------------------------------
#  FIELD                        | DIMENSIONS  |  Additional Metadata
#---------------------------------------------------------------------------------------
     NAME          | UNITS      | DIMS | VLOC | UNGRIDDED | ALIAS   | LONG NAME
#---------------------------------------------------------------------------------------
  MASS             | kg kg-1    | xyz  | C    |           | new_mass | Mass Mixing Ratio
#---------------------------------------------------------------------------------------
```

then the generated source code will be:

```fortran
   real, pointer, dimension(:,:,:) :: new_mass
```


### Sample code
We provide a sample code (gridded component module, `spec` and `CMakeLists.txt` files) that shows
how the automatic code generator is used. The code is available at:

[automatic code generator example](https://github.com/GEOS-ESM/MAPL/tree/cf497f98986aea2df0c818b242ad1bd35904f652/docs/tutorial/grid_comps/automatic_code_generator_example)

in the MAPL repository. The code is provided for illustration only and compiled with the entire MAPL package.