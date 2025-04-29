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

To simplify this document, we use the words *IMPORTS*, *EXPORTS*, and *INTERNALS* to refer to member variables of the Import, Export and Internal states, respectively.

Understanding the Issue
---

Consider the `MOIST` gridded component for example. It has over fifty (50) *IMPORTS* and over five hundred (500) *EXPORTS*.
Registering them (with `MAPL_AddImportSpec` and `MAPL_AddExportSpec` calls) in the `SetServices` routine requires at least seven (7) lines of Fortran statements for each Field. 
For instance, assume that we have:
- `PLE`, `ZLE`, and `T` as *IMPORTS*, and
- `ZPBLCN` and `CNV_FRC` as *EXPORTS*.


The `SetServices` routine will contains the `MAPL_AddImportSpec` and `MAPL_AddExportSpec` calls.

<details>
<summary><font color="green">Click to see SetServices source code.</font></summary>

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
In addition, we must declare the necessary multi-dimensional arrays and access the memory location of each member variable through a `MAPL_GetPointer` call in the `Run` subroutine.

<details>
<summary><font color="green">Click to see Run source code.</font></summary>

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

The automatic code generator takes as input an ASCII specification file that has a header and followed by blocks for the ESMF gridded component Fields, which are grouped by `ESMF_State`. The first two lines form the header which takes this form:

<pre>
schema_version: 2.0.0
component: <i>component_name</i>
</pre>

The space in each line is significant. The current `schema_version` is
`2.0.0`, and *`component_name`* is the name of the GridComp. A blank
line separates the header from the State blocks, and each State block is
separated from the next State block by a blank line.

Each block starts with a single line:

<pre>
category: <i>STATE</i>
</pre>

where *`STATE`* is the `ESMF_State` for the fields in the block: `IMPORT`, `EXPORT`, or `INTERNAL`. The colon and the space are significant. The fields follow the category line, and they are organized into a data table. The first row of the table is the column names, and the remaining rows are the fields terminated by a blank line. Each row contains the values for the columns for a single field. Most of the columns are arguments for the `MAPL_Add`*`STATE`*`Spec` procedure. In addition, there are special columns, which are discussed later in this document. 

The mandatory columns are:

- `SHORT_NAME`
- `UNITS`
- `LONG_NAME`
- `DIMS`
     - `z`: `MAPL_DimsVertOnly`
     - `xy`: `MAPL_DimsHorzOnly`
     - `xyz`: `MAPL_DimsHorzVert`
- `VLOC`
     - `C`: `MAPL_VlocationCenter`
     - `E`: `MAPL_VlocationEdge`
     - `N`: `MAPL_VlocationNone`

We can add, for the sake of our example here, the optional column:

- `RESTART`
     - `OPT`: `MAPL_RestartOptional`
     - `SKIP`: `MAPL_RestartSkip`
     - `REQ`: `MAPL_RestartRequired`
     - `BOOT`: `MAPL_RestartBoot`
     - `SKIPI`: `MAPL_RestartSkipInitial`

Note that some columns have literal values, like `SHORT_NAME`, while others like `DIMS` have a set of allowed values with abbreviations for the allowed values as listed above.

### Row Value Abbreviations
The following abbreviations can be used:

| Column Name &nbsp; | Row Value &nbsp; | *Abbreviation* |
| :--- | :--- | :--- |
| `DIMS` &nbsp; | `MAPL_DimsVertOnly` &nbsp; | *`Z`* |
|  | `MAPL_DimsHorzOnly` &nbsp; | *`XY`* |
|  | `MAPL_DimsHorzVert` &nbsp; | *`XYZ`* |
| `VLOCATION` | `MAPL_VlocationCenter` &nbsp; | *`C`* |
|  | `MAPL_VlocationEdge` &nbsp; | *`E`* |
|  | `MAPL_VlocationNone` &nbsp; | *`N`* |
| `RESTART_EMIT` | `MAPL_RestartOptional` &nbsp; | *`OPT`* |
| | `MAPL_RestartSkip` &nbsp; | *`SKIP`* |
| |`MAPL_RestartRequired` &nbsp; | *`REQ`* |
| | `MAPL_RestartBoot` &nbsp; | *`BOOT`* |
| | `MAPL_RestartSkipInitial` &nbsp; | *`SKIPI`* |
| `ADD2EXPORT` | `.TRUE.` | *`T`* |
| | `.FALSE.` | *`F`* |
 
Because the rows are delimited by the pipe symbol, the row values do not appear in quotes or brackets, and commas are treated as part of the value.
In a block, if a column is blank in a Field row, that column is ignored for the Field. 

Assume that we create such a file (that we name `MyComponent_StateSpecs.rc`) and include the fields used in the previous section.
`MyComponent_StateSpecs.rc` looks like:

```
schema_version: 2.0.0
component: MyComponent

category: IMPORT
#----------------------------------------------------------------------------
#  VARIABLE            | DIMENSIONS  |          Additional Metadata
#----------------------------------------------------------------------------
 SHORT_NAME | UNITS    | DIMS | VLOC | RESTART | LONG_NAME
#----------------------------------------------------------------------------
 ZLE        | m        | xyz  | E    |         | geopotential_height
 T          | K        | xyz  | C    | OPT     | air_temperature
 PLE        | Pa       | xyz  | E    | OPT     | air_pressure

category: EXPORT
#---------------------------------------------------------------------------
#  VARIABLE             | DIMENSIONS  |          Additional Metadata
#---------------------------------------------------------------------------
 SHORT_NAME | UNITS     | DIMS | VLOC |  LONG_NAME
#---------------------------------------------------------------------------
 ZPBLCN     | m         | xy   | N    |  boundary_layer_depth
 CNV_FRC    |           | xy   | N    |  convective_fraction

category: INTERNAL
#---------------------------------------------------------------------------
#  VARIABLE    | DIMENSION   |          Additional Metadata
#---------------------------------------------------------------------------
  SHORT_NAME | UNITS | DIMS | VLOC | ADD2EXPORT | FRIENDLYTO | LONG_NAME
#---------------------------------------------------------------------------


```

Running the automatic code generator on the file `MyComponent_StateSpecs.rc` generates four (4) include files at compilation time:

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

3. `MyComponent_DeclarePointer___.h` for all the multi-dimensional array declartions associated with the fields for all the states) in the `Run` method (The `#include MyComponent_DeclarePointer___.h` statement should be in the local declaration variable section.):

```fortran
real, pointer, dimension(:,:,:) :: PLE
real, pointer, dimension(:,:,:) :: ZLE
real, pointer, dimension(:,:,:) :: T
real, pointer, dimension(:,:)   :: ZPBLCN
real, pointer, dimension(:,:)   :: CNV_FRC
```

4. `MyComponent_GetPointer___.h` for all the `MAPL_GetPointer` calls in the `Run` method (The `#include MyComponent_GetPointer___.h` statement needs to be placed well before any field is accessed.):

```fortran
call MAPL_GetPointer(IMPORT, PLE,     'PLE'     , RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, ZLE,     'ZLE'     , RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(IMPORT, T,       'T'       , RC=STATUS); VERIFY_(STATUS)

call MAPL_GetPointer(EXPORT, ZPBLCN,  'ZPBLCN' , ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
call MAPL_GetPointer(EXPORT, CNV_FRC, 'CNV_FRC', ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
```

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
#include "MyComponent_GetPointer___.h"*IMPORTS*
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

Columns
---

These are the possible columns in a spec file:

- `DIMS`
- `LONG_NAME`
- `SHORT_NAME`
- `UNITS`
- `ADD2EXPORT`
- `ATTR_INAMES`
- `ATTR_IVALUES`
- `ATTR_RNAMES`
- `ATTR_RVALUES`
- `AVERAGING_INTERVAL`
- `DATATYPE`
- `DEFAULT`
- `DEPENDS_ON_CHILDREN`
- `DEPENDS_ON`
- `FIELD_TYPE`
- `FRIENDLYTO`
- `HALOWIDTH`
- `NUM_SUBTILES`
- `PRECISION`
- `REFRESH_INTERVAL`
- `RESTART`
- `ROTATION`
- `STAGGERING`
- `UNGRIDDED_DIMS`
- `UNGRIDDED_COORDS`
- `UNGRIDDED_NAME`
- `UNGRIDDED_UNIT`
- `VLOCATION`
- `ALIAS`
- `ALLOC`
- `CONDITION`

Additional features
---

The document [Setting Up MAPL Automatic Code Generator](https://github.com/GEOS-ESM/MAPL/wiki/Setting-Up-MAPL-Automatic-Code-Generator) lists more features of the ACG.
A few are discussed below.

### Conditional Fields
The `CONDITION` column places an `if` block around the procedure calls for a Field.
For example, if the `IMPORT` block includes the `CONDITION` column, `if` blocks will be placed around the `MAPL_AddImportSpec` and `MAPL_GetPointer` calls for any Fields 
where the `CONDITION` column is not blank. For this `IMPORT` block:
    
```fortran
category: IMPORT
#----------------------------------------------------------------------------
#  VARIABLE            | DIMENSIONS  |          Additional Metadata
#----------------------------------------------------------------------------
 SHORT_NAME | UNITS    | DIMS | VLOC | CONDITION   | LONG_NAME
#----------------------------------------------------------------------------
 PU         | ppm      | xyz  | E    | NOX==.TRUE. | SO2 concentration
 T          | K        | xyz  | C    |             | air_temperature
```

the ACG would generate this Fortran code:

```fortran
if (NOX==.TRUE.) then
    call MAPL_AddImportSpec(GC,                              &
        SHORT_NAME = 'PU',                                   &
        LONG_NAME  = 'SO2 concentration',                    &
        UNITS      = 'ppm',                                  &
        DIMS       = MAPL_DimsHorzVert,                      &
        VLOCATION  = MAPL_VLocationEdge,                     &
        RC=STATUS  )
    VERIFY_(STATUS)
end if

call MAPL_AddImportSpec(GC,                              &
    SHORT_NAME = 'T',                                    &
    LONG_NAME  = 'temperature',                          &
    UNITS      = 'K',                                    &
    DIMS       = MAPL_DimsHorzVert,                      &
    VLOCATION  = MAPL_VLocationCenter,                   &
    RC=STATUS  )
VERIFY_(STATUS)
```

and

```fortran
if (NOX=.TRUE.) then
    call MAPL_GetPointer(IMPORT, PU,     'PU'     , RC=STATUS); VERIFY_(STATUS)
else
    nullify(PU)
end if
call MAPL_GetPointer(IMPORT, T,       'T'       , RC=STATUS); VERIFY_(STATUS)
```

in the include files for *IMPORTS* and the `MAPL_GetPointer` calls, respectively.

### Column Name Abbreviations
The following abbreviations can be used for some of the column names:

| Column Name &nbsp; | *Abbreviation* |
| :----- | :------------ |
| `SHORT_NAME` &nbsp;| *`NAME`* |
| `LONG_NAME` &nbsp; | *`LONG NAME`* |
| `ADD2EXPORT` &nbsp; | *`ADDEXP`* |
| `AVERAGING_INTERVAL` &nbsp; | *`AVINT`* |
| `FRIENDLYTO` &nbsp; | *`FRIEND2`* |
| `NUM_SUBTILES` &nbsp; | *`NUMSUBS`* |
| `PRECISION` &nbsp; | *`PREC`* |
| `UNGRIDDED_DIMS` &nbsp; | *`UNGRID`* |
|  &nbsp; | *`UNGRIDDED`* |
| `VLOCATION` &nbsp; | *`VLOC`* |
| `CONDITION` &nbsp; | *`COND`* |

### Use of asterisk to expand names

The values in the `SHORT_NAME` and `LONG_NAME` columns can be preceded by an asterisk (`*`). 
When the tool processes the `spec` file , the `*` is substituted with the component name.

For instance the `spec` file:

```
schema_version: 2.0.0
component: MyComponent

category: IMPORT
#-------------------------------------------------------------------------------
#  FIELD                        | DIMENSIONS  |  Additional Metadata
#-------------------------------------------------------------------------------
     SHORT_NAME          | UNITS      | DIMS | VLOC | UNGRIDDED | LONG_NAME
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

### Pointer Variable Names

By default, the ACG uses the value of the column `SHORT_NAME` as the name of the
pointer variable associated with the field. For instance if `mass` is the short name in 
the `spec` file, the created pointer variable is `mass`. To use an alternate name for the pointer variable add the column `ALIAS`, and supply the alternate name in the specification file.

For instance, if we have the following in the `spec` file:

```
category: IMPORT
#---------------------------------------------------------------------------------------
#  FIELD                        | DIMENSIONS  |  Additional Metadata
#---------------------------------------------------------------------------------------
     SHORT_NAME    | UNITS      | DIMS | VLOC | UNGRIDDED | ALIAS   | LONG_NAME
#---------------------------------------------------------------------------------------
  MASS             | kg kg-1    | xyz  | C    |           | new_mass | Mass Mixing Ratio
#---------------------------------------------------------------------------------------
```

then the generated source code will be:

```fortran
   real, pointer, dimension(:,:,:) :: new_mass
```

### ALLOC
The `ALLOC` column adds the `ALLOC` argument to the `MAPL_GetPointer` call for any Field
for which the `ALLOC` column is not blank. This block in the spec file:

```fortran
category: IMPORT
#---------------------------------------------------------------------------------------
#  FIELD                        | DIMENSIONS  |  Additional Metadata
#---------------------------------------------------------------------------------------
     SHORT_NAME    | UNITS      | DIMS | VLOC |   ALLOC  | LONG_NAME
#---------------------------------------------------------------------------------------
  T                | K          | xyz  | C    |  .TRUE.  | air_temperature
#---------------------------------------------------------------------------------------
```

would produce this code in the `MAPL_GetPointer` include file:

```fortran
call MAPL_GetPointer(IMPORT, T, 'T', ALLOC=.TRUE., RC=STATUS); VERIFY_(STATUS)
```

### Sample code
We provide a sample code (gridded component module, `spec` and `CMakeLists.txt` files) that shows
how the automatic code generator is used. The code is available at:

[automatic code generator example](https://github.com/GEOS-ESM/MAPL/tree/cf497f98986aea2df0c818b242ad1bd35904f652/docs/tutorial/grid_comps/automatic_code_generator_example)

in the MAPL repository. The code is provided for illustration only and compiled with the entire MAPL package.
