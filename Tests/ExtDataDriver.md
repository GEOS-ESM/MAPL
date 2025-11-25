# Introducton
ExtDataDriver.x is a program maintained as part of the MAPL library. Despite the name the program is designed to allow for testing of BOTH the ExtData and History components for CI testing, development, and troubleshooting these two components. The program basically implements something like the MAPL Cap but for several reasons does not use the MAPL cap. This "CAP" object instantiates ExtData/History/and a root MAPL component just like the regular MAPL Cap and can tilmestep these components in the same order but allows for a little finer grain control. In addition the the program allows to user to run essentially multiple executions of this "Cap" within a single execution of the overall ExtDataDriver.x and modify the behavior of ExtData, History, and root component in each execution. Finally as part of the application a custom MAPL component has been implemented. This component allows runtime fields to be added to the component in either the import or export state on any supported layout and grid that MAPL supports and in the case of export fields fill those with synthetic data. In this way, pretty much any field or group of fields can be generated to feed to either ExtData or History for testing/development/debugging purposes. An obvious use case if a user for example says, I tried this combination of History options for a collection and it failed. The the root component can be setup to produce any number/dimensionality of fields that can be written by History where it can be debugged to mimic the fields the user may be trying to use for example.

# Overview of ExtDataDriver.x
Like the a regular MAPL application that uses the MAPL Cap ExtDataDriver.x is driven by a CAP.rc which in turn specifies the History.rc, extdata.yaml, and rc file for the root of the MAPL hierarchy so this should be familiar to anyone who has used the GEOS model.

The main differences are:
1. The "CAP.rc" actually specifies a list of other CAP.rc files (I'll call those the sub_CAP.rc files that are similar to the regular CAP.rc and allows one execution of the program to run multiple instantiations of the History/ExtData/MAPL hierarchy in one execution. The reasons for why this is powerful will hopefully become clear in the examples.
2. The sub_CAP.rc files are similar to the regular CAP.rc in that they specify the HEARTBEAT and the name of the Root/History/extdata.yaml files but allows for more flexibility. For example these allow the user specify times the system should run for example.
3. The root component rc file specifies the component grid just like the AGCM.rc in the GEOS model and has options to at runtime specify the fields that should to in the import and export state of the component, as well as the dimensionality and how to fill with synthetic data. It also doing some operations with these fields that will be described later.

# Code Structure
**ExtDataDriver.x** consists of the following pieces that can be found under the Test directory in MAPL

**ExtDataDriver.F90**: This is the top level program, not much here, looks like GEOSgcm.x

**ExtDataDriverMod.F90**: This is basically a reimplementation of MAPL_Cap with some extra bells and whistles. It initializes MAPL, starts the output server and runs the "cap-like" grid comp which is the grid comp defined in ExtDataDriverGridComp.F90. The crucial difference is that it can run multiple instanciations of the GridComp defined in ExtDataDriverGridComp.F90 sequentially if desired.

**ExtDataDriverGridComp.F90**: This is basically a reimplementation of MAPL_CapGridComp with a few more bells and whistles but at the end of the day runs ExtData, a root component, and History at one or more time steps and ticks a clock. It just allows more fine-grained control over when it's 3 child components run rather than just ticking a clock and running them at every step.

**ExtDataRoot_GridComp.F90**: This is a MAPL component that is the root component (and only component) underneath the ExtDataDriverGridComp. Like any good gridded component it creates a grid, some fields in its import/export/internal state on that grid and may do something with them.

# Example Inputs
As part of the MAPL library, inputs for multiple test cases that are used to run ExtDataDriver.x for CI testing are maintained. These can be found under the main MAPL directory at `Tests/ExtData_Testing_Framework/test_cases`.

# Test Case 1
To illustrate how ExtDataDriver.x works I will go over the various input files for case1 under the test cases

## Main CAP.rc file
```
CASES::
CAP1.rc
CAP2.rc
::
```
The main CAP.rc is just a list of the sub CAP.rc files. It instantiates a ExtDataDriverGridComp for each one, runs said instance using the sub CAP.rc sequentially. If there is only one item in this list, it is very must like just the standard MAPL_Cap. In the example above, it will will instantiate a ExtDataDriverGridComop using CAP1.rc, run it (this means run it's init/run/finalize methods), then instantiate a new gridcomp using CAP2.rc and run that.

## sub_CAP.rc file(s)
Let's look at CAP1.rc:
```
ROOT_NAME: Root
ROOT_CF: AGCM1.rc
HIST_CF: HISTORY1.rc

BEG_DATE:     20040101 210000

JOB_SGMT:     00001200 000000
HEARTBEAT_DT:   3600

#RUN_EXTDATA: .false.
RUN_TIMES::
20040115 210000
20040215 210000
20040315 210000
20040415 210000
20040515 210000
20040615 210000
20040715 210000
20040815 210000
20040915 210000
20041015 210000
20041115 210000
20041215 210000
::
```
This looks similar to the regular CAP.rc used when running GEOS with a few exceptions. At the top it defines the ROOT configuration files and the ExtData and History configuration files. In this case it says, my root component will used a file named AGCM1.rc and my History will use a file name HISTORY1.rc and since I didn't specify an ExtData file it uses the default name. It defines the BEG_DATE of the clock it will create, the length of time to tick that clock (JOB_SGMT), and the HEARTBEAT_DT. The difference the RUN_TIMES table. This is **optional** but says, only execute the child (History/Root/ExtData) at these specify times. If omitted it just ticks the clock for the desired length and runs at every step.

At the steps when ExtDataDriverGridComp does execute the full run method it will execute ExtData, the root component, then History.

CAP2.rc is similar just points to AGCM2.rc and HISTORY2.rc as the config files used by the root and History component in that instantiation.

## Root component RC file
Now lets look at AGCM1.rc:
```
NX: 1
NY: 1

Root.GRID_TYPE: LatLon
Root.GRIDNAME: DC90x45-PC
Root.LM: 3
Root.IM_WORLD: 90
Root.JM_WORLD: 45
Root.POLE: 'PC'
Root.DATELINE: 'DC'

RUN_MODE: GenerateExports

EXPORT_STATE::
VAR2D , time , days , xy , c
VAR3D , time , days , xyz , c
::

FILL_DEF::
VAR2D time
VAR3D time
::

REF_TIME: 20040701 000000
```
We will go line by line.

Lines 1 and 2 define the layout that will be used by component when making the grid. In this case it is set to 1x1 so setup to run on a single MPI process.

Lines 4 to 10 are defining a 90x45 lat-on grid. This can also be a cubed-sphere or tripolar. If you look in a GEOSgcm.x AGCM.rc this follows the same conventions for the grid definition used there.

Line 12 defines what the root gridcomp will do. All options will be explained later. In this case we have set "GenerateExports", so all it will do is fill any exports using the definitions later on in the file.

Lines 14 to 17 defines the fields that will be added to the export state. **Note this is completely defined at run time, i.e you can change the export (and import state) of this component with no code recompilation!** The general syntax is:

`SHORT_NAME , LONG_NAME , UNITS , dimensionality (xy or xyz) , vertical coordinate (c or e) if applicable`

and results in a MAPL_AddExportSpec being added with the SHORT_NAME, LONG_NAME, UNITS, and dimensionality described in the entry. If you do xy the result is an export spec with MAPL_DimsHorzOnly, and if you do xyz and c you get an export spec with MAPL_DimsHorzVert and MAPL_VLocationCenter.

You can have as many entries here as you want. In the AGCM1.rc example we add a single 2D and a single 3D variable. Note there are some limitations. In particular there is no mechanism now to add varspecs with ungridded dimensions.

Finally lines 19 to 22 define what to fill the export variables with. You basically give it an expression that is a function of the allowed input variables. In this case we are filling them with a variable named time which is a constant field that is the delta relative to the reference time (defined on line 24). Another example to specify expressions for fields with spherical coordinates is
```
FILL_DEF::
VAR2D cos(lons)*cos(lats)
VAR3D cos(lons)*cos(lats)
::
```

Now lets look at AGCM2.rc:

```
NX: 1
NY: 1

Root.GRID_TYPE: LatLon
Root.GRIDNAME: DC90x45-PC
Root.LM: 3
Root.IM_WORLD: 90
Root.JM_WORLD: 45
Root.POLE: 'PC'
Root.DATELINE: 'DC'

RUN_MODE: CompareImports

IMPORT_STATE::
VAR2D , time , days , xy , c
VAR3D , time , days , xyz , c
::

EXPORT_STATE::
VAR2D , time , days , xy , c
VAR3D , time , days , xyz , c
::

FILL_DEF::
VAR2D time
VAR3D time
::

REF_TIME: 20040701 000000
```

Notice this looks exactly like AGCM1.rc with the following differences. First RUN_MODE is set to CompareImports. What this will do is compare the import state and export state field by field and fail if there is any difference in the fields. Note this requires the import and export state to have the same field set. Second we now have an IMPORT_STATE list. This defines the import fields of the component and follows the exact same syntax as the export field list. Note there no option to fill the imports like the exports as a component should not be modifying it's imports!

## History and ExtData files
There is really nothing to say about these. They are just the input to History and ExtData so you can look elsewhere for the syntax but in this case HISTORY1.rc is saying output VAR2D and VAR3D from a component named Root into a single file per year and HISTORY2.rc defines no collections to a History instantiation using this will do nothing!

## Why Have a CAP1.rc and CAP2.rc and Run in One Execution
So by now you are probably asking what's the point of of this. Why not just run ExtDataDriver.x twice and not bother with this CAP.rc file that itself specifies the individual RC files. You are right, you could do that but this way I can get away with one execution. But you will say, ok fine, but what is the point of this case?

In this case and indeed all the test cases I have in MAPL I can do self consistent testing of both History AND ExtData. In one execution of ExtDataDriver.x I can have it generate output files via History, then read those files back in via ExtData, then test that what I read in were read in properly (since I know what I put out in the first place!). That's the point of these test cases.

So CAP1.rc defines a instantiation of ExtDataDriverGridComp that outputs some stuff in History from the exports of the root component which are filled with data by the root component. CAP2.rc defines an instantiation of ExtDataDriverGridComp that outputs nothing (since HISTORY2.rc), but now has some imports which get filled with ExtData using the files generated in the previous iteration, it has some exports that are filled by root component and it compares the states, field by field. If they don't match, something either in History or ExtData did not do something right!

# All Options for Root Component RC File
As you hopefully have seen, gridded component defined in the ExtDataRoot_GridComp.F90 basically allows one to specify the import and export fields of that grid comp maybe do a few things with them like fill the exports or compare the imports to the exports.

## Runtime Behavior
To modify this behavior you use the RUN_MODE: option and it can be set to:

**GenerateExports** - this simply adds the export field  and fills them with the definition

**CompareImports** - this adds both the imports and export fields, fills the export fields, then compares these to the import fields

**FillExportsFromImports** - this adds both the imports and export fields, but rather than use the fill refs for the exports, copies the imports to the exports

**FillImport** - this really does nothing, it add the import fields and that's it

## Specifying Import and Export State
The import and export state of the root component is specified via a tables named `EXPORT_STATE` and `IMPORT_STATE`. Each table consists of multiple lines where each line is a comma separated list with the following values.

`short_name , units , long_name , horiztonal_defintion , vertical_definition`

The `horizontal_definition`, can be xy for 2D gridded, xyz for 3D gridded, or tileonly for tiles
The `vertical_definition`, can be c or e (center or edge), if the `horizontal_definition` was 2D or tile this is ignored

These are ultimately translated into the standard MAPL mechanism to add the fields via the MAPL AddSpec calls.

## Setting the Fill Definitions for Export Fields
If the run mode is set to GenerateExports, each export state is filled using the supplied definition in the `FILL_DEF::` table. Each line of the table consists of the export name followed by an expression. The expression can be any valid expression understood by the MAPL arithemtic parser. The allowed variable names are any of the predefined variables 2D (or 1D if on tiles) in the internal state of the Root component. At this time the following variables are available:
If the run mode is set to GenerateExports, each export state is filled using the supplied definition in the `FILL_DEF::` table. Each line of the table consists of the export name followed by an expression. The expression can be any valid expression understood by the MAPL arithemtic parser. Since the parser supports broadcasting these variables may be used to fill either 2D or 3D variables. The allowed variable names are any of the predefined variables in the internal state of the Root component. At this time the following variables are available:
1. `time` - time relative to the reference time, if not specified this is the initialization time of the component. To specify a reference time add a `REF_TIME: integer_date integer_time` keyword to the resource file, where the value of the is two 8 digit integer encodings for the date and time
2. `lats` - latitudes of the root component grid
3. `lons` - longitude of the root component grid
4. `i_index` - i index of the root component grid
5. `j_index` - j index of the root component grid
6. `doy` - integer day of the year
7. `rand` - the array is filled with random numbers using Fortran intrinsic random number generator

Note that if `ExtDataDriver.x` was set to run on tiles, then the `lons`, `last`, `i_index`, and `j_index` can not be used. Also note that `time` and `doy` are not spatially varying and constant for every grid point.

## Running ExtDataDriver.x on MAPL Tiles

`ExtDataDrivers.x` root component can be run on a MAPL tile grid rather than a standard grid. To do this simply define the corresponding grid that the tile file you will provide was created for then specify:
`tiling_file: tile_file` where you define the path to the tile file. This is the standard tile file used by MAPL and can be in either binary or ASCII form. If this keyword is found the Root component will be on a tile grid. Don't forget ot update your `EXPORT_STATE` and `IMPORT_STATE` definitions. Also note in the previous section the limitations on filling exports.
