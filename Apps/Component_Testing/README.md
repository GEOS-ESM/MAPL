# Introduction
A former intern created a testing framework that can capture the state of a component before/after it runs and then a standalone driver to run that component. It was really only useable for GWD and GOCART due to various issues but I have confirmed it still works at least for GWD.

# Getting Before/After Restarts
Add the following flags to the `AGCM.rc` that says capture the before and after state of a component at this time.
```
COMPONENT_TO_RECORD: GWD
TEST_FRAMEWORK: .true.
TARGET_TIME: '2000-04-14 21:30:00'
MAPL_GridCapture: .true.
```
This will produce a set of files that look like this whose names should be obvious:
```
GWD_import_before_runPhase1
GWD_export_before_runPhase1
GWD_internal_before_runPhase1
GWD_import_after_runPhase1
GWD_export_after_runPhase1
GWD_internal_after_runPhase1
```
# Running the Component Standalone with Driver

## Build
We have an executable in MAPL, `Comp_Testing_Driver.x` that is meant to run a single component for a single step. You can use the checkpoints from the previous step, and get the before/after from the driver.

#### Shared object
One prerequisite is that the component must be a shared object library. To turn a component into a shared object library append the `esma_add_library` line in the `CMakeLists.txt` file like with the `TYPE SHARED` if not there. Here is an example for GWD:
```cmake
esma_add_library (
   ${this}
   SRCS ${srcs}
   DEPENDENCIES GEOS_Shared MAPL ESMF::ESMF NetCDF::NetCDF_Fortran TYPE SHARED
)
```

#### SetServices
Then in the file containing the component module add this OUTSIDE the module. Here is an example for GWD, I've included the last line of the module for clarity:
```Fortran
end module GEOS_GwdGridCompMod

subroutine SetServices(gc, rc)
   use ESMF
   use GEOS_GwdGridCompMod, only : mySetservices=>SetServices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call mySetServices(gc, rc=rc)
end subroutine SetServices
```
Essentially you need to add an extra `SetServices` that uses the `SetServices` of the module in the pattern shown above.

#### GRIDNAME
Finally in the GWD component there is a line you will have to change to run in the standalone:
```diff
-     call MAPL_GetResource(MAPL,GRIDNAME,'AGCM.GRIDNAME:', _RC)
+     call MAPL_GetResource(MAPL,GRIDNAME,'GRIDNAME:', _RC)
```

## Run

#### Input file
Now to run `Comp_Testing_Driver.x` you will need to create an `input.rc` file that is passed in as the argument to the code. Here is an example for GWD:
```
GRID_TYPE: Cubed-Sphere
GRIDNAME: PE12x72-CF
LM: 72
IM_WORLD: 12
JM_WORLD: 72

RUN_DT: 450
COMPONENT_TO_RECORD: GWD
RESTART_FILE: gwd_import_rst
LIBRARY_FILE: /discover/swdev/bmauer/models/v11.6.2/GEOSgcm/install-debug/lib/libGEOSgwd_GridComp.so
PHASE: 1
SUBSET: .false.
NX: 1
NY: 1

TEST_FRAMEWORK_DRIVER: .true.
TEST_FRAMEWORK: .true.

RESTORE_EXPORT_STATE: .true.
EXPORT_RESTART_FILE: gwd_export_rst

GWD_IMPORT_RESTART_FILE: gwd_import_rst
GWD_INTERNAL_RESTART_FILE: gwd_internal_rst

GWD_IMPORT_CHECKPOINT_FILE: gwd_import_checkpoint
GWD_INTERNAL_CHECKPOINT_FILE: gwd_internal_checkpoint
GWD_EXPORT_CHECKPOINT_FILE: gwd_export_checkpoint
BERES_FILE_NAME: newmfspectra40_dc25.nc
```
> [!NOTE]  
> 1. `NX/NY` is the per-face layout, so you need to run on 6 processors in this example
> 1. You will need to point it to your own `.so`

#### Copy files
1. Copy `GWD_import_before_runPhase1` to `gwd_import_rst`, same for internal and export
1. Copy the file `newmfspectra40_dc25.nc` to your run directory. To find it look in the GWD source code, then copy it from your original experiment under `scratch/ExtData/ ..`
1. Copy the file `GWD_GridComp.rc` to your run directory. To find it look in the GWD source code directory 

## Run
```shell
mpirun --n 6 ./Comp_Testing_Driver.x input.rc
```
If all worked you will end up with new before/after files that should match the ones generated from the model.
