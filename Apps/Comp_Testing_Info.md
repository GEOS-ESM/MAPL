# 1.
The driver requires a dso for the component you are running. To ensure this file is produced, make sure that `TYPE SHARED` is listed as a dependency in the CMakeLists.txt for the component. Now, when the model is built again the file should be produced in the "lib" directory within the install directory within your build directory.
# 2.
Add a SetServices subroutine at the bottom of the component file, after the end of the module. For example:
```
subroutine SetServices(gc, rc)
  use ESMF
  use SS2G_GridCompMod, only : mySetservices=>SetServices
  type(ESMF_GridComp) :: gc
  integer, intent(out) :: rc

  call mySetServices(gc, rc=rc)
end subroutine SetServices
```
# 3.
Set up your configuration file. Note that the captured files follow the following format: `<component>_<state>_<before/after>_runPhase<1/2>`. This should be named COMP.rc and contain the following labels:

```
COMPONENT_TO_RECORD: <e.g., DU>
LIBRARY_FILE: <e.g., libDU2G_GridComp.so>
EXPORT_CHECKPOINT: <captured export before file name for the phase you are testing>
RESTART_FILE: <import restart file name>
PHASE: <1/2 (some components only have 1 phase)>
# Grid information:
# -----------------
# For full comparison (e.g., on c24):
  #GRID_TYPE: Cubed-Sphere
  #GRIDNAME: PE24x144-CF
  #LM: 72
  #IM_WORLD: 24
  #JM_WORLD: 144

# For subset of columns (e.g., subset from c24):
  #GRID_TYPE: LatLon
  #GRIDNAME: DC12x24-PC
  #LM: 72
  #IM_WORLD: 12
  #JM_WORLD: 24
  #POLE: 'PC'
  #DATELINE: 'DC'
  #X_1: 0 # x coord of bottom left point
  #X_2: 12 # x coord of top right point
  #Y_1: 24 # y coord of bottom left point
  #Y_2: 48 # y coord of top right point

NX: 1
NY: 1

RUN_DT: 900

TEST_FRAMEWORK_DRIVER: .TRUE.
SUBSET: <.TRUE./.FALSE. whether you are taking a subset of columns>
GRID_CAPTURE: <.TRUE. when SUBSET: .TRUE.>
RESTORE_EXPORT_STATE: <.TRUE./.FALSE. to perform export restart; required for some components to get 0 diff>

DU_IMPORT_RESTART_FILE: <name of import captured before file; if subsetting, name of new file to be created from it>
# Note: not all components have an internal state
DU_INTERNAL_RESTART_FILE: <name of internal captured before file; if subsetting, name of new file to be created from it>
DU_EXPORT_RESTART_FILE: <name of export captured before file; if subsetting, name of new file to be created from it>

DU_IMPORT_CHECKPOINT_FILE: <name of driver checkpoint file to be produced>
DU_IMPORT_CHECKPOINT_TYPE: default

DU_EXPORT_CHECKPOINT_FILE: <name of driver checkpoint file to be produced>
DU_EXPORT_CHECKPOINT_TYPE: default

# Note: not all components have an internal state
DU_INTERNAL_CHECKPOINT_FILE: <name of driver checkpoint file to be produced>
DU_INTERNAL_CHECKPOINT_TYPE: default

IMPORT_CAPTURE_BEFORE_FILE: <e.g., scratch/DU_import_before_runPhase2>
IMPORT_CAPTURE_AFTER_FILE: <e.g., scratch/DU_import_after_runPhase2>
EXPORT_CAPTURE_BEFORE_FILE: <e.g., scratch/DU_export_before_runPhase2>
EXPORT_CAPTURE_AFTER_FILE: <e.g., scratch/DU_export_after_runPhase2>
INTERNAL_CAPTURE_BEFORE_FILE: <e.g., scratch/DU_internal_before_runPhase2>
INTERNAL_CAPTURE_AFTER_FILE: <e.g., scratch/DU_internal_after_runPhase2>

# Set optics parameters (required for GOCART)
# -------------------------------------------
aerosol_monochromatic_optics_wavelength_in_nm_from_LUT: 470 550 670 870

wavelengths_for_profile_aop_in_nm: 550
wavelengths_for_vertically_integrated_aop_in_nm: 550
```

# 4.
Set up AGCM.rc. The following labels should be added:
```
COMPONENT_TO_RECORD: <e.g., DU>
TEST_FRAMEWORK: .TRUE.
TARGET_TIME: <time to test at; e.g., '2000-04-14 24:00:00'>
GRID_CAPTURE: <.TRUE. when SUBSET: .TRUE.>
```

# 5.
Some components may require additional files in your environment, such as: NI2G_instance_NI.rc. For Nitrates, it relies on radius and fnum set in Dust and Sulphur. Hence, these attributes must be set manually inside of NI2G_GridCompMod.F90 for the purposes of the driver. Note that Nitrates and Sulfates components can only be tested at 3 hour intervals from the model start time (for example, if the model starts at 21:00:00, it can be run at 24:00:00, 03:00:00, etc.) due to an alarm issue with GOCART: https://github.com/GEOS-ESM/GOCART/issues/146.

# 6.
You are ready to test! Run the GEOS model until your captured files are created at the appropriate time step. Then, run the driver with `./comp_test.j`.