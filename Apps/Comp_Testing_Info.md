# 1.
The driver requires a dso for the component you are running. To ensure this file is produced, make sure that `TYPE SHARED` is listed as a dependency in the CMakeLists.txt for the component. Now, when the model is built again the file should be produced in the "lib" directory within the install directory within your build directory.
# 2.
Add a SetServices subroutine at the bottom of the component file, after the end of the module. For example:
```f90
subroutine SetServices(gc, rc)
  use ESMF
  use SS2G_GridCompMod, only : mySetservices=>SetServices
  type(ESMF_GridComp) :: gc
  integer, intent(out) :: rc

  call mySetServices(gc, rc=rc)
end subroutine SetServices
```
# 3.
Set up your configuration file, `COMP.rc`. An example file is available under `Apps/`. Note that the captured files always follow the following format: `<component>_<state>_<before/after>_runPhase<1/2>`.\
\
The file should first contain the component you want to test and the name of the component's dso file. The `EXPORT_CHECKPOINT:` label should be the captured export before file name for the phase you are testing. The `RESTART_FILE:` label is for the import restart file name. This will either be the captured import before file, or what you want the new file name to be for the subset of import state columns, if this is turned on with the `SUBSET:` flag. Next, set the run phase to test (some components will only have one phase).\
\
Then, you will need to enter the grid information for testing. Below are some examples.

```
# Grid information:
# -----------------
# For full comparison (e.g., on c24):
  #GRID_TYPE: Cubed-Sphere
  #GRIDNAME: PE24x144-CF
  #LM: 72
  #IM_WORLD: 24
  #JM_WORLD: 144

# For a subset of columns (e.g., subset from c24):
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
```
\
The `TEST_FRAMEWORK_DRIVER:` AND `RESTORE_EXPORT_STATE:` flags should always be turned on. `GRID_CAPTURE:` should be set to `.TRUE.` if subsetting is turned on.\
\
Then, enter the restart file names. Note that not all components have an internal state (e.g., GWD). These should be the name of the captured before files, or if subsetting, the name of the new files to be created from them. You should also enter the names of the checkpoint files to be produced from the driver. Then, enter the file paths for the captured files.\
\
Lastly, if running a GOCART component, you may need to set optics parameters:
```
# Set optics parameters (required for GOCART):
# --------------------------------------------
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