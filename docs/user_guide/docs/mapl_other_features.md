## Miscellaneous Features: MAPL_Utils

Many aspects of the ESMF infrastructure, such as those dealing with
time management, error logging, etc., can easily be used directly by
modelers. Elements of the infrastructure that involve interfaces to
ESMF's communications layer, which are intended to be among ESMF most
powerful methods, are not as easy to adopt. The major hurdle to using
these elements of the ESMF infrastructure is that the user pretty much
has to put his data into ESMF_Fields, which are the main objects on
which the ESMF communication methods work. MAPL  facilitates this by
creating all elements described in data services as ESMF_Fields or ESMF_Bundles
within the three states. In user code these can be extracted directly
and manipulated as ESMF_Fields when using ESMF infrastructure,
or one can extract Fortran pointers to the data when interfacing to
existing user code.


MAPL  provides several features that, although not central to its main
goals, can be very handy. Some of these provide
functionality in an instance-specific way by saving metadata in the
MAPL  object. This saves the user the need to deal with such things in
his __private__ internal state. The main support is for profiling, error
handling, and astronomy. These are very simple and we expect that
eventually they will be superceded by ESMF utilities, or remain as
simple interfaces to them.

### Error Handling
The error handling utility consists of the three macros:

```fortran
  __VERIFY(STATUS)
  __RETURN(ESMF_Success|ESMF_Failure)
  __ASSERT(logical expr,'needs informative message')
```

These are used by setting the local character string variable `Iam` to
the soubroutine name, where possible qualified by the instance's name,
and then using `VERIFY_` to test ESMF and MAPL  return codes,
`RETURN_` to exit routines, and `ASSERT_` for conditional aborts.

### Profiling
The API of the profiling utility consists three subroutines:

```fortran
  MAPL_TimerAdd(MAPL, NAME, RC)
  MAPL_TimerOn (MAPL, NAME, RC)
  MAPL_TimerOff(MAPL, NAME, RC)
```

where `MAPL` is the MAPL  object and `NAME` is the string
name of a performance meter. Meters are usually registered in `SetServices`
with `Add` and can then be turned on and off throughout the user
code. In **MAPL_GenericFinalize** the results are reported to standard
out. Even if the user registers no meters, the performance of the
generic **MAPL_Generic_Finalize** methods is reported.

### Astronomy

The astronomy is also simple and easy to use. At any time after the
MAPL object is created (i.e., after the call to \gssv) it can be
queried for an opaque object of type **MAPL_SunOrbit**.
This orbit object can
then be used to get the insolation at the top of the atmosphere
through the following API:

```fortran
  MAPL_SunGetInsolation(LONS, LATS, ORBIT,ZTH,SLR,INTV,CLOCK,TIME,RC)
```

where `LONS` and `LATS` can be either one- or two-dimensional
Fortran arrays or general **ESMF_array**s with one or two horizontal
dimensions, `ORBIT` is the predefined object of type
`MAPL_SunOrbit`, and `ZTH` and `SLR` are the cosine of the
solar zenith angle
and the TOA insolation at the given latitudes and longitudes; these are,
of course, declared in the same way as  `LONS` and `LATS`. The
remaining arguments are optional and their use is explained in Part II(!?!?!).

By default the orbit created by MAPL  uses late $20^{th}$
orbital parameters. These can be
overidden in the configuration by specifying `ECCENTRICITY:`,
`OBLIQUITY:`, `PERIHELION:`, and `EQUINOX:`. The meanings
of these, as well as more complex uses of the astronomy are also explained
in the prologues of `MAPL_SunMod` in Part II (!?!?!).

### Universal Constants
The following universal constants are defined when `MAPLMod` is
used:

#### Mathematical Constants

| __Constant__  | __Value__ | __Unit__|
| ---- | --- | --- |
| MAPL_PI                 | 3.14159265358979323846 | ---|
| MAPL_DEGREES_TO_RADIANS | MAPL_PI / 180.0 | ---|
| MAPL_RADIANS_TO_DEGREES | 180.0 / MAPL_PI | ---|
| MAPL_KM_PER_DEG         | (1.0/(MAPL_RADIUS/1000.))  MAPL_RADIANS_TO_DEGREES | |
| MAPL_DEG_PER_KM         | (MAPL_RADIUS/1000.)  MAPL_DEGREES_TO_RADIANS        | |



#### Physical Constants

| __Constant__  | __Value__ | __Unit__ |
| --- |  --- | --- |
| MAPL_SECONDS_PER_SIDEREAL_DAY | 86164.0 | s |
| MAPL_OMEGA                    | 2.0 MAPL_PI/MAPL_SECONDS_PER_SIDEREAL_DAY    | $s^{-1}$ |
| MAPL_RADIUS                   | 6371.0E3    |            m |
| MAPL_PSDRY                    | 98305.0   |            Pa |
| MAPL_GRAV                     | 9.80665     |           $m^2 s^{-1}$  |
| MAPL_EARTH_ECCENTRICITY       | 8.1819190842622E-2 | -- |
| MAPL_EARTH_SEMIMAJOR_AXIS     | 6378137 | m |
| MAPL_AIRMW                    | 28.965                 | $kg Kmole^{-1}$ |
| MAPL_RDRY                     | MAPL_RUNIV/MAPL_AIRMW  | $J kg^{-1} K^{-1}$ |
| MAPL_CPDRY                    | 3.5 MAPL_RDRY          | $J kg^{-1} K^{-1}$ |
| MAPL_CVDRY                    | MAPL_CPDRY-MAPL_RDRY   | $J kg^{-1} K^{-1}$ |
| MAPL_RVAP                     | MAPL_RUNIV/MAPL_H2OMW  | $J kg^{-1} K^{-1}$ |
| MAPL_CPVAP                    | 4 MAPL_RVAP            | $J kg^{-1} K^{-1}$ |
| MAPL_CVVAP                    | MAPL_CPVAP-MAPL_RVAP   | $J kg^{-1} K^{-1}$ |
| MAPL_KAPPA                    | MAPL_RDRY/MAPL_CPDRY   | (2.0/7.0) |
| MAPL_EPSILON                  | MAPL_H2OMW/MAPL_AIRMW  | --  |
| MAPL_DELTAP                   | MAPL_CPVAP/MAPL_CPDRY  | -- |
| MAPL_DELTAV                   | MAPL_CVVAP/MAPL_CVDRY  | -- |
| MAPL_GAMMAD                   | MAPL_CPDRY/MAPL_CVDRY  | -- |
| MAPL_RGAS                     | MAPL_RDRY              | $J kg^{-1} K^{-1}$ (DEPRECATED) |
| MAPL_CP                       | MAPL_RGAS/MAPL_KAPPA   | $J kg^{-1} K^{-1}$ (DEPRECATED) |
| MAPL_VIREPS                   | 1.0/MAPL_EPSILON-1.0   |          (DEPRECATED) |
| MAPL_P00                      | 100000.0               | Pa  |
| MAPL_CAPICE                   | 2000.                  | $J kg^{-1} K^{-1}$ |
| MAPL_CAPWTR                   | 4218.                  | $J kg^{-1} K^{-1}$ |
| MAPL_RHOWTR                   | 1000.                  | $kg m^{-3}$ |
| MAPL_NUAIR                    | 1.533E-5               | $m^2 S^{-1}$ (@ 18C) |
| MAPL_TICE                     | 273.16                 | K  |
| MAPL_SRFPRS                   | 98470                  | Pa |
| MAPL_KARMAN                   | 0.40                   | --  |
| MAPL_USMIN                    | 1.00                   | $m/s$ |
| MAPL_RHO_SEAWATER             | 1026.0                 | $kg m^{-3}$   |
| MAPL_RHO_SEAICE               | 917.0                  | $kg m^{-3}$   |
| MAPL_RHO_SNOW                 | 330.0                  | $kg m^{-3}$   |
| MAPL_CELSIUS_TO_KELVIN        | 273.15                 | K |
| MAPL_STFBOL                   | 5.6734E-8              | $W m^{-2} K^{-4}$  |
| MAPL_AVOGAD                   | 6.023E26               | $Kmole^{-1}$ |
| MAPL_RUNIV                    | 8314.47                | $J Kmole^{-1} K^{-1}$ |
| MAPL_H2OMW                    | 18.015                 | $kg Kmole^{-1}$ |
| MAPL_O3MW                     | 47.9982                | $kg Kmole^{-1}$ |
| MAPL_ALHL                     | 2.4665E6               | $J kg^{-1}$ @15C |
| MAPL_ALHF                     | 3.3370E5               | $J kg^{-1}$ |
| MAPL_ALHS                     | MAPL_ALHL+MAPL_ALHF    | $J kg^{-1}$ |

#### MAPL lnternal Constants

| __Constant__  | __Value__ |
| --- | --- |
| MAPL_GRID_NAME_DEFAULT      | 'UNKNOWN'  |
| MAPL_GRID_FILE_NAME_DEFAULT | 'UNKNOWN'  |
| MAPL_CF_COMPONENT_SEPARATOR | '.'  |
| MAPL_TimerModeOld           |  0  |
| MAPL_TimerModeRootOnly      | 1  |
| MAPL_TimerModeMax           | 2  |
| MAPL_TimerModeMinMax        |  3 |
| MAPL_UseStarrQsat           | 1  |
| MAPL_UseGoffGratchQsat      | 2 |
| MAPL_UseMurphyKoopQsat      | 3  |
| MAPL_UseCAMQsat             | 4  |
| MAPL_Unknown                | 0 |
| MAPL_IsGather               | 1  |
| MAPL_IsScatter              | 2  |
| MAPL_TileNameLength         | 128 |
| MAPL_NoShm                  | 255  |
| MAPL_SUCCESS                | 0 |
| MAPL_FILE_NOT_FOUND         | 1  |
| MAPL_DimTopoEdge            | -1  |
| MAPL_DimTopoCyclic          | 0  |
| MAPL_DimTopoCenter          | 1  |
| MAPL_CplUNKNOWN             | 0  |
| MAPL_CplSATISFIED           | 1  |
| MAPL_CplNEEDED              | 2  |
| MAPL_Cpl_NOTNEEDED          | 4  |
| MAPL_FriendlyVariable       | 8  |
| MAPL_FieldItem              | 8  |
| MAPL_BundleItem             | 16  |
| MAPL_StateItem              | 32  |
| MAPL_NoRestart              | 64  |
| MAPL_Write2Disk             | 0  |
| MAPL_Write2RAM              | 1  |
| MAPL_VLocationNone          | 0  |
| MAPL_VLocationEdge          | 1  |
| MAPL_VLocationCenter        | 2  |
| MAPL_DimsUnknown            | 0  |
| MAPL_DimsVertOnly           | 1  |
| MAPL_DimsHorzOnly           | 2  |
| MAPL_DimsHorzVert           | 3  |
| MAPL_DimsTileOnly           | 4  |
| MAPL_DimsTileTile           | 5 |
| MAPL_DimsNone               | 6 |
| MAPL_ScalarField            | 1 |
| MAPL_VectorField            | 2 |
| MAPL_CplAverage             | 0  |
| MAPL_CplMin                 | 1  |
| MAPL_CplMax                 | 2  |
| MAPL_CplAccumulate          | 3  |
| MAPL_MinMaxUnknown          | 4  |
| MAPL_AttrGrid               | 1  |
| MAPL_AttrTile               | 2  |
| MAPL_Uninitialized          | 0  |
| MAPL_InitialDefault         | 1  |
| MAPL_InitialRestart         | 2  |
| MAPL_DuplicateEntry         | -99  |
| MAPL_ConnUnknown            | -1  |
| MAPL_Self                   | 0  |
| MAPL_Import                 | 1  |
| MAPL_Export                 | 2  |
| MAPL_FirstPhase             | 1  |
| MAPL_SecondPhase            | MAPL_FirstPhase + 1  |
| MAPL_ThirdPhase             | MAPL_SecondPhase + 1  |
| MAPL_FourthPhase            | MAPL_ThirdPhase + 1  |
| MAPL_FifthPhase             | MAPL_FourthPhase + 1  |
| MAPL_Ocean                  | 0  |
| MAPL_Lake                   | 19  |
| MAPL_LandIce                | 20  |
| MAPL_Land                   | 100  |
| MAPL_Vegetated              | 101  |
| MAPL_NumVegTypes            | 6  |
| MAPL_AGrid                  | 0  |
| MAPL_CGrid                  | 1  |
| MAPL_DGrid                  | 2  |
| MAPL_RotateLL               | 0  |
| MAPL_RotateCube             | 1  |
| MAPL_HorzTransOrderBinning  | 0  |
| MAPL_HorzTransOrderBilinear | 1  |
| MAPL_HorzTransOrderFraction | 98  |
| MAPL_HorzTransOrderSample   | 99  |
| MAPL_RestartOptional        | 0  |
| MAPL_RestartSkip            | 1  |
| MAPL_RestartRequired        | 2  |
| MAPL_Restart_Bootstrap      | 3  |
| MAPL_RestartSkipInitial     | 4  |

