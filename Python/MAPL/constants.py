"""
Python version of MAPL Constants.
"""

MAPL_PI     = 3.14159265358979323846
MAPL_DEGREES_TO_RADIANS = MAPL_PI / 180.0
MAPL_RADIANS_TO_DEGREES = 180.0 / MAPL_PI


MAPL_PSDRY   = 98305.0                # dry surface pressure [Pa]
MAPL_SECONDS_PER_SIDEREAL_DAY = 86164.0 #s
MAPL_GRAV    = 9.80665                # m^2/s
MAPL_RADIUS  = 6371.0E3               # m
MAPL_OMEGA   = 2.0*MAPL_PI/MAPL_SECONDS_PER_SIDEREAL_DAY    # 1/s
MAPL_EARTH_ECCENTRICITY = 8.1819190842622E-2 # --
MAPL_EARTH_SEMIMAJOR_AXIS = 6378137 # m
MAPL_KM_PER_DEG = (1.0/(MAPL_RADIUS/1000.)) * MAPL_RADIANS_TO_DEGREES
MAPL_DEG_PER_KM = (MAPL_RADIUS/1000.) * MAPL_DEGREES_TO_RADIANS

MAPL_AIRMW   = 28.965                 # kg/Kmole
MAPL_RDRY    = MAPL_RUNIV/MAPL_AIRMW  # J/(kg K)
MAPL_CPDRY   = 3.5*MAPL_RDRY          # J/(kg K)
MAPL_CVDRY   = MAPL_CPDRY-MAPL_RDRY   # J/(kg K)
MAPL_RVAP    = MAPL_RUNIV/MAPL_H2OMW  # J/(kg K)
MAPL_CPVAP   = 4.*MAPL_RVAP           # J/(kg K)
MAPL_CVVAP   = MAPL_CPVAP-MAPL_RVAP   # J/(kg K)
MAPL_KAPPA   = MAPL_RDRY/MAPL_CPDRY   # (2.0/7.0)
MAPL_EPSILON = MAPL_H2OMW/MAPL_AIRMW  # --
MAPL_DELTAP  = MAPL_CPVAP/MAPL_CPDRY  # --
MAPL_DELTAV  = MAPL_CVVAP/MAPL_CVDRY  # --
MAPL_GAMMAD  = MAPL_CPDRY/MAPL_CVDRY  # --
MAPL_RGAS    = MAPL_RDRY              # J/(kg K) (DEPRECATED)
MAPL_CP      = MAPL_RGAS/MAPL_KAPPA   # J/(kg K) (DEPRECATED)
MAPL_VIREPS  = 1.0/MAPL_EPSILON-1.0   #          (DEPRECATED)
MAPL_P00     = 100000.0               # Pa
MAPL_CAPICE  = 2000.                  # J/(K kg)
MAPL_CAPWTR  = 4218.                  # J/(K kg)
MAPL_RHOWTR  = 1000.                  # kg/m^3
MAPL_NUAIR   = 1.533E-5               # m^2/S (@ 18C)
MAPL_TICE    = 273.16                 # K
MAPL_SRFPRS  = 98470                  # Pa
MAPL_KARMAN  = 0.40                   # --
MAPL_USMIN   = 1.00                   # m/s
MAPL_RHO_SEAWATER  = 1026.0          # sea water density [kg/m^3]. SA: should it be = 1026 kg/m^3?
MAPL_RHO_SEAICE    = 917.0           # sea ice   density [kg/m^3]. SA: should it be = 917  kg/m^3?
MAPL_RHO_SNOW      = 330.0           # snow density      [kg/m^3]. SA: should it be = 330  kg/m^3?
MAPL_CELSIUS_TO_KELVIN = 273.15      # K

MAPL_STFBOL  = 5.6734E-8              # W/(m^2 K^4)
MAPL_AVOGAD  = 6.023E26               # 1/kmol
MAPL_RUNIV   = 8314.47                # J/(Kmole K)

MAPL_H2OMW   = 18.015                 # kg/Kmole
MAPL_O3MW    = 47.9982                # kg/Kmole
MAPL_ALHL    = 2.4665E6               # J/kg @15C
MAPL_ALHF    = 3.3370E5               # J/kg
MAPL_ALHS    = MAPL_ALHL+MAPL_ALHF    # J/kg


#Internal constants
MAPL_GRID_NAME_DEFAULT = 'UNKNOWN'
MAPL_GRID_FILE_NAME_DEFAULT = 'UNKNOWN'
MAPL_CF_COMPONENT_SEPARATOR = '.'

MAPL_TimerModeOld = 0
MAPL_TimerModeRootOnly = 1
MAPL_TimerModeMax = 2
MAPL_TimerModeMinMax = 3

MAPL_UseStarrQsat = 1
MAPL_UseGoffGratchQsat = 2
MAPL_UseMurphyKoopQsat =3
MAPL_UseCAMQsat =4

MAPL_Unknown = 0
MAPL_IsGather = 1
MAPL_IsScatter = 2

MAPL_TileNameLength = 128

MAPL_NoShm = 255

MAPL_SUCCESS = 0
MAPL_FILE_NOT_FOUND = 1

MAPL_DimTopoEdge = -1
MAPL_DimTopoCyclic = 0
MAPL_DimTopoCenter = 1

MAPL_CplUNKNOWN = 0
MAPL_CplSATISFIED = 1
MAPL_CplNEEDED = 2
MAPL_Cpl_NOTNEEDED = 4
MAPL_FriendlyVariable = 8
MAPL_FieldItem = 8
MAPL_BundleItem = 16
MAPL_StateItem = 32
MAPL_NoRestart = 64

MAPL_Write2Disk = 0
MAPL_Write2RAM = 1

MAPL_VLocationNone = 0
MAPL_VLocationEdge = 1
MAPL_VLocationCenter = 2

MAPL_DimsUnknown = 0
MAPL_DimsVertOnly = 1
MAPL_DimsHorzOnly = 2
MAPL_DimsHorzVert = 3
MAPL_DimsTileOnly = 4
MAPL_DimsTileTile = 5
MAPL_DimsNone = 6

MAPL_ScalarField = 1
MAPL_VectorField = 2

MAPL_CplAverage = 0
MAPL_CplMin = 1
MAPL_CplMax = 2
MAPL_CplAccumulate = 3
MAPL_MinMaxUnknown = 4

MAPL_AttrGrid = 1
MAPl_AttrTile = 2

MAPL_Uninitialized = 0
MAPL_InitialDefault = 1
MAPL_InitialRestart = 2

MAPL_DuplicateEntry = -99
MAPL_ConnUnknown = -1
MAPL_Self = 0 
MAPL_Import = 1
MAPL_Export = 2

MAPL_FirstPhase = 1
MAPL_SecondPhase = MAPL_FirstPhase + 1
MAPL_ThirdPhase = MAPL_SecondPhase + 1
MAPL_FourthPhase = MAPL_ThirdPhase + 1
MAPL_FifthPhase = MAPL_FourthPhase + 1

MAPL_Ocean = 0
MAPL_Lake = 19
MAPL_LandIce = 20
MAPL_Land = 100
MAPL_Vegetated = 101
MAPL_NumVegTypes = 6

MAPL_AGrid = 0
MAPL_CGrid = 1
MAPL_DGrid = 2

MAPL_RotateLL = 0
MAPL_RotateCube = 1

MAPL_HorzTransOrderBinning = 0
MAPL_HorzTransOrderBilinear = 1
MAPL_HorzTransOrderFraction = 98
MAPL_HorzTransOrderSample = 99

MAPL_RestartOptional = 0
MAPL_RestartSkip = 1
MAPL_RestartRequired = 2
MAPL_Restart_Bootstrap = 3
MAPL_RestartSkipInitial = 4
