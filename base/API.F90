module mapl_base_api
   use mapl_FileMetadataUtils_mod
   use mapl_FileMetadataUtilsVector_mod
   ! StringTemplate is in mp_utils/ - should be exported from mapl_mp_utils_export
   use mapl_MemUtils_mod, only: MAPL_MemUtilsInit, MAPL_MemUtilsDisable, &
         MAPL_MemUtilsWrite, MAPL_MemUtilsIsDisabled, MAPL_MemUtilsFree, &
         MAPL_MemCommited, MAPL_MemUsed, MAPL_MemReport
   use mapl_Sun_mod, only: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig, &
         MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery, &
         MAPL_SunGetInsolation, MAPL_SunGetSolarConstant, &
          MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax, &
          MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit
   use mapl_FileIO_mod, only: WRITE_PARALLEL
   use mapl_SimpleBundleMod_impl_mod, only: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint, &
        MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle
   use mapl_FileIOShared_mod, only: ArrDescr, ArrDescrInit, ArrDescrSet
   use mapl_NCIO_mod, only: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType, &
                        MAPL_IOGetNonDimVars, MAPL_IOCountNonDimVars, &
                        MAPL_IOChangeRes, MAPL_IOCountLevels
   implicit none(type,external)
   private

   ! StrTemplate moved to mapl_mp_utils_export
   public :: MAPL_MemUtilsInit, MAPL_MemUtilsDisable
   public :: MAPL_MemUtilsWrite, MAPL_MemUtilsIsDisabled, MAPL_MemUtilsFree
   public :: MAPL_MemCommited, MAPL_MemUsed, MAPL_MemReport
   public :: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig
   public :: MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery
   public :: MAPL_SunGetInsolation, MAPL_SunGetSolarConstant
   public :: MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax
   public :: MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit
   public :: WRITE_PARALLEL
   public :: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint
   public :: MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle
   public :: ArrDescr, ArrDescrInit, ArrDescrSet
   public :: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType
   public :: MAPL_IOGetNonDimVars, MAPL_IOCountNonDimVars
   public :: MAPL_IOChangeRes, MAPL_IOCountLevels

   public :: FileMetaDataUtils

end module mapl_base_api
