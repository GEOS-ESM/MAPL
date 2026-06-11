module mapl_base_api
   use mapl_FileMetadataUtils_mod
   use mapl_FileMetadataUtilsVector_mod
   ! StringTemplate is in mp_utils/ - should be exported from mapl_mp_utils_export
   use mapl_MemUtils_mod, only: mapl_MemUtilsInit => MemUtilsInit
   use mapl_MemUtils_mod, only: mapl_MemUtilsDisable => MemUtilsDisable
   use mapl_MemUtils_mod, only: mapl_MemUtilsWrite => MemUtilsWrite
   use mapl_MemUtils_mod, only: mapl_MemUtilsIsDisabled => MemUtilsIsDisabled
   use mapl_MemUtils_mod, only: mapl_MemUtilsFree => MemUtilsFree
   use mapl_MemUtils_mod, only: mapl_MemCommited => MemCommited
   use mapl_MemUtils_mod, only: mapl_MemUsed => MemUsed
   use mapl_MemUtils_mod, only: mapl_MemReport => MemReport
   use mapl_MemUtils_mod, only: mapl_MemUtilsModeNode => MemUtilsModeNode
   use mapl_MemUtils_mod, only: mapl_MemUtilsModeFull => MemUtilsModeFull
   use mapl_MemUtils_mod, only: mapl_MemUtilsModeBase => MemUtilsModeBase
   use mapl_Sun_mod, only: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig, &
         MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery, &
         MAPL_SunGetInsolation, MAPL_SunGetSolarConstant, &
          MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax, &
          MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit
   use mapl_FileIO_mod, only: WRITE_PARALLEL
   use mapl_SimpleBundleMod_impl_mod, only: mapl_SimpleBundleCreate => SimpleBundleCreate
   use mapl_SimpleBundleMod_impl_mod, only: mapl_SimpleBundlePrint => SimpleBundlePrint
   use mapl_SimpleBundleMod_impl_mod, only: mapl_SimpleBundleGetIndex => SimpleBundleGetIndex
   use mapl_SimpleBundleMod_impl_mod, only: mapl_SimpleBundleDestroy => SimpleBundleDestroy
   use mapl_SimpleBundleMod_impl_mod, only:  mapl_SimpleBundle => SimpleBundle

   use mapl_FileIOShared_mod, only: ArrDescr, ArrDescrInit, ArrDescrSet
   use mapl_FileIOShared_mod, only: mapl_TileMaskGet => TileMaskGet
   use mapl_NCIO_mod, only: mapl_VarRead => VarRead
   use mapl_NCIO_mod, only: mapl_VarWrite => VarWrite
   use mapl_NCIO_mod, only: mapl_NCIOGetFileType => NCIOGetFileType
   use mapl_NCIO_mod, only: mapl_IOGetNonDimVars => IOGetNonDimVars
   use mapl_NCIO_mod, only: mapl_IOCountNonDimVars => IOCountNonDimVars 
   use mapl_NCIO_mod, only: mapl_IOChangeRes => IOChangeRes
   use mapl_NCIO_mod, only: mapl_IOCountLevels => IOCountLevels
   use mapl_locstreammod, only: mapl_LocStreamCreate => LocStreamCreate
   use mapl_locstreammod, only: mapl_LocStreamAdjustNsubtiles => LocStreamAdjustNsubtiles
   use mapl_locstreammod, only: mapl_LocStreamTransform => LocStreamTransform
   use mapl_locstreammod, only: mapl_LocStreamIsAssociated => LocStreamIsAssociated
   use mapl_locstreammod, only: mapl_LocStreamXformIsAssociated => LocStreamXformIsAssociated
   use mapl_locstreammod, only: mapl_LocStreamGet => LocStreamGet
   use mapl_locstreammod, only: mapl_LocStreamCreateXform => LocStreamCreateXform
   use mapl_locstreammod, only: mapl_LocStreamFracArea => LocStreamFracArea
   use mapl_locstreammod, only: mapl_GridCoordAdjust => GridCoordAdjust
   use mapl_locstreammod, only: mapl_LocStreamTileWeight => LocStreamTileWeight



   implicit none(type,external)
   private

   ! StrTemplate moved to mapl_mp_utils_export
   public :: mapl_MemUtilsInit, mapl_MemUtilsDisable
   public :: mapl_MemUtilsWrite, mapl_MemUtilsIsDisabled, mapl_MemUtilsFree
   public :: mapl_MemCommited, mapl_MemUsed, mapl_MemReport
   public :: mapl_SunOrbitCreate, mapl_SunOrbitCreateFromConfig
   public :: mapl_SunOrbitCreated, mapl_SunOrbitDestroy, mapl_SunOrbitQuery
   public :: mapl_SunGetInsolation, mapl_SunGetSolarConstant
   public :: mapl_SunGetDaylightDuration, mapl_SunGetDaylightDurationMax
   public :: mapl_SunGetLocalSolarHourAngle, mapl_SunOrbit
   public :: WRITE_PARALLEL
   public :: mapl_SimpleBundleCreate, mapl_SimpleBundlePrint
   public :: mapl_SimpleBundleGetIndex, mapl_SimpleBundleDestroy, mapl_SimpleBundle
   public :: ArrDescr, ArrDescrInit, ArrDescrSet
   public :: mapl_VarRead, mapl_VarWrite, mapl_NCIOGetFileType
   public :: mapl_IOGetNonDimVars, mapl_IOCountNonDimVars
   public :: mapl_IOChangeRes, mapl_IOCountLevels

   public :: FileMetaDataUtils

end module mapl_base_api
