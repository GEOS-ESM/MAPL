! Export umbrella for the MAPL base layer.
! Public API of base/ leaf modules. Cross-layer symbols (comms, mp_utils, etc.)
! are exposed via mapl_base_mod (API.F90) which remains the top-level umbrella.
module mapl_base_export

   use mapl_base_internal

   implicit none
   private

   ! FileIO
   ! (WRITE_PARALLEL exported via mapl_base_mod to avoid conflict)

   ! FileIOShared
   public :: ArrDescr, ArrDescrInit, ArrDescrSet

   ! FileMetadata
   ! (mapl_FileMetadataUtils_mod and mapl_FileMetadataUtilsVector_mod are
   !  used without only: in API.F90 — all their public symbols flow through)

   ! NCIO
   public :: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType
   public :: MAPL_IOGetNonDimVars, MAPL_IOCountNonDimVars
   public :: MAPL_IOChangeRes, MAPL_IOCountLevels

   ! MemUtils
   public :: MAPL_MemUtilsInit, MAPL_MemUtilsDisable
   public :: MAPL_MemUtilsWrite, MAPL_MemUtilsIsDisabled, MAPL_MemUtilsFree
   public :: MAPL_MemCommited, MAPL_MemUsed, MAPL_MemReport

   ! Sun / SunOrbit
   public :: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig
   public :: MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery
   public :: MAPL_SunGetInsolation, MAPL_SunGetSolarConstant
   public :: MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax
   public :: MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit

   ! SimpleBundle
   public :: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint
   public :: MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle

end module mapl_base_export
