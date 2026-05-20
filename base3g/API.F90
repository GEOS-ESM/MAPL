module mapl_base3g
   use MAPL_FileMetadataUtilsMod
   use MAPL_FileMetadataUtilsVectorMod
   use MAPL_PackedTimeMod, only: MAPL_PackedDateCreate => PackedDateCreate, &
                                   MAPL_PackedTimeCreate => PackedTimeCreate, &
                                   MAPL_PackedDateTimeCreate => PackedDateTimeCreate, &
                                   MAPL_ESMFTimeFromPacked => ESMFTimeFromPacked, &
                                   MAPL_UnpackDate => UnpackDate, &
                                   MAPL_UnpackTime => UnpackTime, &
                                   MAPL_UnpackDateTime => UnpackDateTime
   use mapl_SimulationTime, only: set_reference_clock, fill_time_dict
   use MAPL_CommsMod, only: mapl_CommsBcast, mapl_CommsScatterV, mapl_CommsGatherV, &
                             mapl_CommsAllGather, mapl_CommsAllGatherV, &
                             mapl_CommsAllReduceMin, mapl_CommsAllReduceMax, &
                             mapl_CommsAllReduceSum, mapl_CommsSend, mapl_CommsRecv, &
                             mapl_CommsSendRecv, mapl_AM_I_ROOT, mapl_AM_I_RANK, &
                             mapl_NPES, ArrayGather, ArrayScatter, MAPL_ROOT, &
                             MAPL_ArrayGather => ArrayGather, MAPL_ArrayScatter => ArrayScatter, &
                             mapl_CreateRequest, mapl_CommRequest, mapl_ArrayIGather, &
                             mapl_ArrayIScatter, mapl_CollectiveWait, &
                             mapl_CollectiveScatter3D, mapl_CollectiveGather3D, &
                             mapl_RoundRobinPEList, mapl_BcastShared, ArrPtr
   use MAPL_SatVaporMod, only: MAPL_EQsatSET, MAPL_EQsat
   use MAPL_StringTemplate, only: fill_grads_template, StrTemplate, fill_grads_template_esmf
   use mapl_LocalDisplacementEnsemble, only: LocalDisplacementEnsemble
   use MAPL_MemUtilsMod, only: MAPL_MemUtilsInit, MAPL_MemUtilsDisable, &
         MAPL_MemUtilsWrite, MAPL_MemUtilsIsDisabled, MAPL_MemUtilsFree, &
         MAPL_MemCommited, MAPL_MemUsed, MAPL_MemReport
   use MAPL_SunMod, only: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig, &
         MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery, &
         MAPL_SunGetInsolation, MAPL_SunGetSolarConstant, &
          MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax, &
          MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit
   use MAPL_TimeInterpolation, only: MAPL_Interp_Fac, MAPL_ClimInterpFac
   use mapl3g_MaxMin, only: MAPL_MaxMin => MaxMin
   use mapl3g_AreaMean, only: MAPL_AreaMean => AreaMean
   use mapl3g_MemInfo, only: MAPL_MemInfoWrite => MemInfoWrite
   use mapl3g_FileIO, only: WRITE_PARALLEL
   use mapl_SimpleBundleMod_impl, only: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint, &
        MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle
   use mapl_FileIOShared, only: ArrDescr, ArrDescrInit, ArrDescrSet
   use mapl_NCIO, only: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType, &
                        MAPL_IOGetNonDimVars, MAPL_IOCountNonDimVars, &
                        MAPL_IOChangeRes, MAPL_IOCountLevels
   implicit none(type,external)
   private

   public :: MAPL_PackedDateCreate, MAPL_PackedTimeCreate, &
              MAPL_PackedDateTimeCreate, MAPL_ESMFTimeFromPacked, &
              MAPL_UnpackDate, MAPL_UnpackTime, MAPL_UnpackDateTime
   public :: set_reference_clock, fill_time_dict
   public :: mapl_CommsBcast, mapl_CommsScatterV, mapl_CommsGatherV
   public :: mapl_CommsAllGather, mapl_CommsAllGatherV
   public :: mapl_CommsAllReduceMin, mapl_CommsAllReduceMax, mapl_CommsAllReduceSum
   public :: mapl_CommsSend, mapl_CommsRecv, mapl_CommsSendRecv
   public :: mapl_AM_I_ROOT, mapl_AM_I_RANK, mapl_NPES
   public :: ArrayGather, ArrayScatter, MAPL_ROOT
   public :: MAPL_ArrayGather, MAPL_ArrayScatter
   public :: mapl_CreateRequest, mapl_CommRequest
   public :: mapl_ArrayIGather, mapl_ArrayIScatter, mapl_CollectiveWait
   public :: mapl_CollectiveScatter3D, mapl_CollectiveGather3D
   public :: mapl_RoundRobinPEList, mapl_BcastShared, ArrPtr
   public :: MAPL_EQsatSET, MAPL_EQsat
   public :: fill_grads_template, StrTemplate, fill_grads_template_esmf
   public :: LocalDisplacementEnsemble
   public :: MAPL_MemUtilsInit, MAPL_MemUtilsDisable
   public :: MAPL_MemUtilsWrite, MAPL_MemUtilsIsDisabled, MAPL_MemUtilsFree
   public :: MAPL_MemCommited, MAPL_MemUsed, MAPL_MemReport
   public :: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig
   public :: MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery
   public :: MAPL_SunGetInsolation, MAPL_SunGetSolarConstant
   public :: MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax
   public :: MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit
   public :: MAPL_Interp_Fac, MAPL_ClimInterpFac
   public :: MAPL_MaxMin, MAPL_AreaMean, MAPL_MemInfoWrite
   public :: WRITE_PARALLEL
   public :: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint
   public :: MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle
   public :: ArrDescr, ArrDescrInit, ArrDescrSet
   public :: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType
   public :: MAPL_IOGetNonDimVars, MAPL_IOCountNonDimVars
   public :: MAPL_IOChangeRes, MAPL_IOCountLevels

end module mapl_base3g
