module mapl_base3g_mod
   use mapl_FileMetadataUtils_mod
   use mapl_FileMetadataUtilsVector_mod
   use mapl_PackedTime_mod, only: MAPL_PackedDateCreate => PackedDateCreate, &
                                   MAPL_PackedTimeCreate => PackedTimeCreate, &
                                   MAPL_PackedDateTimeCreate => PackedDateTimeCreate, &
                                   MAPL_ESMFTimeFromPacked => ESMFTimeFromPacked, &
                                   MAPL_UnpackDate => UnpackDate, &
                                   MAPL_UnpackTime => UnpackTime, &
                                   MAPL_UnpackDateTime => UnpackDateTime
   use mapl_SimulationTime_mod, only: set_reference_clock, fill_time_dict
   use mapl_Comms_mod, only: mapl_CommsScatterV => comms_scatterv, &
                             mapl_CommsGatherV => comms_gatherv, &
                             mapl_CommsAllGather => comms_allgather, &
                             mapl_CommsAllGatherV => comms_allgatherv, &
                             mapl_CommsAllReduceMin => comms_allreduce_min, &
                             mapl_CommsAllReduceMax => comms_allreduce_max, &
                             mapl_CommsAllReduceSum => comms_allreduce_sum, &
                             mapl_CommsSend => comms_send, &
                             mapl_CommsRecv => comms_recv, &
                             mapl_CommsSendRecv => comms_sendrecv, &
                             mapl_AM_I_ROOT => am_i_root, &
                             mapl_AM_I_RANK => am_i_rank, &
                             mapl_NPES => num_pes, &
                             ArrayGather => array_gather, &
                             ArrayScatter => array_scatter, &
                             MAPL_ArrayGather => array_gather, &
                             MAPL_ArrayScatter => array_scatter, &
                             MAPL_ROOT => ROOT_PROCESS_ID
   use mapl_ShmemComms_mod, only: mapl_CommsBcast, mapl_RoundRobinPEList, mapl_BcastShared
   use mapl_SatVapor_mod, only: MAPL_EQsatSET, MAPL_EQsat
   use mapl_StringTemplate_mod, only: fill_grads_template, StrTemplate, fill_grads_template_esmf
   use mapl_LocalDisplacementEnsemble_mod, only: LocalDisplacementEnsemble
   use mapl_MemUtils_mod, only: MAPL_MemUtilsInit, MAPL_MemUtilsDisable, &
         MAPL_MemUtilsWrite, MAPL_MemUtilsIsDisabled, MAPL_MemUtilsFree, &
         MAPL_MemCommited, MAPL_MemUsed, MAPL_MemReport
   use mapl_Sun_mod, only: MAPL_SunOrbitCreate, MAPL_SunOrbitCreateFromConfig, &
         MAPL_SunOrbitCreated, MAPL_SunOrbitDestroy, MAPL_SunOrbitQuery, &
         MAPL_SunGetInsolation, MAPL_SunGetSolarConstant, &
          MAPL_SunGetDaylightDuration, MAPL_SunGetDaylightDurationMax, &
          MAPL_SunGetLocalSolarHourAngle, MAPL_SunOrbit
   use mapl_TimeInterpolation_mod, only: MAPL_Interp_Fac, MAPL_ClimInterpFac
   use mapl_FileIO_mod, only: WRITE_PARALLEL
   use mapl_SimpleBundleMod_impl_mod, only: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint, &
        MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle
   use mapl_FileIOShared_mod, only: ArrDescr, ArrDescrInit, ArrDescrSet
   use mapl_NCIO_mod, only: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType, &
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
   public :: mapl_RoundRobinPEList, mapl_BcastShared
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
   public :: WRITE_PARALLEL
   public :: MAPL_SimpleBundleCreate, MAPL_SimpleBundlePrint
   public :: MAPL_SimpleBundleGetIndex, MAPL_SimpleBundleDestroy, MAPL_SimpleBundle
   public :: ArrDescr, ArrDescrInit, ArrDescrSet
   public :: MAPL_VarRead, MAPL_VarWrite, MAPL_NCIOGetFileType
   public :: MAPL_IOGetNonDimVars, MAPL_IOCountNonDimVars
   public :: MAPL_IOChangeRes, MAPL_IOCountLevels

end module mapl_base3g_mod
