! Export umbrella for the MAPL infrastructure/esmf layer.
! Public API of esmf/ leaf modules exposed to external consumers.
module mapl_esmf_api

   ! Alarm
   use mapl_SimpleAlarm_mod, only: MAPL_SimpleAlarm => SimpleAlarm

   ! Core ESMF utilities
   ! use mapl_ESMF_Utilities_mod
   use mapl_ESMF_Time_Utilities_mod
   use mapl_ESMF_HConfigUtilities_mod

   ! Comms
   use mapl_comms_mod, only: MAPL_ROOT => ROOT_PROCESS_ID
   use mapl_comms_mod, only: MAPL_Barrier => barrier
   use mapl_comms_mod, only: MAPL_Am_I_Root => am_i_root
   use mapl_comms_mod, only: MAPL_Am_I_Rank => am_i_rank
   use mapl_comms_mod, only: MAPL_NPES => num_pes
   use mapl_comms_mod, only: MAPL_CommsSend => comms_send, MAPL_CommsRecv => comms_recv
   use mapl_comms_mod, only: MAPL_CommsSendRecv => comms_sendrecv
   use mapl_comms_mod, only: MAPL_CommsGatherV => comms_gatherv
   use mapl_comms_mod, only: MAPL_CommsScatterV => comms_scatterv
   use mapl_comms_mod, only: MAPL_CommsAllGather => comms_allgather
   use mapl_comms_mod, only: MAPL_CommsAllGatherV => comms_allgatherv
   use mapl_comms_mod, only: MAPL_ArrayGather => array_gather
   use mapl_comms_mod, only: MAPL_ArrayScatter => array_scatter
   use mapl_comms_mod, only: ArrayGather => array_gather   ! TODO: pchakrab - remove after updating GEOS
   use mapl_comms_mod, only: ArrayScatter => array_scatter ! -do-
   use mapl_comms_mod, only: MAPL_CommsAllReduceMin => comms_allreduce_min
   use mapl_comms_mod, only: MAPL_CommsAllReduceMax => comms_allreduce_max
   use mapl_comms_mod, only: MAPL_CommsAllReduceSum => comms_allreduce_sum

   ! ShmemComms
   use mapl_ShmemComms_mod, only: MAPL_RoundRobinPEList => RoundRobinPEList
   use mapl_ShmemComms_mod, only: MAPL_BcastShared => BcastShared
   use mapl_ShmemComms_mod, only: MAPL_CommsBcast => CommsBcast
   use mapl_ShmemComms_mod, only: MAPL_CommRequest => CommRequest
   use mapl_ShmemComms_mod, only: MAPL_CreateRequest => CreateRequest
   use mapl_ShmemComms_mod, only: MAPL_ArrayIGather => ArrayIGather
   use mapl_ShmemComms_mod, only: MAPL_ArrayIScatter => ArrayIScatter
   use mapl_ShmemComms_mod, only: MAPL_CollectiveWait => CollectiveWait

   ! Info / metadata utilities
   use mapl_InfoUtilities_mod

   ! Ungridded dimensions
   use mapl_UngriddedDim_mod
   use mapl_UngriddedDims_mod
   use mapl_UngriddedDimVector_mod

   ! Bounds / grid utilities
   use mapl_LU_Bound_mod
   use mapl_HorizontalDimsSpec_mod
   use mapl_DistGridGet_mod
   use mapl_FieldPointerUtilities_mod

   ! HConfig
   use mapl_HConfigAs_mod, only: &
        mapl_HConfigAsItemType => HConfigAsItemType, &
        mapl_HConfigAsStateIntent => HConfigAsStateIntent, &
        mapl_HConfigAsTime => HConfigAsTime, &
        mapl_HConfigAsTimeInterval => HConfigAsTimeInterval, &
        mapl_HConfigAsTimeRange => HConfigAsTimeRange, &
        mapl_HConfigAsStringVector => HConfigAsStringVector
   use mapl_HConfigAs_mod
   use mapl_HConfigUtilities_mod
   use mapl_get_hconfig_mod
   use mapl_hconfig_get_mod
   use mapl_hconfig_params_mod
   use mapl_generalized_equality_mod


   ! State item
   use mapl_StateItem_mod


   use mapl_FieldPointerUtilities_mod, only: MAPL_AssignFptr => assign_fptr
   use mapl_FieldPointerUtilities_mod, only: FieldGetLocalElementCount
   use mapl_FieldPointerUtilities_mod, only: mapl_FieldClone => FieldClone


   implicit none
   private

   ! Alarm
   public :: MAPL_SimpleAlarm

   ! ! Core ESMF utilities - user comp internal state
   ! public :: MAPL_UserCompGetInternalState
   ! public :: MAPL_UserCompSetInternalState

   ! Comms
   public :: MAPL_ROOT
   public :: MAPL_NPES
   public :: MAPL_Barrier
   public :: MAPL_Am_I_Root
   public :: MAPL_Am_I_Rank
   public :: MAPL_CommsSend
   public :: MAPL_CommsRecv
   public :: MAPL_CommsSendRecv
   public :: MAPL_CommsGatherV
   public :: MAPL_CommsScatterV
   public :: MAPL_CommsAllGather
   public :: MAPL_CommsAllGatherV
   public :: MAPL_ArrayGather
   public :: MAPL_ArrayScatter
   public :: ArrayGather  ! TODO: pchakrab - remove after updating GEOS
   public :: ArrayScatter ! -do-
   public :: MAPL_CommsAllReduceMin
   public :: MAPL_CommsAllReduceMax
   public :: MAPL_CommsAllReduceSum

   ! ShmemComms
   public :: MAPL_RoundRobinPEList
   public :: MAPL_BcastShared
   public :: MAPL_CommsBcast
   public :: MAPL_CommRequest
   public :: MAPL_CreateRequest
   public :: MAPL_CollectiveWait
   public :: MAPL_ArrayIGather
   public :: MAPL_ArrayIScatter

   ! HConfig
   public :: MAPL_HConfigGet
   public :: MAPL_HConfigMatch
   public :: mapl_HConfigAsItemType
   public :: mapl_HConfigAsStateIntent
   public :: mapl_HConfigAsTime
   public :: mapl_HConfigAsTimeInterval
   public :: mapl_HConfigAsTimeRange
   public :: mapl_HConfigAsStringVector

   ! Info / metadata

   ! Field utilities

   ! Ungridded dims
   public :: UngriddedDim
   public :: make_UngriddedDim
   public :: UngriddedDims

   ! State item constants

   ! TYPEKIND

   public :: sub_time_in_datetime
   public :: FieldGetCPtr
   public :: FieldCopy

   public :: MAPL_STATEITEM_UNKNOWN
   public :: MAPL_STATEITEM_FIELD
   public :: MAPL_STATEITEM_FIELDBUNDLE
   public :: MAPL_STATEITEM_STATE
   public :: MAPL_STATEITEM_SERVICE
   public :: MAPL_STATEITEM_SERVICE_PROVIDER
   public :: MAPL_STATEITEM_SERVICE_SUBSCRIBER
   public :: MAPL_STATEITEM_WILDCARD
   public :: MAPL_STATEITEM_BRACKET
   public :: MAPL_STATEITEM_VECTOR
   public :: MAPL_STATEITEM_VECTORBRACKET
   public :: MAPL_STATEITEM_EXPRESSION

   public :: mapl_AssignFptr
   public :: FieldGetLocalElementCount
   public :: mapl_FieldClone

end module mapl_esmf_api
