! Export umbrella for the MAPL infrastructure/esmf layer.
! Public API of esmf/ leaf modules exposed to external consumers.
module mapl_esmf_api

   ! Core ESMF utilities
   use mapl_ESMF_Interfaces_mod
   use mapl_ESMF_Utilities_mod
   use mapl_ESMF_Time_Utilities_mod
   use mapl_ESMF_Subset_mod
   use mapl_ESMF_HConfigUtilities_mod

   ! Info / metadata utilities
   use mapl_InfoUtilities_mod

   ! Ungridded dimensions
   use mapl_UngriddedDim_mod
   use mapl_UngriddedDims_mod
   use mapl_UngriddedDimVector_mod

   ! VM
   use mapl_vm_mod

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

   ! Alarm
   use mapl_SimpleAlarm_mod

   ! State item
   use mapl_StateItem_mod

   ! Comms
   use mapl_Comms_mod
   use mapl_Comms_mod, only: &
        MAPL_Am_I_Root => am_i_root, &
        MAPL_Am_I_Rank => am_i_rank, &
        MAPL_NPES => num_pes, &
        MAPL_CommsSend => comms_send, &
        MAPL_CommsRecv => comms_recv, &
        MAPL_CommsSendRecv => comms_sendrecv, &
        MAPL_CommsGatherV => comms_gatherv, &
        MAPL_CommsScatterV => comms_scatterv, &
        MAPL_CommsAllGather => comms_allgather, &
        MAPL_CommsAllGatherV => comms_allgatherv, &
        MAPL_ArrayGather => array_gather, &
        MAPL_ArrayScatter => array_scatter, &
        MAPL_CommsAllReduceMin => comms_allreduce_min, &
        MAPL_CommsAllReduceMax => comms_allreduce_max, &
        MAPL_CommsAllReduceSum => comms_allreduce_sum
   use mapl_ShmemComms_mod

   use mapl_Comms_mod, only: ArrayGather => array_gather
   use mapl_Comms_mod, only: ArrayScatter => array_scatter
   use mapl_FieldPointerUtilities_mod, only: MAPL_AssignFptr => assign_fptr
   use mapl_FieldPointerUtilities_mod, only: FieldGetLocalElementCount
   use mapl_FieldPointerUtilities_mod, only: mapl_FieldClone => FieldClone


   implicit none
   private

   ! VM / comm utilities
   public :: MAPL_ROOT
   public :: mapl_AmIPet
   public :: mapl_AmIRoot
   public :: mapl_Barrier
   public :: mapl_RoundRobinPEList
   public :: mapl_BcastShared
   public :: mapl_CommsBcast
   public :: MAPL_Am_I_Root
   public :: MAPL_Am_I_Rank
   public :: MAPL_NPES
   public :: ROOT_PROCESS_ID
   public :: MAPL_CommsSend
   public :: MAPL_CommsRecv
   public :: MAPL_CommsSendRecv
   public :: MAPL_CommsGatherV
   public :: MAPL_CommsScatterV
   public :: MAPL_CommsAllGather
   public :: MAPL_CommsAllGatherV
   public :: MAPL_ArrayGather
   public :: MAPL_ArrayScatter
   public :: ArrayGather
   public :: ArrayScatter
   public :: MAPL_CommsAllReduceMin
   public :: MAPL_CommsAllReduceMax
   public :: MAPL_CommsAllReduceSum

   ! User comp internal state

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


   public :: SimpleAlarm

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
