! Export umbrella for the MAPL infrastructure/esmf layer.
! Public API of esmf/ leaf modules exposed to external consumers.
module mapl_esmf_api

   ! Alarm
   use mapl_SimpleAlarm_mod, only: MAPL_SimpleAlarm => SimpleAlarm

   ! Core ESMF utilities
   use mapl_ESMF_Time_Utilities_mod, only: MAPL_SubTimeInDateTime => sub_time_in_datetime

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

   ! HConfig
   use mapl_hconfig_get_mod, only: MAPL_HConfigGet => HConfigGet
   use mapl_ESMF_HConfigUtilities_mod, only: MAPL_HConfigMatch => HConfigMatch
   use mapl_HConfigAs_mod, only: mapl_HConfigAsItemType => HConfigAsItemType
   use mapl_HConfigAs_mod, only: mapl_HConfigAsStateIntent => HConfigAsStateIntent
   use mapl_HConfigAs_mod, only: mapl_HConfigAsTime => HConfigAsTime
   use mapl_HConfigAs_mod, only: mapl_HConfigAsTimeInterval => HConfigAsTimeInterval
   use mapl_HConfigAs_mod, only: mapl_HConfigAsTimeRange => HConfigAsTimeRange
   use mapl_HConfigAs_mod, only: mapl_HConfigAsStringVector => HConfigAsStringVector

   ! Info / metadata utilities
   ! NOTE: MAPL_Info* from mapl_InfoUtilities_mod are widely used across the
   ! MAPL codebase, so we are not removing the prefixes there
   use mapl_InfoUtilities_mod, only: MAPL_InfoSet, MAPL_InfoGet
   use mapl_InfoUtilities_mod, only: MAPL_InfoCreateFromShared
   use mapl_InfoUtilities_mod, only: MAPL_InfoSetShared, MAPL_InfoGetShared
   use mapl_InfoUtilities_mod, only: MAPL_InfoSetPrivate, MAPL_InfoGetPrivate
   use mapl_InfoUtilities_mod, only: MAPL_InfoSetNamespace

   ! Ungridded dimensions
   use mapl_UngriddedDim_mod, only: mapl_UngriddedDim => UngriddedDim
   use mapl_UngriddedDim_mod, only: mapl_make_UngriddedDim => make_UngriddedDim
   use mapl_UngriddedDims_mod, only: mapl_UngriddedDims => UngriddedDims

   ! Bounds / grid utilities
   use mapl_LU_Bound_mod
   use mapl_HorizontalDimsSpec_mod
   use mapl_DistGridGet_mod

   ! Field utilities
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldGetCPtr => FieldGetCPtr
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldCopy => FieldCopy
   use mapl_FieldPointerUtilities_mod, only: MAPL_AssignFptr => assign_fptr
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldGetLocalElementCount => FieldGetLocalElementCount
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldClone => FieldClone


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

   ! State item constants
   use mapl_StateItem_mod, only: MAPL_STATEITEM_UNKNOWN
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE, MAPL_STATEITEM_STATE
   use mapl_StateItem_mod, only: MAPL_STATEITEM_SERVICE
   use mapl_StateItem_mod, only: MAPL_STATEITEM_SERVICE_PROVIDER, MAPL_STATEITEM_SERVICE_SUBSCRIBER
   use mapl_StateItem_mod, only: MAPL_STATEITEM_WILDCARD, MAPL_STATEITEM_BRACKET
   use mapl_StateItem_mod, only: MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_VECTORBRACKET
   use mapl_StateItem_mod, only: MAPL_STATEITEM_EXPRESSION

   ! Type kinds
   use mapl_typekind_mod, only: MAPL_TYPEKIND_MIRROR

   implicit none
   private

   ! Alarm
   public :: MAPL_SimpleAlarm

   ! Core ESMF utilities
   public :: MAPL_SubTimeInDateTime

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

   ! User comp internal state

   ! HConfig
   public :: MAPL_HConfigMatch
   public :: MAPL_HConfigGet
   public :: mapl_HConfigAsItemType
   public :: mapl_HConfigAsStateIntent
   public :: mapl_HConfigAsTime
   public :: mapl_HConfigAsTimeInterval
   public :: mapl_HConfigAsTimeRange
   public :: mapl_HConfigAsStringVector

   ! Info / metadata utilities
   public :: MAPL_InfoSet
   public :: MAPL_InfoGet
   public :: MAPL_InfoCreateFromShared
   public :: MAPL_InfoSetShared
   public :: MAPL_InfoGetShared
   public :: MAPL_InfoSetPrivate
   public :: MAPL_InfoGetPrivate
   public :: MAPL_InfoSetNamespace


   ! Field utilities
   public :: MAPL_FieldGetCPtr
   public :: MAPL_FieldCopy
   public :: MAPL_AssignFptr
   public :: MAPL_FieldGetLocalElementCount
   public :: MAPL_FieldClone

   ! State item constants
   public :: mapl_UngriddedDim
   public :: mapl_make_UngriddedDim
   public :: mapl_UngriddedDims

   ! State item constants

   ! TYPEKIND
   public :: MAPL_TYPEKIND_MIRROR

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

end module mapl_esmf_api
