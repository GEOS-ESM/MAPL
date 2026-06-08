! Export umbrella for the MAPL infrastructure/esmf layer.
! Public API of esmf/ leaf modules exposed to external consumers.
module mapl_esmf_api

   use mapl_esmf_internal

   use mapl_Comms_mod, only: arraygather => array_gather
   use mapl_Comms_mod, only: arrayscatter => array_scatter
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
