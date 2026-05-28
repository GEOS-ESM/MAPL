! Export umbrella for the MAPL infrastructure/esmf layer.
! Public API of esmf/ leaf modules exposed to external consumers.
module mapl_esmf_export

   use mapl_esmf_internal

   implicit none
   private

   ! VM / comm utilities
   public :: MAPL_ROOT
   public :: mapl_AmIPet
   public :: mapl_AmIRoot
   public :: mapl_Barrier
   public :: mapl_RoundRobinPEList
   public :: mapl_BcastShared
   public :: mapl_CollectiveWait
   public :: mapl_CommRequest
   public :: mapl_CommsBcast
   public :: mapl_CreateRequest
   public :: mapl_ArrayIGather
   public :: mapl_ArrayIScatter

   ! User comp internal state
   public :: MAPL_UserCompGetInternalState
   public :: MAPL_UserCompSetInternalState

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
   public :: MAPL_InfoCreateFromShared
   public :: MAPL_InfoGet
   public :: MAPL_InfoGetPrivate
   public :: MAPL_InfoGetShared
   public :: MAPL_InfoSet
   public :: MAPL_InfoSetNamespace
   public :: MAPL_InfoSetPrivate
   public :: MAPL_InfoSetShared

   ! Field utilities
   public :: MAPL_FieldDestroy

   ! Ungridded dims
   public :: UngriddedDim
   public :: make_UngriddedDim
   public :: UngriddedDims

   ! State item constants
   public :: MAPL_STATEITEM_BRACKET
   public :: MAPL_STATEITEM_EXPRESSION
   public :: MAPL_STATEITEM_FIELD
   public :: MAPL_STATEITEM_FIELDBUNDLE
   public :: MAPL_STATEITEM_SERVICE
   public :: MAPL_STATEITEM_SERVICE_PROVIDER
   public :: MAPL_STATEITEM_SERVICE_SUBSCRIBER
   public :: MAPL_STATEITEM_STATE
   public :: MAPL_STATEITEM_UNKNOWN
   public :: MAPL_STATEITEM_VECTOR
   public :: MAPL_STATEITEM_VECTORBRACKET
   public :: MAPL_STATEITEM_WILDCARD

   ! TYPEKIND
   public :: MAPL_TYPEKIND_MIRROR

end module mapl_esmf_export
