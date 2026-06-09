! Public Export umbrella for the MAPL.mp_utils layer.
! Re-exports only the symbols that are part of MAPL's public API
! (i.e., entities carrying the MAPL_ prefix).
! This is what mapl/MAPL.F90 imports from.
module mapl_mp_utils_api
   use mapl_ArrayReductions_mod, only: MAPL_AreaMean => AreaMean
   use mapl_ArrayReductions_mod, only: MAPL_MaxMin => MaxMin
   ! Import all PackedTime functions (both prefixed and unprefixed)
   use mapl_PackedTime_mod,      only: MAPL_PackedDateCreate => PackedDateCreate, &
                                       MAPL_PackedTimeCreate => PackedTimeCreate, &
                                       MAPL_PackedDateTimeCreate => PackedDateTimeCreate, &
                                       MAPL_ESMFTimeFromPacked => ESMFTimeFromPacked, &
                                       MAPL_UnpackDate => UnpackDate, &
                                       MAPL_UnpackTime => UnpackTime, &
                                       MAPL_UnpackDateTime => UnpackDateTime, &
                                       PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate, &
                                       ESMFTimeFromPacked, UnpackDate, UnpackTime, UnpackDateTime
   use mapl_StringTemplate_mod,  only: StrTemplate, fill_grads_template, fill_grads_template_esmf
   use mapl_Shmem_mod, only: MAPL_GetNodeInfo => GetNodeInfo, MAPL_CoresPerNodeGet => CoresPerNodeGet, MAPL_InitializeShmem => InitializeShmem, MAPL_FinalizeShmem => FinalizeShmem, MAPL_AllocNodeArray => AllocNodeArray, MAPL_DeAllocNodeArray => DeAllocNodeArray, MAPL_ShmemAmOnFirstNode => ShmemAmOnFirstNode, MAPL_SyncSharedMemory => SyncSharedMemory, MAPL_BroadcastToNodes => BroadcastToNodes, MAPL_AllocateShared => AllocateShared, GetSharedMemory, ReleaseSharedMemory, MAPL_GetNewRank => GetNewRank, MAPL_NodeComm, MAPL_NodeRootsComm, MAPL_MyNodeNum, MAPL_AmNodeRoot, MAPL_ShmInitialized
   use mapl_LoadBalance_mod, only: MAPL_BalanceWork => BalanceWork, &
                                   MAPL_BalanceCreate => BalanceCreate, &
                                   MAPL_BalanceDestroy => BalanceDestroy, &
                                   MAPL_BalanceGet => BalanceGet
   implicit none
   private

   ! Statistics
   public :: MAPL_MaxMin
   public :: MAPL_AreaMean

   ! Time packing
   public :: MAPL_UnpackTime
   public :: MAPL_UnpackDate
   public :: MAPL_UnpackDateTime

   ! PackedTime functions with MAPL_ prefix
   public :: MAPL_PackedDateCreate
   public :: MAPL_PackedTimeCreate
   public :: MAPL_PackedDateTimeCreate
   public :: MAPL_ESMFTimeFromPacked

   ! Backward compatibility: Unprefixed names (TODO: remove after client repos migrated)
   ! See issue #5011 - these should be removed once GEOSgcm and other clients updated
   public :: PackedDateCreate
   public :: PackedTimeCreate
   public :: PackedDateTimeCreate
   public :: ESMFTimeFromPacked
   public :: UnpackDate
   public :: UnpackTime
   public :: UnpackDateTime
   public :: StrTemplate

   public :: fill_grads_template
   public :: fill_grads_template_esmf
   
   public :: MAPL_GetNodeInfo
   public :: MAPL_CoresPerNodeGet
   public :: MAPL_InitializeShmem
   public :: MAPL_FinalizeShmem
   
   public :: MAPL_AllocNodeArray
   public :: MAPL_DeAllocNodeArray
   public :: MAPL_ShmemAmOnFirstNode
   public :: MAPL_SyncSharedMemory
   public :: MAPL_BroadcastToNodes
   
   public :: MAPL_AllocateShared
   public :: GetSharedMemory
   public :: ReleaseSharedMemory

   public :: MAPL_GetNewRank
   public :: MAPL_NodeComm
   public :: MAPL_NodeRootsComm
   public :: MAPL_MyNodeNum
   public :: MAPL_AmNodeRoot
   public :: MAPL_ShmInitialized

   public MAPL_BalanceWork
   public MAPL_BalanceCreate
   public MAPL_BalanceDestroy
   public MAPL_BalanceGet

end module mapl_mp_utils_api
