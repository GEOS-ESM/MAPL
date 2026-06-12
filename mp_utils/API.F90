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
                                       MAPL_UnpackDateTime => UnpackDateTime

   use mapl_StringTemplate_mod,  only: mapl_StrTemplate => StrTemplate
   use mapl_StringTemplate_mod,  only: mapl_fill_grads_template => fill_grads_template
   use mapl_StringTemplate_mod,  only: mapl_fill_grads_template_esmf => fill_grads_template_esmf

   use mapl_Shmem_mod, only: MAPL_GetNodeInfo => GetNodeInfo
   use mapl_Shmem_mod, only: MAPL_CoresPerNodeGet => CoresPerNodeGet
   use mapl_Shmem_mod, only: MAPL_InitializeShmem => InitializeShmem
   use mapl_Shmem_mod, only: MAPL_FinalizeShmem => FinalizeShmem
   use mapl_Shmem_mod, only: MAPL_AllocNodeArray => AllocNodeArray
   use mapl_Shmem_mod, only: MAPL_DeAllocNodeArray => DeAllocNodeArray
   use mapl_Shmem_mod, only: MAPL_ShmemAmOnFirstNode => ShmemAmOnFirstNode
   use mapl_Shmem_mod, only: MAPL_SyncSharedMemory => SyncSharedMemory
   use mapl_Shmem_mod, only: MAPL_BroadcastToNodes => BroadcastToNodes
   use mapl_Shmem_mod, only: MAPL_AllocateShared => AllocateShared
   use mapl_Shmem_mod, only: mapl_GetSharedMemory => GetSharedMemory
   use mapl_Shmem_mod, only: mapl_ReleaseSharedMemory => ReleaseSharedMemory
   use mapl_Shmem_mod, only: MAPL_GetNewRank => GetNewRank
   use mapl_Shmem_mod, only: MAPL_NodeComm
   use mapl_Shmem_mod, only: MAPL_NodeRootsComm
   use mapl_Shmem_mod, only: MAPL_MyNodeNum
   use mapl_Shmem_mod, only: MAPL_AmNodeRoot
   use mapl_Shmem_mod, only: MAPL_ShmInitialized
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

   public :: mapl_StrTemplate

   public :: mapl_fill_grads_template
   public :: mapl_fill_grads_template_esmf
   
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
   public :: MAPL_GetSharedMemory
   public :: MAPL_ReleaseSharedMemory

   public :: MAPL_GetNewRank
   public :: MAPL_NodeComm
   public :: MAPL_NodeRootsComm
   public :: MAPL_MyNodeNum
   public :: MAPL_AmNodeRoot
   public :: MAPL_ShmInitialized

   public :: MAPL_BalanceWork
   public :: MAPL_BalanceCreate
   public :: MAPL_BalanceDestroy
   public :: MAPL_BalanceGet

end module mapl_mp_utils_api
