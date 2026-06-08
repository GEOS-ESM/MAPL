! Internal umbrella for the MAPL.mp_utils layer.
! Exports everything needed by other MAPL subdirectories, including symbols
! that are not part of the public (Export) API.
! External users should use mapl_mp_utils_export instead.
module mapl_mp_utils_internal
   use mapl_ArrayReductions_mod, only: MAPL_MaxMin, MAPL_AreaMean
   ! Import all PackedTime functions (both prefixed and unprefixed)
   use mapl_PackedTime_mod,      only: MAPL_PackedDateCreate => PackedDateCreate, &
                                       MAPL_PackedTimeCreate => PackedTimeCreate, &
                                       MAPL_PackedDateTimeCreate => PackedDateTimeCreate, &
                                       MAPL_ESMFTimeFromPacked => ESMFTimeFromPacked, &
                                       MAPL_UnpackDate => UnpackDate, &
                                       MAPL_UnpackDateTime => UnpackDateTime, &
                                       PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate, &
                                       ESMFTimeFromPacked, UnpackDate, UnpackTime, UnpackDateTime
   use mapl_StringTemplate_mod,  only: StrTemplate, fill_grads_template, fill_grads_template_esmf
   use mapl_Shmem_mod
   private

   public :: MAPL_MaxMin, MAPL_AreaMean
   public :: MAPL_UnpackTime
   
   ! PackedTime functions with MAPL_ prefix
   public :: MAPL_PackedDateCreate, MAPL_PackedTimeCreate, MAPL_PackedDateTimeCreate
   public :: MAPL_ESMFTimeFromPacked
   public :: MAPL_UnpackDate, MAPL_UnpackDateTime
   
   ! Unprefixed PackedTime functions
   public :: PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate
   public :: ESMFTimeFromPacked, UnpackDate, UnpackTime, UnpackDateTime
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

end module mapl_mp_utils_internal
