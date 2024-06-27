target_sources(MAPL.shared PRIVATE

    BroadcastToNodes_1DI4.F90 BroadcastToNodes_1DR4.F90
    BroadcastToNodes_1DR8.F90 BroadcastToNodes_2DI4.F90
    BroadcastToNodes_2DR4.F90 BroadcastToNodes_2DR8.F90
    BroadcastToNodes_3DI4.F90 BroadcastToNodes_3DR4.F90
    BroadcastToNodes_3DR8.F90 BroadcastToNodes_4DR4.F90
    BroadcastToNodes_4DR8.F90
    getNodeComm.F90 getNodeRootsComm.F90 GetSharedMemory.F90
    MAPL_AllocateShared_1DI4.F90 MAPL_AllocateShared_1DL4.F90
    MAPL_AllocateShared_1DR4.F90 MAPL_AllocateShared_1DR8.F90
    MAPL_AllocateShared_2DI4.F90 MAPL_AllocateShared_2DR4.F90
    MAPL_AllocateShared_2DR8.F90 MAPL_AllocNodeArray_1DI4.F90
    MAPL_AllocNodeArray_1DL4.F90 MAPL_AllocNodeArray_1DR4.F90
    MAPL_AllocNodeArray_1DR8.F90 MAPL_AllocNodeArray_2DI4.F90
    MAPL_AllocNodeArray_2DR4.F90 MAPL_AllocNodeArray_2DR8.F90
    MAPL_AllocNodeArray_3DI4.F90 MAPL_AllocNodeArray_3DR4.F90
    MAPL_AllocNodeArray_3DR8.F90 MAPL_AllocNodeArray_4DI4.F90
    MAPL_AllocNodeArray_4DR4.F90 MAPL_AllocNodeArray_4DR8.F90
    MAPL_AllocNodeArray_5DR8.F90 MAPL_AllocNodeArray_6DR8.F90
    MAPL_CoresPerNodeGet.F90
    MAPL_DeAllocNodeArray_1DI4.F90 MAPL_DeAllocNodeArray_1DL4.F90
    MAPL_DeAllocNodeArray_1DR4.F90 MAPL_DeAllocNodeArray_1DR8.F90
    MAPL_DeAllocNodeArray_2DI4.F90 MAPL_DeAllocNodeArray_2DR4.F90
    MAPL_DeAllocNodeArray_2DR8.F90 MAPL_DeAllocNodeArray_3DI4.F90
    MAPL_DeAllocNodeArray_3DR4.F90 MAPL_DeAllocNodeArray_3DR8.F90
    MAPL_DeAllocNodeArray_4DI4.F90 MAPL_DeAllocNodeArray_4DR4.F90
    MAPL_DeAllocNodeArray_4DR8.F90 MAPL_DeAllocNodeArray_5DR8.F90
    MAPL_DeAllocNodeArray_6DR8.F90
    MAPL_FinalizeShmem.F90 MAPL_GetNewRank.F90
    MAPL_GetNodeInfo.F90 MAPL_InitializeShmem.F90
    MAPL_ShmemAmOnFirstNode.F90 MAPL_SyncSharedMemory.F90
    ReleaseSharedMemory.F90 Shmem.F90
)
