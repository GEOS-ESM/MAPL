module mapl3g_Utilities_Comms_API
   use mapl3g_Comms, only: MAPL_Am_I_Root => Am_I_Root
   use mapl3g_Comms, only: MAPL_NPES => num_pes

   use mapl3g_Comms, only: MAPL_CommsGatherV => comms_gatherv
   use mapl3g_Comms, only: MAPL_CommsScatterV => comms_scatterv

   use mapl3g_Comms, only: MAPL_CommsAllGather => comms_allgather
   use mapl3g_Comms, only: MAPL_CommsAllGatherV => comms_allgatherv

   use mapl3g_Comms, only: MAPL_ArrayGather => array_gather
   use mapl3g_Comms, only: MAPL_ArrayScatter => array_scatter

   use mapl3g_Comms, only: MAPL_CommsAllReduceMin => comms_allreduce_min
   use mapl3g_Comms, only: MAPL_CommsAllReduceMax => comms_allreduce_max
   use mapl3g_Comms, only: MAPL_CommsAllReduceSum => comms_allreduce_sum
end module mapl3g_Utilities_Comms_API
