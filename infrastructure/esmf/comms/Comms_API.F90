module mapl_Utilities_Comms_API_mod
   use mapl_Comms_mod, only: MAPL_Am_I_Root => Am_I_Root
   use mapl_Comms_mod, only: MAPL_NPES => num_pes

   use mapl_Comms_mod, only: MAPL_CommsSend => comms_send
   use mapl_Comms_mod, only: MAPL_CommsRecv => comms_recv
   use mapl_Comms_mod, only: MAPL_CommsSendRecv => comms_sendrecv

   use mapl_Comms_mod, only: MAPL_CommsGatherV => comms_gatherv
   use mapl_Comms_mod, only: MAPL_CommsScatterV => comms_scatterv

   use mapl_Comms_mod, only: MAPL_CommsAllGather => comms_allgather
   use mapl_Comms_mod, only: MAPL_CommsAllGatherV => comms_allgatherv

   use mapl_Comms_mod, only: MAPL_ArrayGather => array_gather
   use mapl_Comms_mod, only: MAPL_ArrayScatter => array_scatter

   use mapl_Comms_mod, only: MAPL_CommsAllReduceMin => comms_allreduce_min
   use mapl_Comms_mod, only: MAPL_CommsAllReduceMax => comms_allreduce_max
   use mapl_Comms_mod, only: MAPL_CommsAllReduceSum => comms_allreduce_sum
end module mapl_Utilities_Comms_API_mod
