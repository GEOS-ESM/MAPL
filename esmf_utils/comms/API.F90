module mapl3g_Utilities_Comms_API
   use mapl3g_Comms, only: MAPL_Am_I_Root => Am_I_Root

   use mapl3g_Comms, only: MAPL_CommsGatherV => CommsGatherV
   use mapl3g_Comms, only: MAPL_CommsScatterV => CommsScatterV

   use mapl3g_Comms, only: MAPL_ArrayGather => ArrayGather
   use mapl3g_Comms, only: MAPL_ArrayScatter => ArrayScatter

   use mapl3g_Comms, only: MAPL_CommsAllReduceMin => CommsAllReduceMin
   use mapl3g_Comms, only: MAPL_CommsAllReduceMax => CommsAllReduceMax
   use mapl3g_Comms, only: MAPL_CommsAllReduceSum => CommsAllReduceSum
end module mapl3g_Utilities_Comms_API
