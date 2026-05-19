! Public interface (package) to MAPL3
module mapl_Utilities

   use mapl_MaxMin, only: MAPL_MaxMin => MaxMin
   use mapl_AreaMean, only: MAPL_AreaMean => AreaMean
   use mapl_MemInfo, only: MAPL_MemInfoWrite => MemInfoWrite
   use mapl_TimeUtilities, only: MAPL_PackTime => PackTime, MAPL_UnpackTime => UnpackTime
   use mapl_OSUtilities, only: MAPL_GetCheckpointSubdir => get_checkpoint_subdir

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.

end module mapl_Utilities
