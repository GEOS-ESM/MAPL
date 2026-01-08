! Public interface (package) to MAPL3
module mapl3_utilities

   use mapl3g_MaxMin, only: MAPL_MaxMin => MaxMin
   use mapl3g_AreaMean, only: MAPL_AreaMean => AreaMean
   use mapl3g_MemInfo, only: MAPL_MemInfoWrite => MemInfoWrite

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.

end module mapl3_utilities
