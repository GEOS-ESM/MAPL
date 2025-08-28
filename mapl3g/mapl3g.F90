! Public interface (package) to MAPL3
module mapl3
   use mapl3g_VM_API
   use mapl3g_MaplFramework
   use generic3g
   use mapl3g_State_API
   use MaplShared
   use pfio
   use mapl3g_geom_API
   use mapl3g_hconfig_API
   

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.
   
end module mapl3
