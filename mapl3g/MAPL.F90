! Public interface (package) to MAPL
module MAPL
   use mapl3g_VM_API
   use mapl3g_MaplFramework
   use generic3g
   use mapl3g_State_API
   use MaplShared
   use pfio
   use mapl3g_geom_API
   use mapl3g_hconfig_API
   use mapl3g_VerticalGrid_API
   use mapl3g_UngriddedDims, only: UngriddedDims
   use mapl3g_FieldBundle_API
   use MAPL_PythonBridge
   

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.
   
end module MAPL
