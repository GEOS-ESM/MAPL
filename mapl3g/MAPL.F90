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
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use MAPL_PythonBridge
   use mapl_base3g
   use mapl_Profiler, initialize_profiler => initialize, finalize_profiler => finalize
   use MAPL_FieldUtils
   use MAPL_StateMaskMod
   use MAPL_StateArithmeticParserMod
   use MAPL_StateFilter
   

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.
   
end module MAPL
