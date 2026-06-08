! Public interface (package) to MAPL
module MAPL

   use mapl_Constants
   use mapl_Profiler_mod, profiler_initialize => initialize, profiler_finalize => finalize

   use mapl_enums_api, &
        VerticalStaggerLoc => mapl_VerticalStaggerLoc, &
        VERTICAL_STAGGER_NONE => MAPL_VERTICAL_STAGGER_NONE, &
        VERTICAL_STAGGER_EDGE => MAPL_VERTICAL_STAGGER_EDGE, &
        VERTICAL_STAGGER_CENTER => MAPL_VERTICAL_STAGGER_CENTER
   use mapl_utils_api
   use mapl_mp_utils_api
   use mapl_infrastructure_api
   use mapl_superstructure_api
   use mapl_base_api

   use pfio
   use mapl_MaplFramework_mod

end module MAPL
