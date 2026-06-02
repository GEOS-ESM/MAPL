! Public interface (package) to MAPL
module MAPL

   use mapl_Constants
   use mapl_Profiler_mod, profiler_initialize => initialize, profiler_finalize => finalize

   use mapl_enums_export
   use mapl_utils_export
   use mapl_mp_utils_export
   use mapl_infrastructure_export
   use mapl_superstructure_export
   use mapl_base_export
   use mapl_base_mod

   use pfio
   use mapl_MaplFramework_mod

   ! Random leaf modules that need to be addressed
!!$
!!$   ! Legacy API modules (to be phased out)
!!$   use mapl_Shmem_mod
!!$   use mapl_LoadBalance_mod
!!$   use MAPL_Constants
!!$   use mapl_PythonBridge_mod
!!$   use mapl_base_mod
!!$   use mapl_Profiler_mod, initialize_profiler => initialize, finalize_profiler => finalize
!!$   use mapl_FieldUtils
!!$   use mapl_StateMask_mod
!!$   use mapl_StateArithmeticParser_mod, only: parser_variables_in_expression, MAPL_StateEval
!!$   use mapl_StateFilter_mod
!!$
!!$   use mapl_ComponentSpec_mod
!!$   use mapl_RestartHandler_mod
!!$   ! esmf layer - FieldPointerUtilities has no used public entities (#4999)
!!$   use mapl_ESMF_Time_Utilities_mod
!!$   use mapl_StateItemImpl_mod
!!$   use mapl_SimpleAlarm_mod
!!$   ! regridder layer
!!$   use mapl_EsmfRegridder_mod
!!$   use mapl_RegridderMethods_mod
!!$   ! hconfig layer
!!$   ! mp_utils layer
!!$   use mapl_StringTemplate_mod
!!$   ! base layer
!!$   use mapl_FileMetadataUtils_mod
!!$   ! geom layer (transitively linked via regridder_mgr)
!!$   use mapl_GridGetGlobal_mod
!!$   ! GeomIO layer
!!$   use mapl_geomio
!!$   use mapl_CompressionSettings_mod
!!$
!!$   ! We use default PUBLIC to avoid explicitly listing exports from
!!$   ! the other layers.  When the dust settles and such micro
!!$   ! management become feasible, this can be reconsidered.

end module MAPL
