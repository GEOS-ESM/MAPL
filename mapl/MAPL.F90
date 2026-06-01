! Public interface (package) to MAPL
module MAPL
   use mapl_VerticalGridManager_mod, only: VerticalGridManager, get_vertical_grid_manager
   use mapl_MaplFramework_mod

   ! Two-tier umbrella exports
   use mapl_mp_utils_export
   use mapl_Enums_export
   use mapl_utils_export
   use mapl_base_export
   use mapl_esmf_export
   use mapl_field_export
   use mapl_geom_export
   use mapl_vertical_grid_export
   use mapl_regridder_mgr_export
   use mapl_generic_export

   ! Legacy API modules (to be phased out)
   use mapl_VM_API_mod
   use mapl_Generic
   use mapl_State_API_mod
   use mapl_Shmem_mod
   use mapl_LoadBalance_mod
   use MAPL_Constants
   use pfio
   use mapl_Geom_API_mod
   use mapl_HConfig_API
   use mapl_vertical_grid_export
   use mapl_EsmfUtils_API_mod
   use mapl_field_export
   use mapl_field_bundle_export
   use mapl_RegridderMgr_API_mod
   use mapl_Generic_API_mod
   use mapl_PythonBridge_mod
   use mapl_base_mod
   use mapl_Profiler_mod, initialize_profiler => initialize, finalize_profiler => finalize
   use mapl_FieldUtils
   use mapl_StateMask_mod
   use mapl_StateArithmeticParser_mod, only: parser_variables_in_expression, MAPL_StateEval
   use mapl_StateFilter_mod

   ! Additional modules needed by gridcomps (issues #4958/#4959)
   ! generic3g layer
   use mapl_Generic_mod
   ! mapl_GenericGridComp excluded: its public setServices conflicts with
   ! the standalone setServices subroutines defined in gridcomp files.
   ! Cap.F90 still uses it directly; address in a later increment.
   use mapl_ComponentSpec_mod
   use mapl_RestartHandler_mod
   ! esmf layer - FieldPointerUtilities has no used public entities (#4999)
   use mapl_ESMF_Time_Utilities_mod
   use mapl_StateItemImpl_mod
   use mapl_SimpleAlarm_mod
   ! regridder layer
   use mapl_EsmfRegridder_mod
   use mapl_RegridderMethods_mod
   ! hconfig layer - HConfigAs has no used public entities (#4999)
   ! mp_utils layer
   use mapl_StringTemplate_mod
   ! base layer
   use mapl_FileMetadataUtils_mod
   ! geom layer (transitively linked via regridder_mgr)
   use mapl_GridGetGlobal_mod
   ! GeomIO layer
   use mapl_geomio
   use mapl_CompressionSettings_mod

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.

end module MAPL
