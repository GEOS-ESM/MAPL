! Public interface (package) to MAPL
module MAPL
   use mapl_VM_API_mod
   use mapl_MaplFramework_mod
   use mapl_Generic
   use mapl_State_API_mod
   use mapl_Utils_API_mod
   use mapl_SplitCommunicator_mod
   use mapl_SimpleCommSplitter_mod
   use mapl_Sort_mod
   use mapl_Shmem_mod
   use mapl_Throw_mod
   use mapl_Range_mod
   use mapl_MinMax_mod
   use mapl_LoadBalance_mod
   use mapl_KeywordEnforcer_mod
   use mapl_Interp_mod
   use mapl_Hash_mod
   use mapl_ErrorHandling_mod
   use MAPL_Constants
   use mapl_CommGroupDescription_mod
   use mapl_AbstractCommSplitter_mod
   use mapl_Downbit_mod
   use mapl_Sleep_mod
   use pfio
   use mapl_Geom_API_mod
   use mapl_HConfig_API
   use mapl_VerticalGrid_API_mod
   use mapl_EsmfUtils_API_mod
   use mapl_Field_API
   use mapl_FieldBundle_API_mod
   use mapl_mp_utils
   use mapl_RegridderMgr_API_mod
   use mapl_Generic_API_mod
   use mapl_PythonBridge_mod
   use mapl_base_mod
   use mapl_Profiler_mod, initialize_profiler => initialize, finalize_profiler => finalize
    use mapl_FieldUtils
    use mapl_StateMask_mod
    use mapl_StateArithmeticParser_mod
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
    use mapl_ExceptionHandling_mod
    use mapl_ISO8601_DateTime_mod
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
