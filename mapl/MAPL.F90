! Public interface (package) to MAPL
module MAPL
   use mapl_VM_API
   use mapl_MaplFramework
   use generic3g
   use mapl_State_API
   use mapl_String
   use mapl_StringUtilities
   use mapl_FileSystemUtilities
   use mapl_DSO_Utilities
   use mapl_SplitCommunicatorMod
   use mapl_SimpleCommSplitterMod
   use mapl_SortMod
   use mapl_ShmemMod
   use mapl_ThrowMod
   use mapl_RangeMod
   use mapl_MinMaxMod
   use mapl_LoadBalanceMod
   use mapl_KeywordEnforcerMod
   use mapl_InterpMod
   use mapl_HashMod
   use mapl_ErrorHandling
   use mapl_DirPathMod
   use mapl_Constants
   use mapl_CommGroupDescriptionMod
   use mapl_AbstractCommSplitterMod
   use mapl_DownbitMod
   use mapl_sleepMod
   use pfio
   use mapl_geom_API
   use mapl_hconfig_API
   use mapl_VerticalGrid_API
   use mapl_EsmfUtils_API
   use mapl_Field_API
   use mapl_FieldBundle_API
   use mapl_mp_utils
   use mapl_RegridderMgr_API
   use mapl_Generic3g_API
   use MAPL_PythonBridge
   use mapl_base3g
   use mapl_Profiler, initialize_profiler => initialize, finalize_profiler => finalize
    use MAPL_FieldUtils
    use MAPL_StateMaskMod
    use MAPL_StateArithmeticParserMod
    use MAPL_StateFilter

    ! Additional modules needed by gridcomps (issues #4958/#4959)
    ! generic3g layer
    use mapl_Generic
    ! mapl_GenericGridComp excluded: its public setServices conflicts with
    ! the standalone setServices subroutines defined in gridcomp files.
    ! Cap.F90 still uses it directly; address in a later increment.
    use mapl_ComponentSpec
    use mapl_RestartHandler
    ! esmf layer
    use mapl_ESMF_Time_Utilities
    use mapl_SimpleAlarm
    use mapl_StateItemImpl
    use MAPL_FieldPointerUtilities
    use MAPL_ExceptionHandling
    use MAPL_ISO8601_DateTime
    ! regridder layer
    use mapl_EsmfRegridder
    use mapl_RegridderMethods
    ! hconfig layer
    use mapl_HConfigAs
    ! mp_utils layer
    use MAPL_StringTemplate
    ! base3g layer
    use MAPL_FileMetadataUtilsMod
    ! utils layer
    use mapl_OS
    ! geom layer (transitively linked via regridder_mgr)
    use mapl_GridGetGlobal
    ! GeomIO layer
    use mapl_geomio
    use mapl_CompressionSettings

    ! We use default PUBLIC to avoid explicitly listing exports from
    ! the other layers.  When the dust settles and such micro
    ! management become feasible, this can be reconsidered.

end module MAPL
