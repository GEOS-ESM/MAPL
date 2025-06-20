module Generic3g
<<<<<<< HEAD
   use mapl3g_ESMF_Subset
   use mapl3g_Generic
   use mapl3g_Field_API
   use mapl3g_VariableSpec
=======
   use mapl3g_Generic
>>>>>>> e5b0046f859 (Finally a compromise that passes all compilers.)
   use mapl3g_GenericPhases
   use mapl3g_OuterMetaComponent
   use mapl3g_GenericGridComp, only: MAPL_GridCompCreate
   use mapl3g_VerticalGrid
   use mapl3g_BasicVerticalGrid
   use mapl3g_ModelVerticalGrid
   use mapl3g_ESMF_Interfaces
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ChildSpec
   use mapl3g_UserSetServices
   use mapl3g_ESMF_HConfigUtilities, only: MAPL_HConfigMatch
   use mapl3g_RestartHandler, only: MAPL_RESTART, MAPL_RESTART_OPTIONAL, MAPL_RESTART_SKIP
   use mapl3g_RestartHandler, only: MAPL_RESTART_REQUIRED, MAPL_RESTART_BOOT, MAPL_RESTART_SKIP_INITIAL
<<<<<<< HEAD
   use mapl3g_VerticalStaggerLoc
   use mapl3g_geomio
=======
>>>>>>> e5b0046f859 (Finally a compromise that passes all compilers.)

end module Generic3g
