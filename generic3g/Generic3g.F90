module Generic3g
   use mapl3g_GenericPhases
   use mapl3g_Generic
   use mapl3g_OuterMetaComponent
   use mapl3g_GenericGridComp, only: MAPL_GridCompCreate
   use mapl3g_VerticalGrid
   use mapl3g_VerticalDimSpec
   use mapl3g_ESMF_Interfaces
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_UserSetServices
   use mapl3g_ESMF_HConfigUtilities, only: MAPL_HConfigMatch
end module Generic3g
