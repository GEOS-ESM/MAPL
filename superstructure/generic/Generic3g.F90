module Generic3g
   use mapl_ESMF_Subset
   use mapl_Generic
   use mapl_Field_API
   use mapl_VariableSpec
   use mapl_GenericPhases
   use mapl_OuterMetaComponent
   use mapl_GenericGridComp, only: MAPL_GridCompCreate
   use mapl_VerticalGrid
   use mapl_BasicVerticalGrid
   use mapl_ModelVerticalGrid
   use mapl_ESMF_Interfaces
   use mapl_ComponentDriver
   use mapl_GriddedComponentDriver
   use mapl_ChildSpec
   use mapl_UserSetServices
   use mapl_ESMF_HConfigUtilities, only: MAPL_HConfigMatch
   use mapl_VerticalStaggerLoc
   use mapl_geomio
   use mapl_ESMF_Utilities
   use mapl_OpenMP_Support
end module Generic3g
