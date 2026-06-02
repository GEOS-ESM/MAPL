! Internal umbrella for the MAPL superstructure/generic layer.
! Aggregates leaf modules compiled into MAPL.generic for use by other MAPL subdirectories.
module mapl_generic_internal

   use mapl3_GenericGrid
   use mapl_ComponentSpecParser_mod
   use mapl_UserSetServices_mod
   use mapl_MethodPhasesMap_mod
   use mapl_MethodPhasesMapUtils_mod
   use mapl_InnerMetaComponent_mod
   use mapl_OuterMetaComponent_mod
   use mapl_GenericGridComp_mod
   use mapl_Generic_mod
   use mapl_Deprecated_mod
   use mapl_RestartHandler_mod
   use mapl_RunEntryPoint_mod
   use mapl_EntryPointVector_mod
   use mapl_OpenMP_Support_mod

   implicit none

end module mapl_generic_internal
