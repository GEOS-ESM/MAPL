target_sources(MAPL.generic3g PRIVATE

  # containers
  ActualPtSpecPtrMap.F90
  ActualPtComponentDriverMap.F90
  VirtualPtStateItemPtrMap.F90
  VirtualPtStateItemSpecMap.F90
  ActualPtStateItemSpecMap.F90
  StateItemVector.F90

  RegistryPtr.F90
  RegistryPtrMap.F90
  ActualPtVector.F90
  ActualPtSpecPtrMap.F90
  ActualPtVec_Map.F90

  AbstractRegistry.F90
  StateRegistry.F90
  StateRegistry_Lifecycle_smod.F90
  StateRegistry_Hierarchy_smod.F90
  StateRegistry_Extensions_smod.F90
  StateRegistry_Propagation_smod.F90
  StateRegistry_Actions_smod.F90
  ExtensionFamily.F90
  VirtualPtFamilyMap.F90
)
