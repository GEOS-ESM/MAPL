target_sources(MAPL.generic3g PRIVATE

  # containers
  ActualPtSpecPtrMap.F90
  ActualPtComponentDriverMap.F90
  VirtualPtStateItemPtrMap.F90
  VirtualPtStateItemSpecMap.F90
  ActualPtStateItemSpecMap.F90
  StateItemVector.F90

  AbstractRegistry.F90
  RegistryPtr.F90
  RegistryPtrMap.F90
  ActualPtVector.F90
  ActualPtSpecPtrMap.F90
  ActualPtVec_Map.F90
  HierarchicalRegistry.F90

  Registry.F90
  StateItemExtension.F90
  StateItemExtensionVector.F90
  StateItemExtensionPtrVector.F90
  ExtensionFamily.F90
  VirtualPtExtensionsMap.F90
)
