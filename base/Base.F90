

module MAPLBase_Mod

  use ESMFL_Mod         !  Stopgap
  use MAPL_ExceptionHandling
  use MAPL_BaseMod
  use MAPL_BaseMod, only: MAPL_GRID_INTERIOR
! For temporary backward compatibility after moving/renaming:
  use MAPL_BaseMod, only: ESMF_GRID_INTERIOR => MAPL_GRID_INTERIOR
  use MAPL_IOMod
  use MAPL_CFIOMod
  use MAPL_CommsMod
! For temporary backward compatibility after Constants Library
  use MAPL_ConstantsMod
  use MAPL_ConstantsMod, only: MAPL_PI_R8
  use MAPL_ConfigMod
  use MAPL_SortMod
  use MAPL_ProfMod
  use MAPL_SunMod
  use MAPL_LocStreamMod
  use MAPL_InterpMod
  use MAPL_HeapMod
  use MAPL_SatVaporMod
  use MAPL_CapOptionsMod
#ifdef USE_FLAP
  use MAPL_FlapCLIMod
#endif
  use MAPL_MemUtilsMod
  use MAPL_HashMod
  use MAPL_LoadBalanceMod
  use MAPL_AbstractGridFactoryMod
  use MAPL_AbstractRegridderMod
  use MAPL_RegridderManagerMod
  use MAPL_NewRegridderManager
  use mapl_HorizontalFluxRegridder
  use MAPL_TransposeRegridderMod
  use MAPL_RegridderSpec
  use MAPL_RegridderTypeSpec
  use MAPL_RegridMethods
  use MAPL_GridManagerMod
  use MAPL_LatLonGridFactoryMod
  use MAPL_CubedSphereGridFactoryMod
  use MAPL_ExternalGridFactoryMod
  use MAPL_ShmemMod
  use MAPL_MaxMinMod
  use MAPL_SimpleBundleMod
  use MAPL_NewArthParserMod
  use MAPL_DirPathMod
  use MAPL_KeywordEnforcerMod
  use MAPL_SimpleCommSplitterMod
  use MAPL_SplitCommunicatorMod
  use MAPL_EtaHybridVerticalCoordinateMod
  use MAPL_ApplicationSupport
  use MAPL_ServerManager
  logical, save, private :: mapl_is_initialized = .false.

end module MAPLBase_Mod
