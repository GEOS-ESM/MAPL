

module MAPL_Mod

  use MAPL_ExceptionHandling
  use ESMFL_Mod         !  Stopgap
  use MAPL_ExceptionHandling
  use MAPL_BaseMod
  use MAPL_BaseMod, only: MAPL_GRID_INTERIOR
! For temporary backward compatibility after moving/renaming:
  use MAPL_BaseMod, only: ESMF_GRID_INTERIOR => MAPL_GRID_INTERIOR
  use MAPL_IOMod
  use MAPL_CFIOMod
  use MAPL_CommsMod
  use MAPL_LocStreamMod
  use MAPL_GenericMod
  use MAPL_VarSpecMod
  use MAPL_ConstantsMod
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
  use MAPL_FlapCapOptionsMod
#endif
  use MAPL_MemUtilsMod
  use MAPL_HashMod
  use MAPL_LoadBalanceMod
  use MAPL_ExtDataGridCompMod, only : T_EXTDATA_STATE, EXTDATA_WRAP
  use MAPL_AbstractGridFactoryMod
  use MAPL_AbstractRegridderMod
  use MAPL_RegridderManagerMod
  use MAPL_NewRegridderManager
  use mapl_HorizontalFluxRegridder
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
  logical, save, private :: mapl_is_initialized = .false.

end module MAPL_Mod
