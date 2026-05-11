

module MAPLBase_Mod

  use ESMFL_Mod         !  Stopgap
  use MAPL_ExceptionHandling
  use MAPL_BaseMod
   use NCIOMod
  use MAPL_LocStreamMod
  use MAPL_ShmemMod
  use MAPL_MemUtilsMod
  use MAPL_SimpleBundleMod
  use MAPL_ServerManager
  use MAPL_FileMetadataUtilsMod
  use MAPL_VerticalDataMod

  logical, save, private :: mapl_is_initialized = .false.

end module MAPLBase_Mod
