

module MAPLBase_Mod

  use ESMFL_Mod         !  Stopgap
  use MAPL_ExceptionHandling
  use MAPL_BaseMod
  use MAPL_BaseMod, only: MAPL_GRID_INTERIOR
! For temporary backward compatibility after moving/renaming:
  use MAPL_BaseMod, only: ESMF_GRID_INTERIOR => MAPL_GRID_INTERIOR
   use MAPL_IOMod
  use MAPL_LocStreamMod
  use MAPL_ShmemMod
  use MAPL_MemUtilsMod
  use MAPL_MaxMinMod
  use MAPL_SimpleBundleMod
  use MAPL_ServerManager
  use MAPL_FileMetadataUtilsMod
  use MAPL_VerticalDataMod

  logical, save, private :: mapl_is_initialized = .false.

end module MAPLBase_Mod
