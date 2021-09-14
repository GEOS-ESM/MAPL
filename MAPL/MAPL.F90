! This module re-exports the public entities
! of the underlying packages.
module MAPL
   use MAPLBase_mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use MAPL_ExtDataGridCompMod, only: T_EXTDATA_STATE, EXTDATA_WRAP
   use MAPL_CFIOMod
   use MAPL_SimpleBundleMod
   use ESMF_CFIOMod
   use pFIO
   use MAPL_GridCompsMod
   implicit none
end module MAPL

module MAPL_Mod
   use MAPL
end module MAPL_Mod
   
