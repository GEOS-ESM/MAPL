! This module re-exports the public entities
! of the underlying packages.
module MAPL
   use MAPLBase_mod
   use MAPL_GenericMod
   use MAPL_VarSpecMiscMod
#if defined(BUILD_WITH_EXTDATA2G)
  use MAPL_ExtDataGridCompNG, only : T_EXTDATA_STATE, EXTDATA_WRAP
#else
  use MAPL_ExtDataGridCompMod, only : T_EXTDATA_STATE, EXTDATA_WRAP
#endif
   use ESMF_CFIOMod
   use pFIO
   use MAPL_GridCompsMod
   use mapl_StubComponent
   implicit none
end module MAPL

module MAPL_Mod
   use MAPL
end module MAPL_Mod
   
