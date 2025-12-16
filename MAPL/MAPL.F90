! This module re-exports the public entities
! of the underlying packages.
module MAPL
   use MAPLBase_mod
   use ESMF_CFIOMod
   use pFIO
   use MAPL_GridCompsMod
   use MAPL_ESMFFieldBundleRead
   use MAPL_ESMFFieldBundleWrite
   use MAPL_Profiler, initialize_profiler =>initialize, finalize_profiler =>finalize
   use MAPL_FieldUtils
   use MAPL_StateUtils
   implicit none
end module MAPL

module MAPL_Mod
   use MAPL
end module MAPL_Mod
   
