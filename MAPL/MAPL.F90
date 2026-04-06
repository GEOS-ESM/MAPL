! This module re-exports the public entities
! of the legacy (MAPL2) underlying packages.
module MAPL2
   use MAPLBase_mod
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_GenericMod
   use MAPL_VarSpecMiscMod
#endif
   use ESMF_CFIOMod
   use pFIO
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_GridCompsMod
   use MAPL_ESMFFieldBundleRead
   use MAPL_ESMFFieldBundleWrite
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_OpenMP_Support, only : MAPL_get_current_thread => get_current_thread
   use MAPL_OpenMP_Support, only : MAPL_get_num_threads => get_num_threads
   use MAPL_OpenMP_Support, only : MAPL_find_bounds => find_bounds
   use MAPL_OpenMP_Support, only : MAPL_Interval => Interval
   use MAPL_Profiler, initialize_profiler => initialize, finalize_profiler => finalize
   use MAPL_FieldUtils
   use MAPL_StateUtils
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_PythonBridge
#endif
   implicit none
end module MAPL2
