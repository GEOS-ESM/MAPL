! This module re-exports the public entities
! of the underlying packages.
#ifdef MAPL_SUPPORT_MAPL3
module MAPL2
#else
module MAPL
#endif
   use MAPLBase_mod
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_GenericMod
   use MAPL_VarSpecMiscMod
#endif
   use ESMF_CFIOMod
   use pFIO
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_GridCompsMod
   use mapl_StubComponent
#endif
   use MAPL_ESMFFieldBundleRead
   use MAPL_ESMFFieldBundleWrite
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_OpenMP_Support, only : MAPL_get_current_thread => get_current_thread
   use MAPL_OpenMP_Support, only : MAPL_get_num_threads => get_num_threads
   use MAPL_OpenMP_Support, only : MAPL_find_bounds => find_bounds
   use MAPL_OpenMP_Support, only : MAPL_Interval => Interval
#endif
   use MAPL_Profiler, initialize_profiler =>initialize, finalize_profiler =>finalize
   use MAPL_FieldUtils
   use MAPL_StateUtils
#ifndef MAPL_SUPPORT_MAPL3
   use MAPL_PythonBridge
#endif
   implicit none
#ifdef MAPL_SUPPORT_MAPL3
end module MAPL2
#else
end module MAPL
#endif

#ifdef MAPL_SUPPORT_MAPL3
module MAPL2_Mod
   use MAPL2
end module MAPL2_Mod
#else
module MAPL_Mod
   use MAPL
end module MAPL_Mod
#endif

