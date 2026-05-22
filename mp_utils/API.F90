! Public API for the MAPL.mp_utils layer (MPI-dependent utilities, Tier 2).
! This module re-exports all public symbols from MAPL.mp_utils for consumption
! by the top-level MAPL umbrella module.
module mapl_mp_utils_mod

   use mapl_ArrayReductions_mod, only: MAPL_MaxMin, MAPL_AreaMean
   use mapl_MemInfo_mod, only: MAPL_MemInfoWrite => MemInfoWrite
   use mapl_TimeUtilities_mod, only: MAPL_PackTime => PackTime, MAPL_UnpackTime => UnpackTime
   use mapl_OSUtilities_mod, only: MAPL_GetCheckpointSubdir => get_checkpoint_subdir
   use mapl_PackedTime_mod, only: PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate
   use mapl_StringTemplate_mod, only: StrTemplate

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.

end module mapl_mp_utils_mod
