! Public API for the MAPL.mp_utils layer (MPI-dependent utilities, Tier 2).
! This module re-exports all public symbols from MAPL.mp_utils for consumption
! by the top-level MAPL umbrella module.
module mapl_mp_utils

   use mapl_ArrayReductions, only: MAPL_MaxMin, MAPL_AreaMean
   use mapl_MemInfo, only: MAPL_MemInfoWrite => MemInfoWrite
   use mapl_TimeUtilities, only: MAPL_PackTime => PackTime, MAPL_UnpackTime => UnpackTime
   use mapl_OSUtilities, only: MAPL_GetCheckpointSubdir => get_checkpoint_subdir

   ! We use default PUBLIC to avoid explicitly listing exports from
   ! the other layers.  When the dust settles and such micro
   ! management become feasible, this can be reconsidered.

end module mapl_mp_utils
