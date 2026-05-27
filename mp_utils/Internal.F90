! Internal umbrella for the MAPL.mp_utils layer.
! Exports everything needed by other MAPL subdirectories, including symbols
! that are not part of the public (Export) API.
! External users should use mapl_mp_utils_export instead.
module mapl_mp_utils_internal
   use mapl_ArrayReductions_mod, only: MAPL_MaxMin, MAPL_AreaMean
   use mapl_MemInfo_mod,         only: MAPL_MemInfoWrite => MemInfoWrite
   use mapl_TimeUtilities_mod,   only: MAPL_PackTime => PackTime, MAPL_UnpackTime => UnpackTime
   use mapl_OSUtilities_mod,     only: MAPL_GetCheckpointSubdir => get_checkpoint_subdir
   use mapl_PackedTime_mod,      only: PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate
   use mapl_StringTemplate_mod,  only: StrTemplate
   private

   public :: MAPL_MaxMin, MAPL_AreaMean
   public :: MAPL_MemInfoWrite
   public :: MAPL_PackTime, MAPL_UnpackTime
   public :: MAPL_GetCheckpointSubdir
   public :: PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate
   public :: StrTemplate

end module mapl_mp_utils_internal
