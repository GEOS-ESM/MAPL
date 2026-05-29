! Public Export umbrella for the MAPL.mp_utils layer.
! Re-exports only the symbols that are part of MAPL's public API
! (i.e., entities carrying the MAPL_ prefix).
! This is what mapl/MAPL.F90 imports from.
! For internal MAPL use, see mapl_mp_utils_internal.
module mapl_mp_utils_export
   use mapl_mp_utils_internal
   
   private

   public :: MAPL_MaxMin, MAPL_AreaMean
   public :: MAPL_MemInfoWrite
   public :: MAPL_PackTime, MAPL_UnpackTime
   public :: MAPL_GetCheckpointSubdir
   
   ! PackedTime functions with MAPL_ prefix
   public :: MAPL_PackedDateCreate, MAPL_PackedTimeCreate, MAPL_PackedDateTimeCreate
   public :: MAPL_ESMFTimeFromPacked
   public :: MAPL_UnpackDate, MAPL_UnpackDateTime

   ! Backward compatibility: Unprefixed names (TODO: remove after client repos migrated)
   ! See issue #5011 - these should be removed once GEOSgcm and other clients updated
   public :: PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate
   public :: ESMFTimeFromPacked, UnpackDate, UnpackTime, UnpackDateTime
   public :: StrTemplate

end module mapl_mp_utils_export
