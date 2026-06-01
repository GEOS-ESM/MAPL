! Public Export umbrella for the MAPL.mp_utils layer.
! Re-exports only the symbols that are part of MAPL's public API
! (i.e., entities carrying the MAPL_ prefix).
! This is what mapl/MAPL.F90 imports from.
! For internal MAPL use, see mapl_mp_utils_internal.
module mapl_mp_utils_export
   use mapl_mp_utils_internal

   implicit none
   private

   ! Statistics
   public :: MAPL_MaxMin
   public :: MAPL_AreaMean

   ! Memory info
   public :: MAPL_MemInfoWrite

   ! Time packing
   public :: MAPL_PackTime
   public :: MAPL_UnpackTime
   public :: MAPL_UnpackDate
   public :: MAPL_UnpackDateTime

   ! Checkpoint
   public :: MAPL_GetCheckpointSubdir

   ! PackedTime functions with MAPL_ prefix
   public :: MAPL_PackedDateCreate
   public :: MAPL_PackedTimeCreate
   public :: MAPL_PackedDateTimeCreate
   public :: MAPL_ESMFTimeFromPacked

   ! Backward compatibility: Unprefixed names (TODO: remove after client repos migrated)
   ! See issue #5011 - these should be removed once GEOSgcm and other clients updated
   public :: PackedDateCreate
   public :: PackedTimeCreate
   public :: PackedDateTimeCreate
   public :: ESMFTimeFromPacked
   public :: UnpackDate
   public :: UnpackTime
   public :: UnpackDateTime
   public :: StrTemplate

end module mapl_mp_utils_export
