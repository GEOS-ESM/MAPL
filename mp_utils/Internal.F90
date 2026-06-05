! Internal umbrella for the MAPL.mp_utils layer.
! Exports everything needed by other MAPL subdirectories, including symbols
! that are not part of the public (Export) API.
! External users should use mapl_mp_utils_export instead.
module mapl_mp_utils_internal
   use mapl_ArrayReductions_mod, only: MAPL_MaxMin, MAPL_AreaMean
   use mapl_TimeUtilities_mod,   only: MAPL_PackTime => PackDate, MAPL_UnpackTime => UnpackDate
   ! Import all PackedTime functions (both prefixed and unprefixed)
   use mapl_PackedTime_mod,      only: MAPL_PackedDateCreate => PackedDateCreate, &
                                       MAPL_PackedTimeCreate => PackedTimeCreate, &
                                       MAPL_PackedDateTimeCreate => PackedDateTimeCreate, &
                                       MAPL_ESMFTimeFromPacked => ESMFTimeFromPacked, &
                                       MAPL_UnpackDate => UnpackDate, &
                                       MAPL_UnpackDateTime => UnpackDateTime, &
                                       PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate, &
                                       ESMFTimeFromPacked, UnpackDate, UnpackTime, UnpackDateTime
   use mapl_StringTemplate_mod,  only: StrTemplate, fill_grads_template, fill_grads_template_esmf
   use mapl_Shmem_mod
   private

   public :: MAPL_MaxMin, MAPL_AreaMean
   public :: MAPL_PackTime, MAPL_UnpackTime
   
   ! PackedTime functions with MAPL_ prefix
   public :: MAPL_PackedDateCreate, MAPL_PackedTimeCreate, MAPL_PackedDateTimeCreate
   public :: MAPL_ESMFTimeFromPacked
   public :: MAPL_UnpackDate, MAPL_UnpackDateTime
   
   ! Unprefixed PackedTime functions
   public :: PackedDateCreate, PackedTimeCreate, PackedDateTimeCreate
   public :: ESMFTimeFromPacked, UnpackDate, UnpackTime, UnpackDateTime
   public :: StrTemplate
   public :: fill_grads_template
   public :: fill_grads_template_esmf

   public :: mapl_SyncSharedMemory

   public :: mapl_GetNodeInfo
   public :: mapl_InitializeShmem
   public :: mapl_FinalizeShmem

end module mapl_mp_utils_internal
