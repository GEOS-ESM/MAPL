! Internal umbrella for the MAPL infrastructure/esmf layer.
! Aggregates leaf modules compiled into MAPL.esmf for use by
! other MAPL subdirectories.
module mapl_esmf_internal

   ! Core ESMF utilities
   use mapl_ESMF_Interfaces_mod
   use mapl_ESMF_Utilities_mod
   use mapl_ESMF_Time_Utilities_mod
   use mapl_ESMF_Subset_mod
   use mapl_ESMF_HConfigUtilities_mod

   ! Info / metadata utilities
   use mapl_InfoUtilities_mod

   ! Ungridded dimensions
   use mapl_UngriddedDim_mod
   use mapl_UngriddedDims_mod
   use mapl_UngriddedDimVector_mod

   ! VM
   use mapl_vm_mod

   ! Bounds / grid utilities
   use mapl_LU_Bound_mod
   use mapl_HorizontalDimsSpec_mod
   use mapl_DistGridGet_mod
   use mapl_FieldPointerUtilities_mod

   ! HConfig
   use mapl_HConfig_API
   use mapl_HConfigAs_mod
   use mapl_HConfigUtilities_mod
   use mapl_get_hconfig_mod
   use mapl_hconfig_get_mod
   use mapl_hconfig_params_mod
   use mapl_generalized_equality_mod

   ! Alarm
   use mapl_SimpleAlarm_mod

   ! State item
   use mapl_StateItemImpl_mod

   ! Comms
   use mapl_Comms_mod
   use mapl_ShmemComms_mod

   implicit none

end module mapl_esmf_internal
