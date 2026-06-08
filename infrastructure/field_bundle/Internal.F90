! Internal umbrella for the MAPL infrastructure/field_bundle subdirectory.
! Aggregates leaf modules from field_bundle/ for use by other MAPL subdirectories.
module mapl_field_bundle_internal

   use mapl_FieldBundleGet_mod
   use mapl_FieldBundleGetByIndex_mod
   use mapl_FieldBundleSet_mod
   use mapl_FieldBundleInfo_mod
   use mapl_FieldBundleDelta_mod
   use mapl_FieldBundleCreate_mod
   use mapl_FieldBundleCopy_mod
   use mapl_FieldBundleDestroy_mod
   use mapl_FieldBundleGetPointer_mod
   use mapl_FieldBundleMatch_mod

   implicit none

end module mapl_field_bundle_internal
