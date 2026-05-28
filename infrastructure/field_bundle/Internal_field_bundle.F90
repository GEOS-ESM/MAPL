! Internal umbrella for the MAPL infrastructure/field_bundle subdirectory.
! Aggregates leaf modules from field_bundle/ for use by other MAPL subdirectories.
module mapl_field_bundle_internal

   use mapl_FieldBundleGetImpl_mod
   use mapl_FieldBundleGetByIndexImpl_mod
   use mapl_FieldBundleSetImpl_mod
   use mapl_FieldBundleInfo_mod
   use mapl_FieldBundleDelta_mod
   use mapl_FieldBundleCreateImpl_mod
   use mapl_FieldBundleCopyImpl_mod
   use mapl_FieldBundleDestroyImpl_mod
   use mapl_FieldBundleGetPointerImpl_mod
   use mapl_FieldBundleMatch_mod

   implicit none

end module mapl_field_bundle_internal
