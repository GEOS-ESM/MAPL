! Internal umbrella for the MAPL.field library (aggregate).
! Combines field/, field_bundle/, and state/ subdirectories.
! This is the intermediate directory pattern: Internal = union of subdir Internals.
module mapl_field_internal

   ! field/ subdirectory
   use mapl_FieldUtils
   use mapl_FieldBLAS_mod
   use mapl_FieldUtilities_mod
   use mapl_FieldUnaryFunctions_mod
   use mapl_FieldBinaryOperations_mod
   use mapl_FieldUnits_mod
   use mapl_FieldCondensedArray_mod
   use mapl_FieldFillDefault_mod
   use mapl_FieldFillImpl_mod
   use mapl_FieldCreateImpl_mod
   use mapl_FieldDelta_mod
   use mapl_FieldGetImpl_mod
   use mapl_FieldSetImpl_mod
   use mapl_FieldInfo_mod
   use mapl_RestartModes_mod

   ! field_bundle/ subdirectory
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

   ! state/ subdirectory
   use mapl_StateGetImpl_mod
   use mapl_StateSet_mod
   use mapl_StateGetPointerImpl_mod
   use mapl_StateGetGeomImpl_mod
   use mapl_StateAddMethodImpl_mod
   use mapl_StateUtils
   use mapl_StateArithmeticParser_mod
   use mapl_StateMask_mod
   use mapl_StateFilter_mod
   use mapl_StateDestroyImpl_mod

   implicit none

end module mapl_field_internal
