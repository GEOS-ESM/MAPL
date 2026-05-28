! Export umbrella for the MAPL.field library (aggregate).
! Public API combining field/, field_bundle/, and state/ subdirectories.
module mapl_field_export

   use ESMF, only: MAPL_FieldBundleAdd => ESMF_FieldBundleAdd
   use mapl_FieldPointerUtilities_mod, only: MAPL_FieldClone => FieldClone
   use mapl_FieldCreateImpl_mod, only: MAPL_FieldCreate
   use mapl_FieldCreateImpl_mod, only: MAPL_FieldEmptyComplete
   use mapl_FieldCreateImpl_mod, only: MAPL_FieldsAreAliased
   use mapl_FieldGetImpl_mod, only: MAPL_FieldGet => FieldGet
   use mapl_FieldSetImpl_mod, only: MAPL_FieldSet => FieldSet
   use mapl_FieldFillImpl_mod, only: MAPL_FieldFill => FieldFill
   use mapl_FieldPointerUtilities_mod, only: MAPL_AssignFptr => assign_fptr
   
   use mapl_FieldBundleCopyImpl_mod, only: MAPL_FieldBundleCopy => FieldBundleCopy
   use mapl_FieldBundleCreateImpl_mod, only: MAPL_FieldBundleCreate => FieldBundleCreate
   use mapl_FieldBundleCreateImpl_mod, only: MAPL_FieldBundlesAreAliased => FieldBundlesAreAliased
   use mapl_FieldBundleDestroyImpl_mod, only: MAPL_FieldBundleDestroy
   use mapl_FieldBundleGetImpl_mod, only: MAPL_FieldBundleGet => FieldBundleGet
   use mapl_FieldBundleGetByIndexImpl_mod, only: MAPL_FieldBundleGetByIndex => FieldBundleGetByIndex
   use mapl_FieldBundleGetPointerImpl_mod, only: MAPL_FieldBundleGetPointer => FieldBundleGetPointerToData
   use mapl_FieldBundleInfo_mod, only: MAPL_FieldBundleInfoGetInternal => FieldBundleInfoGetInternal
   use mapl_FieldBundleInfo_mod, only: MAPL_FieldBundleInfoSetInternal => FieldBundleInfoSetInternal
   use mapl_FieldBundleMatch_mod, only: MAPL_FieldBundleSameData => FieldBundleSameData
   use mapl_FieldBundleSetImpl_mod, only: MAPL_FieldBundleSet => FieldBundleSet

   use mapl_StateArithmeticParser_mod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   use mapl_StateAddMethodImpl_mod, only: mapl_StateAddMethod
   use mapl_StateDestroyImpl_mod, only: MAPL_StateDestroy
   use mapl_StateGetImpl_mod, only: MAPL_StateGet => StateGet
   use mapl_StateGetGeomImpl_mod, only: MAPL_StateGetGeom => StateGetGeom
   use mapl_StateGetPointerImpl_mod, only: MAPL_StateGetPointer => StateGetPointer

   use mapl_RestartModes_mod, only: MAPL_RESTART_REQUIRED, MAPL_RESTART_SKIP

   implicit none
   private

   ! Field
   public :: MAPL_FieldClone
   public :: MAPL_FieldCreate
   public :: MAPL_FieldEmptyComplete
   public :: MAPL_FieldsAreAliased
   public :: MAPL_FieldGet
   public :: MAPL_FieldSet
   public :: MAPL_FieldFill
   public :: MAPL_AssignFptr

   ! FieldBundle
   public :: MAPL_FieldBundleAdd
   public :: MAPL_FieldBundleCopy
   public :: MAPL_FieldBundleCreate
   public :: MAPL_FieldBundleDestroy
   public :: MAPL_FieldBundleGet
   public :: MAPL_FieldBundleGetByIndex
   public :: MAPL_FieldBundleGetPointer
   public :: MAPL_FieldBundleInfoGetInternal
   public :: MAPL_FieldBundleInfoSetInternal
   public :: MAPL_FieldBundleSameData
   public :: MAPL_FieldBundlesAreAliased
   public :: MAPL_FieldBundleSet

   ! State
   public :: MAPL_ParserVariablesInExpression
   public :: mapl_StateAddMethod
   public :: MAPL_StateDestroy
   public :: MAPL_StateGet
   public :: MAPL_StateGetGeom
   public :: MAPL_StateGetPointer

   ! Restart modes
   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_SKIP

end module mapl_field_export
