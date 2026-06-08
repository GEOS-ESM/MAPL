! Export umbrella for the MAPL.field_bundle library.
module mapl_field_bundle_api

   use ESMF, only: MAPL_FieldBundleAdd => ESMF_FieldBundleAdd
   use mapl_FieldBundleCopy_mod, only: MAPL_FieldBundleCopy => FieldBundleCopy
   use mapl_FieldBundleCreate_mod, only: MAPL_FieldBundleCreate => FieldBundleCreate
   use mapl_FieldBundleCreate_mod, only: MAPL_FieldBundlesAreAliased => FieldBundlesAreAliased
   use mapl_FieldBundleDestroy_mod, only: MAPL_FieldBundleDestroy
   use mapl_FieldBundleGet_mod, only: MAPL_FieldBundleGet => FieldBundleGet
   use mapl_FieldBundleGetByIndex_mod, only: MAPL_FieldBundleGetByIndex => FieldBundleGetByIndex
   use mapl_FieldBundleGetPointer_mod, only: MAPL_FieldBundleGetPointer => FieldBundleGetPointerToData
   use mapl_FieldBundleInfo_mod, only: MAPL_FieldBundleInfoGetInternal => FieldBundleInfoGetInternal
   use mapl_FieldBundleInfo_mod, only: MAPL_FieldBundleInfoSetInternal => FieldBundleInfoSetInternal
   use mapl_FieldBundleMatch_mod, only: MAPL_FieldBundleSameData => FieldBundleSameData
   use mapl_FieldBundleSet_mod, only: MAPL_FieldBundleSet => FieldBundleSet
   use mapl_FieldBundleFilter_mod, only: MAPL_FieldBundleFilter => FieldBundleFilter

   implicit none
   private

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
   public :: MAPL_FieldBundleFilter

end module mapl_field_bundle_api
