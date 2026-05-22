module mapl_FieldBundle_API_mod

   use ESMF, only: MAPL_FieldBundleAdd => ESMF_FieldBundleAdd
   use mapl_FieldBundleType_Flag_mod
   use mapl_VectorBasisKind_mod
   use mapl_FieldBundleCreateImpl_mod, only: MAPL_FieldBundleCreate => FieldBundleCreate
   use mapl_FieldBundleCreateImpl_mod, only: MAPL_FieldBundlesAreAliased => FieldBundlesAreAliased
   use mapl_FieldBundleDestroyImpl_mod, only: MAPL_FieldBundleDestroy
   use mapl_FieldBundleGetImpl_mod, only: MAPL_FieldBundleGet => FieldBundleGet
   use mapl_FieldBundleGetByIndexImpl_mod, only: MAPL_FieldBundleGetByIndex => FieldBundleGetByIndex
   use mapl_FieldBundleSetImpl_mod, only: MAPL_FieldBundleSet => FieldBundleSet
   use mapl_FieldBundleInfo_mod, only: MAPL_FieldBundleInfoGetInternal => FieldBundleInfoGetInternal
   use mapl_FieldBundleInfo_mod, only: MAPL_FieldBundleInfoSetInternal => FieldBundleInfoSetInternal
   use mapl_FieldBundleGetPointerImpl_mod, only: MAPL_FieldBundleGetPointer => FieldBundleGetPointerToData
   use mapl_FieldBundleCopyImpl_mod, only: MAPL_FieldBundleCopy => FieldBundleCopy
   use mapl_FieldBundleMatch_mod, only: MAPL_FieldBundleSameData => FieldBundleSameData

   implicit none

   private

   ! Available to users
   public :: MAPL_FieldBundleCreate
   public :: MAPL_FieldBundlesAreAliased
   public :: MAPL_FieldBundleDestroy
   public :: MAPL_FieldBundleGet
   public :: MAPL_FieldBundleGetByIndex
   public :: MAPL_FieldBundleSet
   public :: MAPL_FieldBundleAdd
   public :: MAPL_FieldBundleGetPointer
   public :: MAPL_FieldBundleCopy
   public :: MAPL_FieldBundleSameData
   ! Maybe these should be private?
   public :: MAPL_FieldBundleInfoGetInternal
   public :: MAPL_FieldBundleInfoSetInternal

   public :: FieldBundleType_Flag
   public :: FIELDBUNDLETYPE_INVALID
   public :: FIELDBUNDLETYPE_BASIC
   public :: FIELDBUNDLETYPE_VECTOR
   public :: FIELDBUNDLETYPE_BRACKET
   public :: FIELDBUNDLETYPE_VECTORBRACKET

   public :: operator(==)
   public :: operator(/=)

   ! VectorBasisKind
   public :: VectorBasisKind
   public :: VECTOR_BASIS_KIND_INVALID
   public :: VECTOR_BASIS_KIND_GRID
   public :: VECTOR_BASIS_KIND_NS

   ! Used internally by MAPL
   ! Users shouldn't need these

end module mapl_FieldBundle_API_mod
