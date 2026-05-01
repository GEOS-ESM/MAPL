module mapl3g_FieldBundle_API

   use ESMF, only: MAPL_FieldBundleAdd => ESMF_FieldBundleAdd
   use mapl3g_FieldBundleType_Flag
   use mapl3g_VectorBasisKind
   use mapl3g_FieldBundleCreate, only: MAPL_FieldBundleCreate => FieldBundleCreate
   use mapl3g_FieldBundleCreate, only: MAPL_FieldBundlesAreAliased => FieldBundlesAreAliased
   use mapl3g_FieldBundleGet, only: MAPL_FieldBundleGet => FieldBundleGet
   use mapl3g_FieldBundleSet, only: MAPL_FieldBundleSet => FieldBundleSet
   use mapl3g_FieldBundleInfo, only: MAPL_FieldBundleInfoGetInternal => FieldBundleInfoGetInternal
   use mapl3g_FieldBundleInfo, only: MAPL_FieldBundleInfoSetInternal => FieldBundleInfoSetInternal
   use mapl3g_FieldBundleGetPointer, only: MAPL_FieldBundleGetPointer => FieldBundleGetPointerToData
   use mapl3g_FieldBundleCopy, only: MAPL_FieldBundleCopy => FieldBundleCopy
   use mapl3g_FieldBundleMatch, only: MAPL_FieldBundleSameData => FieldBundleSameData

   implicit none

   private

   ! Available to users
   public :: MAPL_FieldBundleCreate
   public :: MAPL_FieldBundlesAreAliased
   public :: MAPL_FieldBundleGet
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

end module mapl3g_FieldBundle_API
