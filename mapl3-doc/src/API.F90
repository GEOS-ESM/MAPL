module mapl3g_FieldBundle_API

   use mapl3g_FieldBundleType_Flag
   use mapl3g_FieldBundleCreate, only: MAPL_FieldBundleCreate => FieldBundleCreate
   use mapl3g_FieldBundleGet, only: MAPL_FieldBundleGet => FieldBundleGet
   use mapl3g_FieldBundleSet, only: MAPL_FieldBundleSet => FieldBundleSet
   use mapl3g_FieldBundleInfo, only: MAPL_FieldBundleInfoGetInternal => FieldBundleInfoGetInternal
   use mapl3g_FieldBundleInfo, only: MAPL_FieldBundleInfoSetInternal => FieldBundleInfoSetInternal

   implicit none

   private

   ! Available to users
   public :: MAPL_FieldBundleCreate
   public :: MAPL_FieldBundleGet
   public :: MAPL_FieldBundleSet
   ! Maybe these should be private?
   public :: MAPL_FieldBundleInfoGetInternal
   public :: MAPL_FieldBundleInfoSetInternal

   public :: FieldBundleType_Flag
   public :: FIELDBUNDLETYPE_INVALID
   public :: FIELDBUNDLETYPE_BASIC
   public :: FIELDBUNDLETYPE_VECTOR
   public :: FIELDBUNDLETYPE_BRACKET

   public :: operator(==)
   public :: operator(/=)

   ! Used internally by MAPL
   ! Users shouldn't need these

end module mapl3g_FieldBundle_API
