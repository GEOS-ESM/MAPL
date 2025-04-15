module mapl3g_FieldBundle_API

   use mapl3g_FieldBundleGet, only: MAPL_FieldBundleGet
   use mapl3g_FieldBundleGet, only: MAPL_FieldBundleSet
   use mapl3g_FieldBundleCreate, only: MAPL_FieldBundleCreate
   use mapl3g_FieldBundleInfo, only: MAPL_FieldBundleInfoGetInternal
   use mapl3g_FieldBundleInfo, only: MAPL_FieldBundleInfoSetInternal

   implicit none

   private

   ! Available to users
   public :: MAPL_FieldBundleCreate
   public :: MAPL_FieldBundleGet
   public :: MAPL_FieldBundleSet
   public :: MAPL_FieldBundleInfoGetInternal
   public :: MAPL_FieldBundleInfoSetInternal

   ! Used internally by MAPL
   ! Users shouldn't need these

end module mapl3g_FieldBundle_API
