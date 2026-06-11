module mapl_typekind_mod
   use esmf, only: ESMF_TypeKind_Flag
   implicit none
   private

   public :: MAPL_TYPEKIND_MIRROR

   type(ESMF_TypeKind_Flag), parameter :: MAPL_TYPEKIND_MIRROR = ESMF_TypeKind_Flag(200)

end module mapl_typekind_mod
