module mapl3g_AccumulatorConstants.F90

   use esmf, only :: ESMF_TypeKind_Flag, ESMF_TypeKind_R8, ESMF_TypeKind_R4
   use esmf, only :: ESMF_KIND_R8, ESMF_KIND_R4
   implicit none
   private
   public :: COUNTER_TYPEKIND
   public :: COUNTER_TYPEKIND_NAME
   public :: COUNTER_KIND

   type(ESMF_TypeKind_Flag), parameter :: COUNTER_TYPEKIND = ESMF_TypeKind_R8
   character(len=*), parameter :: COUNTER_TYPEKIND_NAME = 'ESMF_TYPEKIND_R8'
   integer(kind=ESMF_KIND_R8), parameter :: COUNTER_KIND = ESMF_KIND_R8

end module mapl3g_AccumulatorConstants.F90
