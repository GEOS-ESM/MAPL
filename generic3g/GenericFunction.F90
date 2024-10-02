module mapl3g_accumulation_functions

   use, esmf, only: ESMF_KIND_R8, ESMF_KIND_R4
   implicit none
   private

   public :: id_R8
   public :: add_R8
   public :: mean_R8
   public :: max_R8
   public :: min_R8
   public :: id_R4
   public :: add_R4
   public :: mean_R4
   public :: max_R4
   public :: min_R4
   public :: set_R4
   public :: set_R8
   
   type, abstract :: FuncObject
   contains
      generic :: eval => evalR4!, evalR8
      procedure(R4Function), deferred :: evalR4
!      procedure(R8Function), deferred :: evalR8
   end type FuncObject

   abstract interface
      elemental function R4Function(this, t) result(ft)
         real(ESMF_KIND_R4) :: ft
         class(FuncObject), intent(in) :: this
         real(ESMF_KIND_R4), intent(in) :: t
      end function R4Function
   end interface

   type, abstract :: BinaryFuncObject
   contains
      generic :: eval => evalR4!, evalR8
!      procedure(R8BinaryFunction), deferred :: evalR8
      procedure(R4BinaryFunction), deferred :: evalR4
   end type BinaryFuncObject

   abstract interface
      elemental function R4BinaryFunction(this, t1, t2) result(ft)
         real(ESMF_KIND_R4) :: ft
         class(BinaryFuncObject), intent(in) :: this
         real(ESMF_KIND_R4), intent(in) :: t1, t2
      end function R4BinaryFunction
!      elemental function R8BinaryFunction(this, t1, t2) result(ft)
!         real(ESMF_KIND_R8) :: ft
!         class(BinaryFuncObject), intent(in) :: this
!         real(ESMF_KIND_R8), intent(in) :: t1, t2
!      end function R8BinaryFunction
   end interface

contains

   
      
end module mapl3g_accumulation_functions
