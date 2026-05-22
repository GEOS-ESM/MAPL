module mapl_LU_Bound_mod
   implicit none
   private

   public :: LU_Bound

   type :: LU_Bound
      integer :: lower
      integer :: upper
   end type LU_Bound

end module mapl_LU_Bound_mod
