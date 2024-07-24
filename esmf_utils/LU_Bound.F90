module mapl3g_LU_Bound
   implicit none
   private

   public :: LU_Bound

   type :: LU_Bound
      integer :: lower
      integer :: upper
   end type LU_Bound

end module mapl3g_LU_Bound
