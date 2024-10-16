module Kernel_mod
   implicit none (type, external)
   public :: Kernel_T

   type, abstract :: Kernel_T
   contains
      procedure(I_Run), deferred :: run
   end type Kernel_T

   abstract interface
      subroutine I_Run(this, rc)
         use mapl_ErrorHandlingMod
         import Kernel_T
         class(Kernel_T), intent(in) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_Run
   end interface

end module Kernel_mod
