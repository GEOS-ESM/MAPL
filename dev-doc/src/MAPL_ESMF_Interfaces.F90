! This module is a collection of abstract interfaces that enforce
! interfaces of user routines that are passed to ESMF.

module mapl_ESMF_Interfaces
   implicit none
   private ! except
   public :: I_CallBackMethod
   public :: CallbackMethodWrapper

   abstract interface
      subroutine I_CallBackMethod(state, rc)
         use ESMF
         type(ESMF_State) :: state
         integer, intent(out) :: rc
      end subroutine I_CallBackMethod
   end interface

   type CallbackMethodWrapper
      procedure(I_CallBackMethod), pointer, nopass :: userRoutine
   end type CallbackMethodWrapper

end module mapl_ESMF_Interfaces
