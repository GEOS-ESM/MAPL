! This module is a collection of abstract interfaces that enforce
! interfaces of user routines that are passed to ESMF.  MAPL3 has
! several of these for ... SetServices, GridCompRun, etc.
module mapl_ESMF_Interfaces
   public :: I_CallBackMethod
   public :: CallbackMethodWrapper

   abstract interface
      subroutine I_CallBackMethod(state, rc)
         use ESMF
         type(ESMF_State), intent(inout) :: state
         integer, optional, intent(out) :: rc
      end subroutine I_CallBackMethod
   end interface

   type CallbackMethodWrapper
      procedure(I_CallBackMethod), pointer, nopass :: userRoutine
   end type CallbackMethodWrapper

end module mapl_ESMF_Interfaces
