#include "MAPL_ErrLog.h"

module mapl3g_GenericCouplerComponent
   use :: esmf, only: ESMF_CplComp
   use :: esmf, only: ESMF_CplCompRun
   use :: esmf, only: ESMF_State
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_SUCCESS
   use :: mapl3g_ChildComponent
   use :: mapl_ErrorHandling
   implicit none
   private

   public :: GenericCouplerComponent


   type :: GenericCouplerComponent
      type(ESMF_CplComp) :: cplcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
   contains
      procedure, private :: run_self
      generic :: run => run_self
   end type GenericCouplerComponent

contains

   subroutine SetServices(cplcomp, rc)
      type(ESMF_CplComp) :: cplcomp
      integer, intent(out) :: rc
   end subroutine SetServices

   subroutine run_self(this, clock, rc)
      class(GenericCouplerComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_CplCompRun(this%cplcomp, &
           importState=this%importState, exportState=this%exportState, &
           clock=clock, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine run_self

end module mapl3g_GenericCouplerComponent
