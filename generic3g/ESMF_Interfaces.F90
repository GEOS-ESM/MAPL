!-------
! The interfaces specified here are mandated by ESMF. By providing these
! as an abstract interface,  we enable declaration of corresponding dummy procedure
! arguments elsewhere in the code in a precise and elegant manner.  E.g.,
!
!    procedure(I_SetServices) :: userRoutine
!
!-------


module mapl3g_ESMF_Interfaces
   implicit none
   private

   public :: I_SetServices
   public :: I_Run

   public :: I_CplSetServices
   public :: I_CplRun

   public :: MAPL_UserCompGetInternalState
   public :: MAPL_UserCompSetInternalState

   interface MAPL_UserCompGetInternalState
      subroutine ESMF_UserCompGetInternalState(gridcomp, name, wrapper, status)
         use ESMF, only: ESMF_GridComp
         type(*) :: gridcomp
         character(*), optional :: name
         type(*) :: wrapper
         integer :: status
      end subroutine ESMF_UserCompGetInternalState
   end interface MAPL_UserCompGetInternalState

   interface MAPL_UserCompSetInternalState
      subroutine ESMF_UserCompSetInternalState(gridcomp, name, wrapper, status)
         use ESMF, only: ESMF_GridComp
         type(*) :: gridcomp
         character(*), optional :: name
         type(*) :: wrapper
         integer :: status
      end subroutine ESMF_UserCompSetInternalState
   end interface MAPL_UserCompSetInternalState

   abstract interface

      subroutine I_SetServices(gridcomp, rc)
         use ESMF, only: ESMF_GridComp
         implicit none
         type(ESMF_GridComp)  :: gridcomp
         integer, intent(out) :: rc
      end subroutine I_SetServices

      subroutine I_Run(gridcomp, importState, exportState, clock, rc)
         use esmf, only: ESMF_GridComp
         use esmf, only: ESMF_State
         use esmf, only: ESMF_Clock
         implicit none
         type(ESMF_GridComp)   :: gridcomp
         type(ESMF_State)      :: importState
         type(ESMF_State)      :: exportState
         type(ESMF_Clock)      :: clock
         integer, intent(out)  :: rc
      end subroutine I_Run

      subroutine I_CplSetServices(cplcomp, rc)
         use ESMF, only: ESMF_CplComp
         implicit none
         type(ESMF_CplComp)   :: cplcomp
         integer, intent(out) :: rc
      end subroutine I_CplSetServices


      subroutine I_CplRun(cplcomp, importState, exportState, clock, rc)
         use :: esmf, only: ESMF_CplComp
         use :: esmf, only: ESMF_State
         use :: esmf, only: ESMF_Clock
         implicit none
         type(ESMF_CplComp)    :: cplcomp
         type(ESMF_State)      :: importState
         type(ESMF_State)      :: exportState
         type(ESMF_Clock)      :: clock
         integer, intent(out)  :: rc
      end subroutine I_CplRun

   end interface


end module mapl3g_ESMF_Interfaces
