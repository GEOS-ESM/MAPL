#include "MAPL_ErrLog.h"

! This module provides a family of classes that encapsulate variant
! methods of specifying/running SetServices on a user gridcomp.

! Note that the subclasses (type extensions) are themselves private to
! the module.  Client code is expected to use the overloaded factory
! procedure user_setservices() and assign the result to an object of
! the base class AbstractUserSetServices:
!
!    class(AbstractUserSetServices), allocatable :: ss
!    ss = user_setservices(...)
!

module mapl3g_UserSetServices
   use :: ESMF, only: ESMF_GridComp
   use :: ESMF, only: ESMF_GridCompSetServices
   use :: ESMF, only: ESMF_SUCCESS
   use :: mapl3g_ESMF_Interfaces, only: I_SetServices
   use :: mapl_ErrorHandling
   implicit none
   private

   public :: user_setservices        ! overloaded factory method
   public :: AbstractUserSetServices  ! Base class for variant SS functors

   type, abstract :: AbstractUserSetServices
   contains
      procedure(I_RunSetServices), deferred :: run_setservices
   end type AbstractUserSetServices

   abstract interface

      subroutine I_RunSetServices(this, gridcomp, rc)
         use esmf, only: ESMF_GridComp
         import AbstractUserSetServices
         class(AbstractUserSetServices), intent(in) :: this
         type(ESMF_GridComp) :: gridcomp
         integer, intent(out) :: rc
      end subroutine I_RunSetServices

   end interface

   ! Concrete subclass to encapsulate a traditional user setservices
   ! consisting of a procuder conforming to the I_SetServices
   ! interface.
   type, extends(AbstractUserSetServices) :: ProcSetServices
      procedure(I_SetServices), nopass, pointer :: proc_setservices
   contains
      procedure :: run_setservices => run_proc_setservices
   end type ProcSetServices

   ! Concrete subclass to encapsulate a user setservices procedure
   ! contained in a DSO.
   type, extends(AbstractUserSetServices) :: DSOSetServices
      character(:), allocatable :: sharedObj
      character(:), allocatable :: userRoutine
   contains
      procedure :: run_setservices => run_dso_setservices
   end type DSOSetServices

   interface user_setservices
      module procedure new_proc_setservices
      module procedure new_dso_setservices
   end interface user_setservices

contains

   !----------------------------------
   ! Direct procedure support

   function new_proc_setservices(setservices) result(proc_setservices)
      type(ProcSetServices) :: proc_setservices
      procedure(I_SetServices) :: setservices

      proc_setservices%proc_setservices => setservices
   end function new_proc_setservices

   subroutine run_proc_setservices(this, gridcomp, rc)
      class(ProcSetServices), intent(in) :: this
      type(ESMF_GridComp) :: gridComp
      integer, intent(out) :: rc

      integer :: status, userRC

      call ESMF_GridCompSetServices(gridcomp, this%proc_setservices, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(ESMF_SUCCESS)
   end subroutine run_proc_setservices

   !----------------------------------
   ! DSO support
   
   ! Argument names correspond to ESMF arguments.
   function new_dso_setservices(sharedObj, userRoutine) result(dso_setservices)
      type(DSOSetServices) :: dso_setservices
      character(len=*), intent(in) :: sharedObj
      character(len=*), intent(in) :: userRoutine

      dso_setservices%sharedObj   = sharedObj
      dso_setservices%userRoutine = userRoutine

   end function new_dso_setservices

   subroutine run_dso_setservices(this, gridcomp, rc)
      class(DSOSetservices), intent(in) :: this
      type(ESMF_GridComp) :: GridComp
      integer, intent(out) :: rc

      integer :: status, userRC

      call ESMF_GridCompSetServices(gridcomp, sharedObj=this%sharedObj, userRoutine=this%userRoutine, userRC=userRC,_RC)
      _VERIFY(userRC)

      _RETURN(ESMF_SUCCESS)
   end subroutine run_dso_setservices

end module mapl3g_UserSetServices
