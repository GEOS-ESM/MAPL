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
   public :: DSOSetServices
   public :: operator(==)
   public :: operator(/=)
   
   type, abstract :: AbstractUserSetServices
   contains
      procedure(I_RunSetServices), deferred :: run
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
      procedure(I_SetServices), nopass, pointer :: userRoutine
   contains
      procedure :: run => run_proc_setservices
   end type ProcSetServices

   ! Concrete subclass to encapsulate a user setservices procedure
   ! contained in a DSO.
   type, extends(AbstractUserSetServices) :: DSOSetServices
      character(:), allocatable :: sharedObj
      character(:), allocatable :: userRoutine
   contains
      procedure :: run => run_dso_setservices
   end type DSOSetServices

   interface user_setservices
      module procedure new_proc_setservices
      module procedure new_dso_setservices
   end interface user_setservices

   interface operator(==)
      module procedure equal_ProcSetServices
      module procedure equal_DSOSetServices
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_ProcSetServices
      module procedure not_equal_DSOSetServices
   end interface operator(/=)

contains

   !----------------------------------
   ! Direct procedure support

   function new_proc_setservices(userRoutine) result(proc_setservices)
      type(ProcSetServices) :: proc_setservices
      procedure(I_SetServices) :: userRoutine

      proc_setservices%userRoutine => userRoutine
   end function new_proc_setservices

   subroutine run_proc_setservices(this, gridcomp, rc)
      class(ProcSetServices), intent(in) :: this
      type(ESMF_GridComp) :: gridComp
      integer, intent(out) :: rc

      integer :: status, userRC

      call ESMF_GridCompSetServices(gridcomp, this%userRoutine, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(ESMF_SUCCESS)
   end subroutine run_proc_setservices

   !----------------------------------
   ! DSO support
   
   ! Argument names correspond to ESMF arguments.
   function new_dso_setservices(sharedObj, userRoutine) result(dso_setservices)
      use mapl_DSO_Utilities
      type(DSOSetServices) :: dso_setservices
      character(len=*), intent(in) :: sharedObj
      character(len=*), intent(in) :: userRoutine

      dso_setservices%sharedObj   = sharedObj
      dso_setservices%userRoutine = userRoutine

   end function new_dso_setservices

   subroutine run_dso_setservices(this, gridcomp, rc)
      use mapl_DSO_Utilities
      class(DSOSetservices), intent(in) :: this
      type(ESMF_GridComp) :: GridComp
      integer, intent(out) :: rc

      integer :: status, userRC
      logical :: found

      _ASSERT(is_supported_dso_name(this%sharedObj), 'unsupported dso name:: <'//this%sharedObj//'>')
      print*,__FILE__,__LINE__, adjust_dso_name(this%sharedObj), ' ', this%userRoutine
      call ESMF_GridCompSetServices(gridcomp, sharedObj=adjust_dso_name(this%sharedObj), &
           userRoutine=this%userRoutine, userRoutinefound=found, userRC=userRC, rc=status)

      _VERIFY(userRC)
      _VERIFY(rc)

      _RETURN(ESMF_SUCCESS)
   end subroutine run_dso_setservices


   pure logical function equal_ProcSetServices(a, b) result(equal)
      type(ProcSetServices), intent(in) :: a, b
      equal = associated(a%userRoutine, b%userRoutine)
   end function equal_ProcSetServices

   pure logical function equal_DSOSetServices(a, b) result(equal)
      type(DSOSetServices), intent(in) :: a, b
      
      equal = (a%sharedObj == b%sharedObj) .and. (a%userRoutine == b%userRoutine)
   end function equal_DSOSetServices

   pure logical function not_equal_ProcSetServices(a, b) result(not_equal)
      type(ProcSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_ProcSetServices

   pure logical function not_equal_DSOSetServices(a, b) result(not_equal)
      type(DSOSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_DSOSetServices
   

   
end module mapl3g_UserSetServices
