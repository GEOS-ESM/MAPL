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
      procedure(I_write_formatted), deferred :: write_formatted
      generic :: write(formatted) => write_formatted
   end type AbstractUserSetServices

   abstract interface

      subroutine I_RunSetServices(this, gridcomp, rc)
         use esmf, only: ESMF_GridComp
         import AbstractUserSetServices
         class(AbstractUserSetServices), intent(in) :: this
         type(ESMF_GridComp) :: gridcomp
         integer, intent(out) :: rc
      end subroutine I_RunSetServices

      subroutine I_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         import AbstractUserSetServices
         class(AbstractUserSetServices), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine I_write_formatted

   end interface

   ! Concrete subclass to encapsulate a traditional user setservices
   ! consisting of a procuder conforming to the I_SetServices
   ! interface.
   type, extends(AbstractUserSetServices) :: ProcSetServices
      procedure(I_SetServices), nopass, pointer :: userRoutine ! ESMF naming convention
   contains
      procedure :: run => run_ProcSetServices
      procedure :: write_formatted => write_formatted_proc
   end type ProcSetServices

   ! Concrete subclass to encapsulate a user setservices procedure
   ! contained in a DSO.
   type, extends(AbstractUserSetServices) :: DSOSetServices
      character(:), allocatable :: sharedObj    ! ESMF naming convention
      character(:), allocatable :: userRoutine  ! ESMF naming convention
   contains
      procedure :: run => run_DSOSetServices
      procedure :: write_formatted => write_formatted_dso
   end type DSOSetServices

   interface user_setservices
      module procedure new_ProcSetServices
      module procedure new_DSOSetservices
   end interface user_setservices

   interface operator(==)
      module procedure equal_setServices
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_setServices
   end interface operator(/=)

contains

   !----------------------------------
   ! Direct procedure support

   function new_ProcSetServices(userRoutine) result(proc_setservices)
      type(ProcSetServices) :: proc_setservices
      procedure(I_SetServices) :: userRoutine

      proc_setservices%userRoutine => userRoutine

   end function new_ProcSetServices

   subroutine run_ProcSetServices(this, gridcomp, rc)
      class(ProcSetServices), intent(in) :: this
      type(ESMF_GridComp) :: gridComp
      integer, intent(out) :: rc

      integer :: status, user_status

      call ESMF_GridCompSetServices(gridcomp, this%userRoutine, _USERRC)

      _RETURN(ESMF_SUCCESS)
   end subroutine run_ProcSetServices

   subroutine write_formatted_proc(this, unit, iotype, v_list, iostat, iomsg)
      class(ProcSetServices), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit,*,iostat=iostat, iomsg=iomsg) "userRoutine: <procedure>"
      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
      _UNUSED_DUMMY(this)
   end subroutine write_formatted_proc

   !----------------------------------
   ! DSO support
   
   ! Argument names correspond to ESMF arguments.
   function new_DSOSetServices(sharedObj, userRoutine) result(dso_setservices)
      use mapl_DSO_Utilities
      type(DSOSetServices) :: dso_setservices
      character(len=*), intent(in) :: sharedObj
      character(len=*), optional, intent(in) :: userRoutine

      character(:), allocatable :: userRoutine_

      userRoutine_ = 'setservices_' ! unless
      if (present(userRoutine)) userRoutine_ = userRoutine
         
      dso_setservices%sharedObj   = sharedObj
      dso_setservices%userRoutine = userRoutine_

   end function new_DSOSetServices

   subroutine run_DSOSetServices(this, gridcomp, rc)
      use mapl_DSO_Utilities
      class(DSOSetservices), intent(in) :: this
      type(ESMF_GridComp) :: GridComp
      integer, intent(out) :: rc

      integer :: status, user_status
      logical :: found

      _ASSERT(is_supported_dso_name(this%sharedObj), 'unsupported dso name:: <'//this%sharedObj//'>')
      call ESMF_GridCompSetServices(gridcomp, sharedObj=adjust_dso_name(this%sharedObj), &
           userRoutine=this%userRoutine, userRoutinefound=found, _USERRC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine run_DSOSetServices

   subroutine write_formatted_dso(this, unit, iotype, v_list, iostat, iomsg)
      class(DSOSetServices), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit,*,iostat=iostat, iomsg=iomsg) "sharedObj: ", this%sharedObj
      if (iostat /= 0) return
      write(unit,*,iostat=iostat, iomsg=iomsg) "userRoutine: ", this%userRoutine

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted_dso

   logical function equal_setServices(a, b) result(equal)
      class(AbstractUserSetServices), intent(in) :: a, b

      select type (a)
      type is (DSOSetServices)
         select type(b)
         type is (DSOSetServices)
            equal = equal_DSOSetServices(a,b)
         class default
            equal = .false.
         end select
      type is (ProcSetServices)
         select type(b)
         type is (ProcSetservices)
            equal = equal_ProcSetServices(a,b)
         class default
            equal = .false.
         end select
      class default
         equal = .false.
      end select

   end function equal_setServices

   logical function not_equal_setServices(a, b) result(not_equal)
      class(AbstractUserSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_setServices

   logical function equal_ProcSetServices(a, b) result(equal)
      type(ProcSetServices), intent(in) :: a, b
      equal = associated(a%userRoutine, b%userRoutine)
   end function equal_ProcSetServices

   logical function equal_DSOSetServices(a, b) result(equal)
      type(DSOSetServices), intent(in) :: a, b
      
      equal = (a%sharedObj == b%sharedObj) .and. (a%userRoutine == b%userRoutine)
   end function equal_DSOSetServices

   logical function not_equal_ProcSetServices(a, b) result(not_equal)
      type(ProcSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_ProcSetServices

   logical function not_equal_DSOSetServices(a, b) result(not_equal)
      type(DSOSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_DSOSetServices
   

   
end module mapl3g_UserSetServices
