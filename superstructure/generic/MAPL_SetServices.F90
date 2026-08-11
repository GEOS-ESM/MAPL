#include "MAPL.h"

! This module provides a family of classes that encapsulate variant
! methods of specifying/running SetServices on a user gridcomp.

! Note that the subclasses (type extensions) are themselves private to
! the module.  Client code is expected to use the overloaded factory
! procedure ProcSetServices() or DsoSetServices() and assign the result to an object of
! the base class MAPL_SetServices:
!
!    class(MAPL_SetServices), allocatable :: ss
!    ss = ProcSetServices(...)
!

module mapl_SetServices_mod
   use :: ESMF, only: ESMF_GridComp
   use :: ESMF, only: ESMF_GridCompSetServices
   use :: ESMF, only: ESMF_SUCCESS
   use :: mapl_ESMF_Interfaces_mod, only: I_SetServices
   use :: mapl_ErrorHandling_mod
   implicit none(type,external)
   private

   public :: MAPL_SetServices  ! Base class for variant SS functors
   public :: ProcSetServices
   public :: DsoSetServices
   public :: operator(==)
   public :: operator(/=)
   
   type, abstract :: MAPL_SetServices
   contains
      procedure(I_RunSetServices), deferred :: run
      procedure(I_write_formatted), deferred :: write_formatted
      generic :: write(formatted) => write_formatted
   end type MAPL_SetServices

   abstract interface

      subroutine I_RunSetServices(this, gridcomp, rc)
         use esmf, only: ESMF_GridComp
         import MAPL_SetServices
         class(MAPL_SetServices), intent(in) :: this
         type(ESMF_GridComp) :: gridcomp
         integer, intent(out) :: rc
      end subroutine I_RunSetServices

      subroutine I_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         import MAPL_SetServices
         class(MAPL_SetServices), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine I_write_formatted

   end interface

   ! Concrete subclass to encapsulate a traditional user setservices
   ! consisting of a procedure conforming to the I_SetServices
   ! interface.
   type, extends(MAPL_SetServices) :: ProcSetServices
      procedure(I_SetServices), nopass, pointer :: userRoutine ! ESMF naming convention
   contains
      procedure :: run => run_ProcSetServices
      procedure :: write_formatted => write_formatted_proc
   end type ProcSetServices

   ! Concrete subclass to encapsulate a user setservices procedure
   ! contained in a DSO.
   type, extends(MAPL_SetServices) :: DsoSetServices
      character(:), allocatable :: sharedObj    ! ESMF naming convention
      character(:), allocatable :: userRoutine  ! ESMF naming convention
   contains
      procedure :: run => run_DsoSetServices
      procedure :: write_formatted => write_formatted_dso
   end type DsoSetServices

   interface ProcSetServices
      module procedure new_ProcSetServices
   end interface ProcSetServices

   interface DsoSetServices
      module procedure new_DsoSetServices
   end interface DsoSetServices

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
   function new_DsoSetServices(sharedObj, userRoutine) result(dso_setservices)
      use mapl_DSO_Utilities_mod
      type(DsoSetServices) :: dso_setservices
      character(len=*), intent(in) :: sharedObj
      character(len=*), optional, intent(in) :: userRoutine

      character(:), allocatable :: userRoutine_

      userRoutine_ = 'setservices_' ! unless
      if (present(userRoutine)) userRoutine_ = userRoutine
         
      dso_setservices%sharedObj   = sharedObj
      dso_setservices%userRoutine = userRoutine_

   end function new_DsoSetServices

   subroutine run_DsoSetServices(this, gridcomp, rc)
      use mapl_DSO_Utilities_mod
      class(DsoSetServices), intent(in) :: this
      type(ESMF_GridComp) :: GridComp
      integer, intent(out) :: rc

      integer :: status, user_status
      logical :: found

      _ASSERT(is_supported_dso_name(this%sharedObj), 'unsupported dso name:: <'//this%sharedObj//'>')
      call ESMF_GridCompSetServices(gridcomp, sharedObj=adjust_dso_name(this%sharedObj), &
           userRoutine=this%userRoutine, userRoutinefound=found, _USERRC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine run_DsoSetServices

   subroutine write_formatted_dso(this, unit, iotype, v_list, iostat, iomsg)
      class(DsoSetServices), intent(in) :: this
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
      class(MAPL_SetServices), intent(in) :: a, b

      select type (a)
      type is (DsoSetServices)
         select type(b)
         type is (DsoSetServices)
            equal = equal_DsoSetServices(a,b)
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
      class(MAPL_SetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_setServices

   logical function equal_ProcSetServices(a, b) result(equal)
      type(ProcSetServices), intent(in) :: a, b
      equal = associated(a%userRoutine, b%userRoutine)
   end function equal_ProcSetServices

   logical function equal_DsoSetServices(a, b) result(equal)
      type(DsoSetServices), intent(in) :: a, b
      
      equal = (a%sharedObj == b%sharedObj) .and. (a%userRoutine == b%userRoutine)
   end function equal_DsoSetServices

   logical function not_equal_ProcSetServices(a, b) result(not_equal)
      type(ProcSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_ProcSetServices

   logical function not_equal_DsoSetServices(a, b) result(not_equal)
      type(DsoSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_DsoSetServices
   

   
end module mapl_SetServices_mod
