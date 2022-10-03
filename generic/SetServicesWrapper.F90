#include "MAPL_ErrLog.h"
module mapl_SetServicesWrapper
   use ESMF
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: AbstractSetServicesWrapper
   public :: DSO_SetServicesWrapper
   public :: ProcSetServicesWrapper

   type, abstract :: AbstractSetServicesWrapper
   contains
      procedure(I_Run), deferred :: run
   end type AbstractSetServicesWrapper

   type, extends(AbstractSetServicesWrapper) :: DSO_SetServicesWrapper
      private
      character(:), allocatable :: sharedObj
      character(:), allocatable :: userRoutine
   contains
      procedure :: run => run_dso
   end type DSO_SetServicesWrapper

   type, extends(AbstractSetServicesWrapper) :: ProcSetServicesWrapper
      procedure(I_SetServices), nopass, pointer :: userRoutine
   contains
      procedure :: run => run_proc
   end type ProcSetServicesWrapper

   abstract interface
      subroutine I_Run(this, gc, unusable, rc)
         use ESMF
         use MAPL_KeywordEnforcerMod
         import AbstractSetServicesWrapper
         class(AbstractSetServicesWrapper), intent(in) :: this
         type(ESMF_GridComp), intent(inout) :: gc
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine I_Run

      subroutine I_SetServices(gc, rc)
         use ESMF
         type(ESMF_GridComp) :: gc
         integer, intent(out) :: rc
      end subroutine I_SetServices

   end interface

   interface DSO_SetServicesWrapper
      module procedure new_dso
   end interface DSO_SetServicesWrapper

contains

   function new_dso(sharedObj, userRoutine) result(this)
      type(DSO_SetServicesWrapper) :: this
      character(len=*), intent(in) :: sharedObj
      character(len=*), intent(in) :: userRoutine

      this%sharedObj = sharedObj
      this%userRoutine = userRoutine
   end function new_dso

   recursive subroutine run_dso(this, gc, unusable, rc)
      class(DSO_SetServicesWrapper), intent(in) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      call ESMF_GridCompSetServices(gc, trim(this%userRoutine), sharedObj=trim(this%sharedObj), userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_dso

   recursive subroutine run_proc(this, gc, unusable, rc)
      class(ProcSetServicesWrapper), intent(in) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, userRC

      call ESMF_GridCompSetServices(gc, this%userRoutine, userRC=userRC, _RC)
      _VERIFY(userRC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_proc

end module mapl_SetServicesWrapper
