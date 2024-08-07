#include "MAPL_Generic.h"

module mapl3g_BundleAction
   use mapl3g_ExtensionAction
   use mapl3g_ActionVector
   use mapl_ErrorHandling
   implicit none
   private

   public :: BundleAction

   type, extends(ExtensionAction) :: BundleAction
      private
      type(ActionVector) :: actions
   contains
      procedure :: initialize
      procedure :: run_old
      procedure :: run_new
      procedure :: add_action
   end type BundleAction

   interface BundleAction
      procedure new_BundleAction
   end interface BundleAction

contains

   function new_BundleAction() result(action)
      type(BundleAction) :: action
      action%actions = ActionVector()
   end function new_BundleAction

   ! BundleAction may not make sense with a shared import/export state.
   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(BundleAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActionVectorIterator) :: iter

!#      associate (e => this%actions%ftn_end())
!#        iter = this%actions%ftn_begin()
!#        do while (iter /= e)
!#           call iter%next()
!#           subaction => iter%of()
!#           call subaction%initialize(importState, exportState, clock, _RC)
!#        end do
!#      end associate
      _FAIL('Not implemented')
   end subroutine initialize

   subroutine run_old(this, rc)
      class(BundleAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: i
      integer :: status
      class(ExtensionAction), pointer :: action
      
      do i = 1, this%actions%size()
         action => this%actions%of(i)
         call action%run(_RC)
      end do
      
      _RETURN(_SUCCESS)
   end subroutine run_old
   
   ! BundleAction may not make sense with a shared import/export state.
   subroutine run_new(this, importState, exportState, clock, rc)
      use esmf
      class(BundleAction), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActionVectorIterator) :: iter

!#      associate (e => this%actions%ftn_end())
!#        iter = this%actions%ftn_begin()
!#        do while (iter /= e)
!#           call iter%next()
!#           subaction => iter%of()
!#           call subaction%initialize(importState, exportState, clock, _RC)
!#        end do
!#      end associate
      _FAIL('Not implemented')
   end subroutine run_new

   subroutine add_action(this, action)
      class(BundleAction), intent(inout) :: this
      class(ExtensionAction), intent(in) :: action

      call this%actions%push_back(action)
   end subroutine add_action
   
end module mapl3g_BundleAction
