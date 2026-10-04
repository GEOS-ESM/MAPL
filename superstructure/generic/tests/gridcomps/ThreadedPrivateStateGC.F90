#include "MAPL.h"

! A minimal MAPL3 user gridcomp used by Test_Threading.pf to verify that
! named private state set in SetServices is reachable from every thread's
! mini gridcomp during a threaded run, and that all threads share the same
! object (i.e., a write from thread 0 is visible to every other thread).
!
! Design
! ------
! * SetServices allocates ThreadedPrivateState%slot(0:N-1) where N is the
!   number of threads, and sets slot(i) = -1 for all i.
! * Each Run invocation reads the thread index via mapl_get_current_thread,
!   looks up the private state via _GET_NAMED_PRIVATE_STATE, and writes
!   the thread index into slot(thread).
! * After the threaded run the test verifies:
!   (a) every slot has been written (reachability), and
!   (b) all slots are visible through the primary user gridcomp's private
!       state (sharing: mapl_get_owning_gridcomp redirected all mini-gc
!       accesses to the same object).

module ThreadedPrivateStateGC
   use mapl_OwningGridComp_mod, only: mapl_get_owning_gridcomp
   use mapl_ErrorHandling_mod
   use mapl_Generic_api, only: mapl_GridCompSetEntryPoint, mapl_get_current_thread
   use esmf
   implicit none(type,external)
   private

   public :: setservices
   public :: ThreadedPrivateState
   public :: ThreadedPrivateStateWrapper
   public :: PRIVATE_STATE_LABEL

   character(*), parameter :: PRIVATE_STATE_LABEL = 'ThreadedPrivateState'

   type :: ThreadedPrivateState
      integer, allocatable :: slot(:)  ! indexed 0:num_threads-1
   end type ThreadedPrivateState

   ! Wrapper matching _DECLARE_WRAPPER(ThreadedPrivateState): one pointer
   ! component named ptr.  Exported so the test can retrieve the private state
   ! without type punning.
   type :: ThreadedPrivateStateWrapper
      type(ThreadedPrivateState), pointer :: ptr
   end type ThreadedPrivateStateWrapper

contains

   subroutine setservices(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status
      type(ThreadedPrivateState), pointer :: state

      call mapl_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Allocate private state with enough slots for up to 8 threads.
      ! The test configures num_threads=2 so slots 0 and 1 are exercised.
      _SET_NAMED_PRIVATE_STATE(gc, ThreadedPrivateState, PRIVATE_STATE_LABEL)
      _GET_NAMED_PRIVATE_STATE(gc, ThreadedPrivateState, PRIVATE_STATE_LABEL, state)
      allocate(state%slot(0:7), source=-1)

      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      integer :: thread
      type(ThreadedPrivateState), pointer :: state

      thread = mapl_get_current_thread()

      ! This call may receive a mini gridcomp (gc) during a threaded run.
      ! mapl_get_owning_gridcomp (invoked inside the macro) redirects to
      ! the primary user gridcomp so all threads share one state object.
      _GET_NAMED_PRIVATE_STATE(gc, ThreadedPrivateState, PRIVATE_STATE_LABEL, state)

      state%slot(thread) = thread

      _RETURN(ESMF_SUCCESS)
   end subroutine run

end module ThreadedPrivateStateGC

! External entry point required by DsoSetServices.
subroutine setservices(gc, rc)
   use ThreadedPrivateStateGC, only: ThreadedPrivateStateGC_setservices => setservices
   use esmf, only: ESMF_GridComp
   implicit none
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc
   call ThreadedPrivateStateGC_setservices(gc, rc)
end subroutine setservices
