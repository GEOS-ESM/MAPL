#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) run_user_threaded_smod
   use mapl_OpenMP_Support_mod, only: get_current_thread
   use mapl_ErrorHandling_mod
   !$ use omp_lib, only: omp_get_num_threads
   implicit none(type,external)

   ! Sentinel used to detect sub components that were never run because
   ! the OpenMP runtime provided a smaller team than was requested.
   integer, parameter :: NOT_RUN = -1

contains

   ! Run the user component concurrently on `num_threads` OpenMP threads.
   ! Each thread runs its own "mini" gridcomp on a subset of the geometry
   ! of this component.  This is the MAPL3 analog of the MAPL2 omp_driver.
   module recursive subroutine run_user_threaded(this, phase_idx, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, intent(in) :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: thread
      integer :: num_threads
      integer :: team_size
      type(ESMF_Clock) :: clock
      type(ESMF_GridComp) :: thread_gc
      type(MultiState) :: thread_states
      type(OuterMetaComponent), pointer :: meta
      integer, allocatable :: statuses(:), user_statuses(:)

      num_threads = this%get_num_threads()
      call this%activate_threading(num_threads, _RC)

      clock = this%user_gc_driver%get_clock()

      ! Initialized to a sentinel so that a sub component which is never
      ! run (e.g. because the runtime reduced the team size) is detected
      ! rather than silently reported as successful.
      allocate(statuses(num_threads), source=NOT_RUN, __STAT__)
      allocate(user_statuses(num_threads), source=ESMF_SUCCESS, __STAT__)
      team_size = num_threads

      ! OpenMP data sharing clauses do not accept polymorphic entities on
      ! all supported compilers, hence the local pointer.
      select type (this)
      type is (OuterMetaComponent)
         meta => this
      class default
         _FAIL('unsupported extension of OuterMetaComponent')
      end select

      !$omp parallel default(none), &
      !$omp& private(thread, thread_states, thread_gc), &
      !$omp& shared(meta, statuses, user_statuses, clock, phase_idx, team_size), &
      !$omp& num_threads(num_threads)

      thread = get_current_thread()
      !$ if (thread == 0) team_size = omp_get_num_threads()

      statuses(thread+1) = ESMF_SUCCESS
      thread_states = meta%get_thread_states(rc=statuses(thread+1))
      if (statuses(thread+1) == ESMF_SUCCESS) then
         thread_gc = meta%get_thread_gridcomp(rc=statuses(thread+1))
      end if

      if (statuses(thread+1) == ESMF_SUCCESS) then
         call ESMF_GridCompRun(thread_gc, &
              importState=thread_states%importState, &
              exportState=thread_states%exportState, &
              clock=clock, phase=phase_idx, &
              userRC=user_statuses(thread+1), rc=statuses(thread+1))
      end if
      !$omp end parallel

      call this%deactivate_threading(_RC)

      _ASSERT(team_size == num_threads, 'OpenMP provided fewer threads than the component requested')
      _ASSERT(all(statuses /= NOT_RUN), 'at least one sub component was never run')
      _ASSERT(all(user_statuses == ESMF_SUCCESS), 'user component failed on at least one thread')
      _ASSERT(all(statuses == ESMF_SUCCESS), 'threaded run failed on at least one thread')

      _RETURN(_SUCCESS)
   end subroutine run_user_threaded

end submodule run_user_threaded_smod
