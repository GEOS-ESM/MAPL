#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) run_user_smod

   use mapl_ComponentDriver_mod
   use mapl_ComponentDriverPtrVector_mod
   use mapl_enums_api, only: MAPL_GENERIC_COUPLER_INVALIDATE, MAPL_GENERIC_COUPLER_UPDATE
   use mapl_ErrorHandling_mod
   use pflogger, only: logger_t => logger

   implicit none(type,external)

contains

   module recursive subroutine run_user(this, clock, phase_name, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      ! optional arguments
      character(len=*), optional, intent(in) :: phase_name
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StringVector), pointer :: run_phases
      logical :: found
      class(logger_t), pointer :: logger
      integer :: phase, status

      type(ComponentDriverPtrVector) :: export_Couplers
      type(ComponentDriverPtrVector) :: import_Couplers
      type(ComponentDriverPtr) :: drvr
      integer :: i
      type(ESMF_Time) :: currTime
      logical :: is_ringing

      call ESMF_ClockGet(clock, currTime=currTime, _RC)
      if (this%run_if_alarm_rings_next) then
         call ESMF_ClockGetNextTime(clock, nextTime=currTime, _RC)
      end if
      is_ringing = this%user_run_alarm%is_ringing(currTime, _RC)
      _RETURN_IF(.not. is_ringing)

      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name, found=found)
      _ASSERT(found, 'phase <'//phase_name//'> not found for gridcomp <'//this%get_name()//'>')

      if (this%is_threading_active()) then
         ! This component is being run from inside an OpenMP parallel region
         ! opened by an ancestor.  The couplers, the profiler and the logger
         ! are shared by all threads and are not thread safe, so only the
         ! thread local "mini" component may run here.
         import_couplers = this%registry%get_import_couplers()
         export_couplers = this%registry%get_export_couplers()
         _ASSERT(import_couplers%size() == 0 .and. export_couplers%size() == 0, &
              'component <'//this%get_name()//'> has couplers and cannot be run by a threaded ancestor')
         call run_thread_local(this, phase, _RC)
         _RETURN(ESMF_SUCCESS)
      end if

      import_couplers = this%registry%get_import_couplers()
      do i = 1, import_couplers%size()
         drvr = import_couplers%of(i)
          call drvr%ptr%run(phase_idx=MAPL_GENERIC_COUPLER_UPDATE, _RC)
      end do

      logger => this%get_logger()
      call logger%info(phase_name//": starting...")
      call this%start_timer(phase_name)
      if (this%get_use_threads()) then
         call this%run_user_threaded(phase, _RC)
      else
         call this%user_gc_driver%run(phase_idx=phase, _RC)
      end if
      call this%stop_timer(phase_name)
      call logger%info(phase_name//": ...completed")

      export_couplers = this%registry%get_export_couplers()
      do i = 1, export_couplers%size()
         drvr = export_couplers%of(i)
          call drvr%ptr%run(phase_idx=MAPL_GENERIC_COUPLER_INVALIDATE, _RC)
      end do

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains

      subroutine run_thread_local(outer_meta, phase_idx, rc)
         class(OuterMetaComponent), intent(inout) :: outer_meta
         integer, intent(in) :: phase_idx
         integer, optional, intent(out) :: rc

         integer :: status, user_status
         type(ESMF_GridComp) :: thread_gc
         type(MultiState) :: thread_states
         type(ESMF_Clock) :: user_clock

         thread_states = outer_meta%get_thread_states(_RC)
         thread_gc = outer_meta%get_thread_gridcomp(_RC)
         user_clock = outer_meta%user_gc_driver%get_clock()

         call ESMF_GridCompRun(thread_gc, &
              importState=thread_states%importState, &
              exportState=thread_states%exportState, &
              clock=user_clock, phase=phase_idx, _USERRC)

         _RETURN(_SUCCESS)
      end subroutine run_thread_local

   end subroutine run_user

end submodule run_user_smod
