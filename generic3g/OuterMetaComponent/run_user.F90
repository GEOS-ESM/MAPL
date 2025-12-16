#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) run_user_smod

   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE
   use mapl_ErrorHandling
   use pflogger, only: logger_t => logger

   implicit none

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
      is_ringing = this%user_run_alarm%is_ringing(currTime, _RC)
      _RETURN_IF(.not. is_ringing)

      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name, found=found)
      _ASSERT(found, 'phase <'//phase_name//'> not found for gridcomp <'//this%get_name()//'>')

      import_couplers = this%registry%get_import_couplers()
      do i = 1, import_couplers%size()
         drvr = import_couplers%of(i)
         call drvr%ptr%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end do

      logger => this%get_logger()
      call logger%debug("Run (phase: "//phase_name//"): starting...")
      call this%start_timer("Run "//this%get_name()//" (phase:"//phase_name//")")
      call this%user_gc_driver%run(phase_idx=phase, _RC)
      call this%stop_timer("Run "//this%get_name()//" (phase:"//phase_name//")")
      call logger%debug("Run (phase: "//phase_name//"): ...completed")

      export_couplers = this%registry%get_export_couplers()
      do i = 1, export_couplers%size()
         drvr = export_couplers%of(i)
         call drvr%ptr%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
      end do

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_user

end submodule run_user_smod
