#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) write_restart_smod

   use mapl_MultiState_mod
   use mapl_RestartHandler_mod
   use mapl_os_mod
   use mapl_ErrorHandling_mod

   implicit none(type,external)

contains

   module recursive subroutine write_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! Locals
      character(*), parameter :: PHASE_NAME = 'GENERIC::WRITE_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      integer :: status
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(ESMF_Time) :: current_time

      call recurse_write_restart_(this, _RC)
      call this%run_custom(ESMF_METHOD_WRITERESTART, PHASE_NAME, _RC)

      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      call ESMF_ClockGet(driver%get_clock(), currTime=current_time, _RC)
      restart_handler = RestartHandler(this%get_geom(), current_time, this%get_logger())
      states = driver%get_states()

      if (this%component_spec%misc%checkpoint_controls%import) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_IMPORT, _RC)
         call this%start_timer("WriteImportCheckpoint", _RC)
         call restart_handler%write(states%importState, filename, _RC)
         call this%stop_timer("WriteImportCheckpoint", _RC)
      end if

      if (this%component_spec%misc%checkpoint_controls%internal) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_INTERNAL, _RC)
         call this%start_timer("WriteInternalCheckpoint", _RC)
         call restart_handler%write(states%internalState, filename, _RC)
         call this%stop_timer("WriteInternalCheckpoint", _RC)
      end if

      if (this%component_spec%misc%checkpoint_controls%export) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_EXPORT, _RC)
         call this%start_timer("WriteExportCheckpoint", _RC)
         call restart_handler%write(states%exportState, filename, _RC)
         call this%stop_timer("WriteExportCheckpoint", _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine write_restart

end submodule write_restart_smod
