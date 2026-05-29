#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) write_restart_smod
   use mapl_MultiState_mod
   use mapl_RestartHandler_mod
   use mapl_os_mod
   use mapl_mp_utils_internal, only: MAPL_GetCheckpointSubdir
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine write_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! Locals
      character(*), parameter :: PHASE_NAME = 'GENERIC::WRITE_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      integer :: status
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(esmf_Time) :: currTime

      call recurse_write_restart_(this, _RC)
      call this%run_custom(ESMF_METHOD_WRITERESTART, PHASE_NAME, _RC)
      _RETURN_UNLESS(this%has_geom())
     
      driver => this%get_user_gc_driver()
      call esmf_ClockGet(driver%get_clock(), currTime=currTime, _RC)

      restart_handler = RestartHandler(this%get_geom(), currTime, this%get_logger())

      states = driver%get_states()
      subdir = MAPL_GetCheckpointSubdir(this%hconfig, currTime, _RC)

      if (this%component_spec%misc%checkpoint_controls%import) then
         filename = mapl_PathJoin(subdir, driver%get_name() // '_import.nc')
         call this%start_timer("WriteImportCheckpoint", _RC)
         call restart_handler%write(states%importState, filename, _RC)
         call this%stop_timer("WriteImportCheckpoint", _RC)
      end if
      
      if (this%component_spec%misc%checkpoint_controls%internal) then
         filename = mapl_PathJoin(subdir, driver%get_name() // '_internal.nc')
         call this%start_timer("WriteInternalCheckpoint", _RC)
         call restart_handler%write(states%internalState, filename, _RC)
         call this%stop_timer("WriteInternalCheckpoint", _RC)
      end if
      
      if (this%component_spec%misc%checkpoint_controls%export) then
         filename = mapl_PathJoin(subdir, driver%get_name() // '_export.nc')
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
