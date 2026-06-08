#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_read_restart_smod

   use mapl_enums_api, only: MAPL_GENERIC_INIT_READ_RESTART
   use mapl_ErrorHandling_mod
   use mapl_MultiState_mod
   use mapl_RestartHandler_mod, only: RestartHandler
   use mapl_os_mod

   implicit none(type,external)

contains

   module recursive subroutine initialize_read_restart(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_READ_RESTART'
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      character(:), allocatable :: subdir
      character(:), allocatable :: filename
      type(esmf_Time) :: current_time
      integer :: status
      class(Logger), pointer :: user_logger
      logical :: bootstrap

      call recurse(this, phase_idx=MAPL_GENERIC_INIT_READ_RESTART, _RC)
      call this%run_custom(ESMF_METHOD_READRESTART, PHASE_NAME, _RC)

      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      states = driver%get_states()
      call esmf_ClockGet(driver%get_clock(), currTime=current_time, _RC)

      user_logger => this%get_logger()
      restart_handler = RestartHandler(this%get_geom(), current_time, user_logger)

      ! if I try to pass this derived type in to read in folowing code nag crashes
      bootstrap = this%component_spec%misc%restart_controls%bootstrap
      if (this%component_spec%misc%restart_controls%import) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_IMPORT, _RC)
         call this%start_timer("ReadImportRestart", _RC)
         call restart_handler%read(states%importState, filename, bootstrap, _RC)
         call this%stop_timer("ReadImportRestart", _RC)
      end if

      if (this%component_spec%misc%restart_controls%internal) then
         filename = this%get_checkpoint_filename(current_time, ESMF_STATEINTENT_INTERNAL, _RC)
         call this%start_timer("ReadInternalRestart", _RC)
         call restart_handler%read(states%internalState, filename, bootstrap, _RC)
         call this%stop_timer("ReadInternalRestart", _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_read_restart

end submodule initialize_read_restart_smod
