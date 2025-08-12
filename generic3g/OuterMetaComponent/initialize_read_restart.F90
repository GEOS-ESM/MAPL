#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_read_restart_smod

   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   use mapl3g_MultiState
   use mapl3g_RestartHandler, only: RestartHandler
   use mapl_OS

   implicit none

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
      type(esmf_Time) :: currTime
      integer :: status

      call recurse(this, phase_idx=GENERIC_INIT_READ_RESTART, _RC)
      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      call esmf_ClockGet(driver%get_clock(), currTime=currTime, _RC)

      restart_handler = RestartHandler( &
           this%get_geom(), &
           currTime, &
           this%get_logger())

      states = driver%get_states()
      subdir = get_checkpoint_subdir(this%hconfig, currTime, _RC)

      if (this%component_spec%misc%restart_controls%import) then
         filename = mapl_PathJoin(subdir, driver%get_name() // '_import.nc')
         call restart_handler%read(states%importState, filename, _RC)
      end if
      
      if (this%component_spec%misc%restart_controls%internal) then
         filename = mapl_PathJoin(subdir, driver%get_name() // '_internal.nc')
         call restart_handler%read(states%internalState, filename, _RC)
      end if
      
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_read_restart

end submodule initialize_read_restart_smod
