#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_read_restart_smod

   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   use mapl3g_MultiState
   use mapl3g_RestartHandler, only: RestartHandler

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
      type(ESMF_Clock) :: clock
      type(RestartHandler) :: restart_handler
      integer :: status

      call recurse(this, phase_idx=GENERIC_INIT_READ_RESTART, _RC)

      _RETURN_UNLESS(this%has_geom())

      driver => this%get_user_gc_driver()
      states = driver%get_states()
      restart_handler = RestartHandler( &
           driver%get_name(), & ! this%get_geom() returns the name in brackets
           this%get_geom(), &
           driver%get_clock(), &
           this%get_logger(), &
           _RC)

      if (this%component_spec%misc%restart_controls%import) then
         call restart_handler%read("import", states%importState, _RC)
      end if
      
      if (this%component_spec%misc%restart_controls%internal) then
         call restart_handler%read("internal", states%internalState, _RC)
      end if
      
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_read_restart

end submodule initialize_read_restart_smod
