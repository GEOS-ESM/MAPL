#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) write_restart_smod
   use mapl3g_MultiState
   use mapl3g_RestartHandler
   use mapl_ErrorHandling
   implicit none (type, external)

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
      type(GriddedComponentDriver), pointer :: driver
      type(MultiState) :: states
      type(RestartHandler) :: restart_handler
      integer :: status

      _RETURN_UNLESS(this%has_geom())
      
      driver => this%get_user_gc_driver()
      states = driver%get_states()
      restart_handler = RestartHandler(this%get_name(), this%get_geom(), driver%get_clock(), _RC)
      
      if (this%component_spec%misc%checkpoint_controls%import) then
         call restart_handler%write("import", states%importState, _RC)
      end if
      
      if (this%component_spec%misc%checkpoint_controls%internal) then
         call restart_handler%write("internal", states%internalState, _RC)
      end if
      
      if (this%component_spec%misc%checkpoint_controls%export) then
         call restart_handler%write("export", states%exportState, _RC)
      end if
   
      call recurse_write_restart_(this, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine write_restart

end submodule write_restart_smod
