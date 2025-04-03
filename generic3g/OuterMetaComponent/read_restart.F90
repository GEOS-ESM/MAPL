#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) read_restart_smod
   use mapl3g_RestartHandler
   use mapl3g_Multistate
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! Locals
      type(GriddedComponentDriver), pointer :: driver
      character(:), allocatable :: name
      type(MultiState) :: states
      type(ESMF_State) :: internal_state, import_state
      type(ESMF_Geom) :: geom
      type(RestartHandler) :: restart_handler
      integer :: status

      driver => this%get_user_gc_driver()
      name = driver%get_name()
      ! TODO: Need a better way of identifying a gridcomp that reads a restart
      if (this%has_geom()) then
         geom = this%get_geom()
         states = driver%get_states()
         call states%get_state(import_state, "import", _RC)
         call states%get_state(internal_state, "internal", _RC)
         restart_handler = RestartHandler(name, geom, clock, _RC)
         call restart_handler%read("import", import_state, _RC)
         call restart_handler%read("internal", internal_state, _RC)
      end if
      call recurse_read_restart(this, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
   end subroutine read_restart

end submodule read_restart_smod
