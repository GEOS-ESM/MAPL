#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) write_restart_smod
   use mapl3g_RestartHandler
   use mapl3g_MultiState
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
      character(:), allocatable :: name
      type(MultiState) :: states
      type(ESMF_State) :: internal_state, import_state
      type(ESMF_Geom) :: geom
      type(RestartHandler) :: restart_handler
      integer :: status

      driver => this%get_user_gc_driver()
      name = driver%get_name()
      ! TODO: Need a better way of identifying a gridcomp that writes restart
      if ((name /= "cap") .and. (name /= "HIST") .and. (name/="EXTDATA")) then
         geom = this%get_geom()
         states = driver%get_states()
         call states%get_state(import_state, "import", _RC)
         call states%get_state(internal_state, "internal", _RC)
         restart_handler = RestartHandler(name, geom, clock, _RC)
         call restart_handler%write("import", import_state, _RC)
         call restart_handler%write("internal", internal_state, _RC)
      end if
      if (name /= "HIST") then
         call recurse_write_restart_(this, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(importState)
   end subroutine write_restart

end submodule write_restart_smod
