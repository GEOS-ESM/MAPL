#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) write_restart_smod
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
      character(:), allocatable :: name
      type(ESMF_State) :: internal_state
      type(ESMF_Geom) :: geom
      type(RestartHandler) :: restart_handler
      integer :: status

      driver => this%get_user_gc_driver()
      name = driver%get_name()
      if (this%has_geom()) then
         geom = this%get_geom()
         restart_handler = RestartHandler(name, geom, clock, _RC)
         call restart_handler%write("import", importState, _RC)
         internal_state = this%get_internal_state()
         call restart_handler%write("internal", internal_state, _RC)
         if (this%component_spec%misc%write_exports) then
            call restart_handler%write("export", exportState, _RC)
         end if
      end if
      if (name /= "HIST") then
         call recurse_write_restart_(this, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(exportState)
   end subroutine write_restart

end submodule write_restart_smod
