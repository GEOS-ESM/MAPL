#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) read_restart_smod
   implicit none

contains

   module recursive subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! Locals
      type(GriddedComponentDriver), pointer :: driver
      type(ESMF_GridComp) :: gc
      character(:), allocatable :: name
      type(MultiState) :: states
      type(ESMF_State) :: internal_state, import_state
      type(ESMF_Geom) :: geom
      type(Restart) :: rstrt
      integer :: status

      driver => this%get_user_gc_driver()
      name = driver%get_name()
      if ((name /= "cap") .and. (name /= "HIST")) then
         gc = driver%get_gridcomp()
         geom = this%get_geom()
         states = driver%get_states()
         call states%get_state(import_state, "import", _RC)
         call states%get_state(internal_state, "internal", _RC)
         rstrt = Restart(name, geom, clock, _RC)
         call rstrt%read("import", import_state, _RC)
         call rstrt%read("internal", internal_state, _RC)
      end if
      if (name /= "HIST") then
         call recurse_read_restart(this, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine read_restart

end submodule read_restart_smod
