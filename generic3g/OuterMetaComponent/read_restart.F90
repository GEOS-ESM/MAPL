#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) read_restart_smod
   implicit none

contains

   module subroutine read_restart(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      ! Locals
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      character(:), allocatable :: child_name
      type(ESMF_GridComp) :: child_outer_gc
      type(OuterMetaComponent), pointer :: child_outer_meta
      type(MultiState) :: child_states
      type(ESMF_State) :: child_internal_state, child_import_state
      type(ESMF_Geom) :: child_geom
      type(ESMF_Clock) :: child_clock
      type(Restart) :: rstrt
      integer :: status

      associate(e => this%children%end())
        iter = this%children%begin()
        do while (iter /= e)
           child_name = iter%first()
           if (child_name /= "HIST") then
              child => iter%second()
              child_clock = child%get_clock()
              child_outer_gc = child%get_gridcomp()
              child_outer_meta => get_outer_meta(child_outer_gc, _RC)
              child_geom = child_outer_meta%get_geom()
              rstrt = Restart(child_name, child_geom, child_clock, _RC)
              child_internal_state = child_outer_meta%get_internal_state()
              call rstrt%read("internal", child_internal_state, _RC)
              child_states = child%get_states()
              call child_states%get_state(child_import_state, "import", _RC)
              call rstrt%read("import", child_import_state, _RC)
              call child%read_restart(_RC)
           end if
           call iter%next()
        end do
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine read_restart

end submodule read_restart_smod
