#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) run_child_by_name_smod
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine run_child_by_name(this, child_name, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child
      type(ESMF_GridComp) :: child_gc
      type(OuterMetaComponent), pointer :: child_meta
      logical :: found
      integer :: phase_idx
      class(Logger), pointer :: lgr
      character(:), allocatable :: this_name

      child = this%get_child(child_name, _RC)
      child_gc = child%get_gridcomp()
      child_meta => get_outer_meta(child_gc, _RC)

      phase_idx = 1
      if (present(phase_name)) then
         phase_idx = get_phase_index(child_meta%get_phases(ESMF_METHOD_RUN), phase_name=phase_name, found=found)
         _ASSERT(found, "run phase: <"//phase_name//"> not found.")
      end if

      lgr => this%get_logger()
      this_name = this%get_name() ! workaround for gfortran
      call lgr%debug('%a run child <%a~> (phase=%a~)', this_name, child_name, phase_name, _RC)
      call child%run(phase_idx=phase_idx, _RC)
      call lgr%debug('  ... %a completed run child <%a~> (phase=%a~)', this_name, child_name, phase_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_child_by_name

end submodule run_child_by_name_smod
