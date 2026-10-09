#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_geom_b_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_GEOM_B
   use mapl_GraphBuilder_mod, only: GraphBuilder
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains
   
   ! In this sweep, components inherit geometry from their parent
   ! unless otherwise specified.
   module recursive subroutine initialize_geom_b(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

       integer :: status
       character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_GEOM_B'
       type(GraphBuilder) :: gb
       
       call this%propagate_geom_to_children(_RC)
       call recurse(this, phase_idx=MAPL_GENERIC_INIT_GEOM_B, _RC)

       ! openspec/changes/phase-10a-state-materialization: moved here
       ! from initialize_advertise.F90 (a pre-existing timing bug found
       ! while writing this sub-change's own pFUnit coverage - see that
       ! file's own comment at the old call site for the full story).
       ! This is the correct point: by here, this component's own geom
       ! is real (a GEOMETRY_PROVIDER's own geom is set at the START of
       ! its own GEOM_A, well before GEOM_B even starts; a
       ! GEOMETRY_FROM_PARENT child's own geom is set by this call's own
       ! propagate_geom_to_children() above, before recursion) - and
       ! recurse() above has already completed every child's own GEOM_B
       ! (including their own run_geometry_hook call) bottom-up, so by
       ! the time a parent's own hook runs here, every child already has
       ! both a real geom AND its own geometry graph node registered -
       ! required for both the pull and the push direction
       ! (mapl_GraphBuilder_mod's own module header, "Horizontal
       ! geometry" note). Gated behind mapl_GraphMode_mod%graph_mode()
       ! reporting something other than GRAPH_MODE_REGISTRY - a no-op
       ! for every real production run today, same as before this
       ! correction.
       call gb%run_geometry_hook(this)

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(unusable)
   end subroutine initialize_geom_b

end submodule initialize_geom_b_smod
