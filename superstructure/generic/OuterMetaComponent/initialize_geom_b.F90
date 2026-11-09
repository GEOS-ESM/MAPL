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

       ! The geometry hook runs here, not in initialize_advertise.F90:
       ! GEOM_A/GEOM_B run after ADVERTISE, so only now is this
       ! component's own geom real (a GEOMETRY_PROVIDER's geom is set at
       ! the start of its GEOM_A; a GEOMETRY_FROM_PARENT child's geom is
       ! set by propagate_geom_to_children() above). recurse() above has
       ! also completed every child's GEOM_B, including its own
       ! run_geometry_hook call, so every child already has a real geom
       ! and its geometry graph node registered - required for both the
       ! pull and the push direction (see the "Horizontal geometry" note
       ! in the mapl_GraphBuilder_mod header). A no-op when graph_mode()
       ! is GRAPH_MODE_REGISTRY.
       call gb%run_geometry_hook(this)

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(unusable)
   end subroutine initialize_geom_b

end submodule initialize_geom_b_smod
