#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_geom_b_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_GEOM_B
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
       
       call this%propagate_geom_to_children(_RC)
       call recurse(this, phase_idx=MAPL_GENERIC_INIT_GEOM_B, _RC)

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(unusable)
   end subroutine initialize_geom_b

end submodule initialize_geom_b_smod
