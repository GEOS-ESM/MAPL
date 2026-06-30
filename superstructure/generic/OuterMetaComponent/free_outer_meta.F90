#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) free_outer_meta_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine free_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: user_gridcomp
      type(OuterMetaComponent), pointer :: outer_meta

      _GET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE, outer_meta)

      user_gridcomp = outer_meta%user_gc_driver%get_gridcomp()
      call free_inner_meta(user_gridcomp, _RC)

      _RETURN(_SUCCESS)
   end subroutine free_outer_meta

end submodule free_outer_meta_smod
