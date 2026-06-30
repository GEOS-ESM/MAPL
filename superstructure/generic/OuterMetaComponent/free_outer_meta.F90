#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) free_outer_meta_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine free_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper
      type(ESMF_GridComp) :: user_gridcomp

      _GET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE, wrapper%outer_meta)

      user_gridcomp = wrapper%outer_meta%user_gc_driver%get_gridcomp()
      call free_inner_meta(user_gridcomp, _RC)

      deallocate(wrapper%outer_meta)

      _RETURN(_SUCCESS)
   end subroutine free_outer_meta

end submodule free_outer_meta_smod
