#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) free_outer_meta_smod
   use mapl3g_ESMF_Interfaces, only: MAPL_UserCompGetInternalState
   use mapl_ErrorHandling
   implicit none (type, external)

contains

   module subroutine free_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaWrapper) :: wrapper
      type(ESMF_GridComp) :: user_gridcomp

      call MAPL_UserCompGetInternalState(gridcomp, OUTER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not created for this gridcomp")

      user_gridcomp = wrapper%outer_meta%user_gc_driver%get_gridcomp()
      call free_inner_meta(user_gridcomp, _RC)

      deallocate(wrapper%outer_meta)

      _RETURN(_SUCCESS)
   end subroutine free_outer_meta

end submodule free_outer_meta_smod
