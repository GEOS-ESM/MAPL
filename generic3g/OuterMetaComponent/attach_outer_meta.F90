#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) attach_outer_meta_smod
   use mapl3g_ESMF_Interfaces, only: MAPL_UserCompSetInternalState
   use mapl_ErrorHandling
   implicit none (type, external)

contains

   module subroutine attach_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _SET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE)

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

end submodule attach_outer_meta_smod
