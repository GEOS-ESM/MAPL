#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) attach_outer_meta_smod
   implicit none

contains

   module subroutine attach_outer_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _SET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE, outer_meta)

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

end submodule attach_outer_meta_smod
