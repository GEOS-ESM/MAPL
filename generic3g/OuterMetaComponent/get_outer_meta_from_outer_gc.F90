#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) get_outer_meta_from_outer_gc_smod
   implicit none

contains

   module function get_outer_meta_from_outer_gc(gridcomp, rc) result(outer_meta)
      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _GET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE, outer_meta)

      _RETURN(_SUCCESS)
   end function get_outer_meta_from_outer_gc

end submodule get_outer_meta_from_outer_gc_smod
