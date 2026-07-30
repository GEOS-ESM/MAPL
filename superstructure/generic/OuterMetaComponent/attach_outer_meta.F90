#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) attach_outer_meta_smod
   use mapl_UserSetServices_mod, only: AbstractUserSetServices
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine attach_outer_meta(gridcomp, user_setservices, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      class(AbstractUserSetServices), optional, intent(in) :: user_setservices
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: outer_meta

      _SET_NAMED_PRIVATE_STATE(gridcomp, OuterMetaComponent, OUTER_META_PRIVATE_STATE)

      if (present(user_setservices)) then
         outer_meta => get_outer_meta(gridcomp, _RC)
         allocate(outer_meta%user_setservices, source=user_setservices)
      end if

      _RETURN(_SUCCESS)
   end subroutine attach_outer_meta

end submodule attach_outer_meta_smod
