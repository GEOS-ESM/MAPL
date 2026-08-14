#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) outer_meta_is_attached_to_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module function outer_meta_is_attached_to(gridcomp, rc) result(is_attached)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: names

      call ESMF_InternalState(gridcomp, names, _RC)
      do i=1, size(names)
         is_attached = names(i) == OUTER_META_PRIVATE_STATE
         if(is_attached) exit
      end do
      
      _RETURN(_SUCCESS)

   end function outer_meta_is_attached_to

end submodule outer_meta_is_attached_to_smod
