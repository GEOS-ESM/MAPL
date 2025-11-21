#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) apply_to_children_custom_smod
   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling
   implicit none

contains

   ! This procedure should not be invoked recursively - it is not for traversing the tree,
   ! but rather just to facilitate custom operations where a parent component must pass
   ! information to its children.
   module subroutine apply_to_children_custom(this, oper, rc)
      class(OuterMetaComponent), intent(inout) :: this
      procedure(I_child_op) :: oper
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc

      associate(b => this%children%begin(), e => this%children%end())
        iter = b
        do while (iter /= e)
           child => iter%second()
           child_outer_gc = child%get_gridcomp()
           child_meta => get_outer_meta(child_outer_gc, _RC)
           call oper(this, child_meta, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine apply_to_children_custom

end submodule apply_to_children_custom_smod
