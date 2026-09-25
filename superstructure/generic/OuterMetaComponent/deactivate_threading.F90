#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) deactivate_threading_smod
   use mapl_GriddedComponentDriverMap_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   ! Restore the single threaded behavior of this component and its
   ! children.  The sub objects are retained so that they can be reused
   ! the next time threading is activated.
   module recursive subroutine deactivate_threading(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc

      associate (e => this%children%end())
        iter = this%children%begin()
        do while (iter /= e)
           child => iter%second()
           child_outer_gc = child%get_gridcomp()
           child_meta => get_outer_meta(child_outer_gc, _RC)
           call child_meta%deactivate_threading(_RC)
           call iter%next()
        end do
      end associate

      this%threading_active = .false.

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine deactivate_threading

end submodule deactivate_threading_smod
