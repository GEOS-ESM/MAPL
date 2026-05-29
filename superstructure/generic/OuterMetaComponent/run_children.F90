#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) run_children_smod
   use mapl_GriddedComponentDriverMap_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine run_children_(this, unusable, phase_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           call this%run_child(iter%first(), phase_name=phase_name, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_children_

end submodule run_children_smod
