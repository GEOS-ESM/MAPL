#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_num_children_smod

   use mapl_GriddedComponentDriverMap_mod
   use mapl_ErrorHandling_mod

   implicit none(type,external)

contains

   module function get_num_children(this) result(num_children)
      class(OuterMetaComponent), target, intent(in) :: this
      integer :: num_children

      num_children = this%children%size()
   end function get_num_children

end submodule get_num_children_smod
