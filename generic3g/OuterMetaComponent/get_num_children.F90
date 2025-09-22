#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_num_children_smod

   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling

   implicit none

contains

   module function get_num_children(this) result(num_children)
      class(OuterMetaComponent), target, intent(in) :: this
      integer :: num_children

      num_children = this%children%size()
   end function get_num_children

end submodule get_num_children_smod
