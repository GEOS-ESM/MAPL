#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_children_names_smod

   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling

   implicit none

contains

   module function get_children_names(this) result(names)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(StringVector) :: names

      type(GriddedComponentDriverMapIterator) :: iter

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           call names%push_back(iter%first())
        end do
      end associate
   end function get_children_names

end submodule get_children_names_smod
