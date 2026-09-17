#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_component_graph_smod
   implicit none(type,external)

contains

   module function get_component_graph(this) result(local_graph)
      type(ComponentGraph), pointer :: local_graph
      class(OuterMetaComponent), target, intent(in) :: this

      local_graph => this%local_graph
   end function get_component_graph

end submodule get_component_graph_smod
