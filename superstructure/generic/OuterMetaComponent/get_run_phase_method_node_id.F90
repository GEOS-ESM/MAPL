#include "MAPL.h"

! Returns NodeId by value (not pointer; NodeId is a tiny value type).
! Invalid NodeId (INVALID_NodeId) means unset; callers decide whether that's an error.
submodule (mapl_OuterMetaComponent_mod) get_run_phase_method_node_id_smod
   implicit none(type,external)

contains

   module function get_run_phase_method_node_id(this) result(node_id)
      type(NodeId) :: node_id
      class(OuterMetaComponent), intent(in) :: this

      node_id = this%run_phase_method_node_id
   end function get_run_phase_method_node_id

end submodule get_run_phase_method_node_id_smod
