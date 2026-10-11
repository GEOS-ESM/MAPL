#include "MAPL.h"

! openspec/changes/phase-10d-ordinary-phase-method-node: see
! OuterMetaComponent.F90's run_phase_method_node_id field-declaration
! comment. Returns by value (not a pointer - a NodeId is a tiny value
! type, CallbackMethodBinding%get_method_node_id's own precedent, not
! get_vertical_grid's pointer-to-a-polymorphic-field precedent, which
! solves a different problem that does not apply here). Reports "unset"
! via the returned NodeId%is_valid() being .false. (INVALID_NodeId,
! the field's own default value) - callers are responsible for
! deciding whether an invalid NodeId is itself an error under their
! own gating.
submodule (mapl_OuterMetaComponent_mod) get_run_phase_method_node_id_smod
   implicit none(type,external)

contains

   module function get_run_phase_method_node_id(this) result(node_id)
      type(NodeId) :: node_id
      class(OuterMetaComponent), intent(in) :: this

      node_id = this%run_phase_method_node_id
   end function get_run_phase_method_node_id

end submodule get_run_phase_method_node_id_smod
