#include "MAPL.h"

! openspec/changes/phase-10d-ordinary-phase-method-node: see
! OuterMetaComponent.F90's run_phase_method_node_id field-declaration
! comment. Rejects a second call (no silent replace) - matches this
! repo's established no-silent-replace registration convention
! (ComponentGraph%register_node, MethodGraphNode%bind_argument, etc.);
! GraphBuilder%build_run_phase_node's own idempotent-construction guard
! (mirroring advertise_one's "already indexed -> nothing to do") is
! what prevents a second real call from ever reaching this setter
! under ordinary use.
submodule (mapl_OuterMetaComponent_mod) set_run_phase_method_node_id_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine set_run_phase_method_node_id(this, node_id, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%run_phase_method_node_id%is_valid(), &
           'OuterMetaComponent: set_run_phase_method_node_id - already set')

      this%run_phase_method_node_id = node_id

      _RETURN(_SUCCESS)
   end subroutine set_run_phase_method_node_id

end submodule set_run_phase_method_node_id_smod
