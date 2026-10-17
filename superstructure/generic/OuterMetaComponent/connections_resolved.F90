#include "MAPL.h"

! openspec/changes/hierarchy-wide-component-graph, tasks.md Group 9: see
! OuterMetaComponent.F90's own connections_resolved field-declaration
! comment for the full rationale - this replaces
! graphbuilder_resolve_connections's former graph%is_frozen() idempotency
! gate (GraphBuilder_ConnectionResolution_smod.F90), which stopped being
! valid once freeze moved off the per-component GENERIC_INIT_
! ACCEPT_TRANSFER call site.
submodule (mapl_OuterMetaComponent_mod) connections_resolved_smod
   implicit none(type, external)

contains

   module function connections_already_resolved(this) result(resolved)
      logical :: resolved
      class(OuterMetaComponent), intent(in) :: this

      resolved = this%connections_resolved
   end function connections_already_resolved

   module subroutine mark_connections_resolved(this)
      class(OuterMetaComponent), intent(inout) :: this

      this%connections_resolved = .true.
   end subroutine mark_connections_resolved

end submodule connections_resolved_smod
