#include "MAPL.h"

! Gate for graphbuilder_resolve_connections idempotency. Replaces former
! graph%is_frozen() check which became invalid when freeze moved to whole-hierarchy level.
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
