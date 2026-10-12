#include "MAPL.h"

! Task 5: Validate and freeze
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_Freeze_smod
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! ComponentGraph%freeze() already validates before transitioning
   ! lifecycle state (ComponentGraph.F90 graph_freeze: validate() is
   ! called first, and the frozen flag is only set on success) - this is
   ! a thin, directly-testable wrapper, not a reimplementation.
   module subroutine graphbuilder_freeze(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph

      graph => this%get_component_graph()
      call graph%freeze(_RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_freeze

end submodule GraphBuilder_Freeze_smod
