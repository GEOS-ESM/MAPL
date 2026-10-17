#include "MAPL.h"

! openspec/changes/hierarchy-wide-component-graph, tasks.md Group 9
! (design.md D7, "Freeze becomes one hierarchy-wide call"):
! GENERIC_INIT_GRAPH_FREEZE's own handler.
!
! Deliberately does NOT call recurse() the way every other init-phase
! handler in this directory does. GENERIC_INIT_PHASE_SEQUENCE's own
! recursion into every component in the hierarchy is driven entirely by
! each phase handler's own explicit `call recurse(this, phase_idx=...)`
! - ESMF/NUOPC itself never auto-propagates GridCompInitialize into
! child gridcomps; MAPL_DriverInitializePhases (superstructure/
! component/ComponentDriver.F90) only ever calls
! ESMF_GridCompInitialize once per phase, on the single top-level
! GridComp the driver owns. Omitting the recurse() call here therefore
! means this handler, as dispatched by GenericGridComp.F90's own
! `initialize` select case, only ever actually executes on that one
! top-level GridComp's own OuterMetaComponent - never on any
! descendant, since nothing else ever tells ESMF to invoke this phase
! on a child. is_hierarchy_root below is kept as an explicit,
! self-documenting belt-and-suspenders check (not load-bearing under
! today's only real call path, the driver) rather than relying solely
! on that call-path argument - see OuterMetaComponent.F90's own
! is_hierarchy_root field-declaration comment.
!
! GraphBuilder%run_graph_freeze_hook (GraphBuilder_Freeze_smod.F90) is
! what performs the actual single explicit whole-tree walk (its own
! ordinary ad hoc Fortran recursion, ONE level removed from this ESMF
! phase-dispatch mechanism entirely) that freezes every component's
! own ComponentGraph exactly once and then runs the hierarchy-wide
! unresolved-import convergence check.
submodule (mapl_OuterMetaComponent_mod) initialize_graph_freeze_smod
   use mapl_GraphBuilder_mod, only: GraphBuilder
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   module subroutine initialize_graph_freeze(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(GraphBuilder) :: gb

      if (this%is_hierarchy_root) then
         call gb%run_graph_freeze_hook(this)
      end if

      _RETURN(_SUCCESS)
   end subroutine initialize_graph_freeze

end submodule initialize_graph_freeze_smod
