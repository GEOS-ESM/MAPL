#include "MAPL.h"

! Lifecycle-hook wrappers (task 2.3/3.4)
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_LifecycleHooks_smod
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use pflogger, only: Logger
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   module subroutine graphbuilder_run_advertise_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status

      call graphbuilder_advertise(this, status)
      call report_if_failed(this, 'advertise', status)
   end subroutine graphbuilder_run_advertise_hook

   module subroutine graphbuilder_run_activate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(GraphResolutionEntryVector) :: unresolved
      integer :: i
      type(GraphResolutionEntry), pointer :: entry
      class(Logger), pointer :: lgr

      call graphbuilder_check_unsatisfied_imports(this, unresolved_imports=unresolved, rc=status)
      call report_if_failed(this, 'check_unsatisfied_imports', status)

      lgr => this%get_logger()
      do i = 1, unresolved%size()
         entry => unresolved%of(i)
         call lgr%warning( &
              'GraphBuilder: no ordinary-match export found for unresolved import %a:%a', &
              entry%component_name, entry%short_name)
      end do
   end subroutine graphbuilder_run_activate_hook

   ! griddedcomponentdriver-integration-lifecycle design.md Decisions
   ! ("REQ-MTH-011 step (c) convergence: match the existing fixed
   ! two-pass schedule; add one missing hard-error check"): once this
   ! call's own graphbuilder_freeze succeeds - i.e. once no further
   ! ACCEPT_TRANSFER pass can ever change anything further, per
   ! GENERIC_INIT_PHASE_SEQUENCE's fixed two-pass schedule
   ! (enums/GenericPhases.F90) - re-run the same
   ! graphbuilder_check_unsatisfied_imports() check
   ! graphbuilder_run_activate_hook already runs (at GENERIC_INIT_ADVERTISE
   ! time, before either real pass), but this time escalate a non-empty
   ! result to an explicit failure instead of only a logged warning.
   ! Still caught and downgraded to a logged failure by this hook's own
   ! existing report_if_failed convention below - this is a hard error
   ! only within graphbuilder_resolve_connections/
   ! check_unsatisfied_imports' own directly-tested API, never a new way
   ! for a GraphBuilder defect to break real component initialization
   ! (design.md Risks).
    module subroutine graphbuilder_run_connect_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(GraphResolutionEntryVector) :: unsupported
      type(GraphResolutionEntryVector) :: unresolved
      integer :: i
      type(GraphResolutionEntry), pointer :: entry
      class(Logger), pointer :: lgr

      call graphbuilder_resolve_connections(this, unsupported_characteristics=unsupported, rc=status)
      call report_if_failed(this, 'resolve_connections', status)

      lgr => this%get_logger()
      do i = 1, unsupported%size()
         entry => unsupported%of(i)
         call lgr%warning( &
              'GraphBuilder: no registered extension provider for mismatched characteristic %a:%a:%a', &
              entry%component_name, entry%short_name, entry%reason)
      end do

      ! openspec/changes/phase-10d-ordinary-phase-method-node, design.md
      ! D2: constructed here, BEFORE freeze below - not after. This
      ! node's own argument bindings are sourced only from this
      ! component's own advertised import/export items (already
      ! resolved at GENERIC_INIT_ADVERTISE time, design.md D3), never
      ! from a resolved connection, so there is no reason to wait for
      ! freeze - and ComponentGraph%register_node/next_node_id both
      ! unconditionally reject a call once the graph is frozen
      ! (ComponentGraph.F90), so placing this after freeze would hard-
      ! fail unconditionally rather than merely in some edge case.
      ! Construction only - see graphbuilder_build_run_phase_node's own
      ! header comment for why nothing here invokes the constructed
      ! node.
      call graphbuilder_build_run_phase_node(this, status)
      call report_if_failed(this, 'build_run_phase_node', status)

      call graphbuilder_freeze(this, status)
      call report_if_failed(this, 'freeze', status)

      if (status == 0) then
         call graphbuilder_check_unsatisfied_imports(this, unresolved_imports=unresolved, rc=status)
         call report_if_failed(this, 'check_unsatisfied_imports', status)
         if (status == 0) then
            call assert_converged(unresolved, status)
            call report_if_failed(this, 'convergence_check', status)
         end if
      end if
   end subroutine graphbuilder_run_connect_hook

   ! openspec/changes/phase-10a-state-materialization: the real
   ! GENERIC_INIT_ACCEPT_TRANSFER call site's own entry point into
   ! graphbuilder_populate_state - same report_if_failed convention
   ! every other hook above already uses, so a GraphBuilder defect here
   ! can never break real component initialization (design.md Risks;
   ! tasks.md 3.4).
   module subroutine graphbuilder_run_populate_state_hook(this, multi_state)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state

      integer :: status

      call graphbuilder_populate_state(this, multi_state, status)
      call report_if_failed(this, 'populate_state', status)
   end subroutine graphbuilder_run_populate_state_hook

   ! Split out from graphbuilder_run_connect_hook so this check has its
   ! own real rc to report through the same report_if_failed convention
   ! every other step in that hook already uses, rather than an inline
   ! _ASSERT with no rc of its own to propagate.
   module subroutine assert_converged(unresolved, rc)
      type(GraphResolutionEntryVector), intent(in) :: unresolved
      integer, optional, intent(out) :: rc

      character(:), allocatable :: msg

      _RETURN_IF(unresolved%size() == 0)

      msg = 'GraphBuilder: unresolved required import(s) remain once the fixed realize/accept/realize '// &
           'schedule ends (griddedcomponentdriver-integration-lifecycle design.md "REQ-MTH-011 step (c) convergence")'
      _FAIL(msg)
   end subroutine assert_converged

   module subroutine report_if_failed(this, step_name, status)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: step_name
      integer, intent(in) :: status

      class(Logger), pointer :: lgr

      if (status /= 0) then
         lgr => this%get_logger()
         call lgr%warning( &
              'GraphBuilder: %a step failed with rc=%i0 - continuing without graph representation for this step', &
              step_name, status)
      end if
   end subroutine report_if_failed

end submodule GraphBuilder_LifecycleHooks_smod
