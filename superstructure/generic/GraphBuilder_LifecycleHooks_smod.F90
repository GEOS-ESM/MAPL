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
      character(:), allocatable :: msg

      call graphbuilder_check_unsatisfied_imports(this, unresolved_imports=unresolved, rc=status)
      call report_if_failed(this, 'check_unsatisfied_imports', status)

      lgr => this%get_logger()
      do i = 1, unresolved%size()
         entry => unresolved%of(i)
         ! openspec/changes/hierarchy-wide-component-graph, found while
         ! chasing a real (nondeterministic hang/corruption) gfortran
         ! failure in Group 6's own real-lifecycle testing: pflogger's
         ! own class(*) variadic %a substitution is unreliable under
         ! gfortran for allocatable-character actual arguments (a known
         ! gfortran/pflogger issue, not specific to this call site) -
         ! fixed by concatenating into one plain character string
         ! first and passing that single string, never a variadic %a
         ! substitution, matching the safer convention this fix
         ! establishes for every other %a:%a call site this change
         ! touches.
         msg = 'GraphBuilder: no ordinary-match export found for unresolved import ' // &
              entry%component_name // ':' // entry%short_name
         call lgr%warning(msg)
      end do
   end subroutine graphbuilder_run_activate_hook

   ! griddedcomponentdriver-integration-lifecycle design.md Decisions
   ! ("REQ-MTH-011 step (c) convergence: match the existing fixed
   ! two-pass schedule; add one missing hard-error check"): the
   ! connect()-time analog of legacy's own two-pass resolution.
   !
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md Group 9
   ! (design.md D7): this hook no longer calls graphbuilder_freeze nor
   ! runs the unresolved-import convergence check itself - both moved
   ! to graphbuilder_run_graph_freeze_hook below, which runs the check
   ! exactly once, hierarchy-wide, after every component's own graph is
   ! frozen (not per-component, immediately after each component's own
   ! freeze, as this hook used to do). Freezing per-component here was
   ! never actually safe to begin with once imports/exports can be
   ! mirrored from a child that has not yet itself been visited by this
   ! same GENERIC_INIT_ACCEPT_TRANSFER pass - see tasks.md Group 9's own
   ! header note for the full blocker analysis that motivated moving
   ! freeze to its own, later, one-shot phase.
    module subroutine graphbuilder_run_connect_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(GraphResolutionEntryVector) :: unsupported
      integer :: i
      type(GraphResolutionEntry), pointer :: entry
      class(Logger), pointer :: lgr
      character(:), allocatable :: msg

      call graphbuilder_resolve_connections(this, unsupported_characteristics=unsupported, rc=status)
      call report_if_failed(this, 'resolve_connections', status)

      lgr => this%get_logger()
      do i = 1, unsupported%size()
         entry => unsupported%of(i)
         ! openspec/changes/hierarchy-wide-component-graph: same %a
         ! variadic-string fix as graphbuilder_run_activate_hook above
         ! - see that call site's own comment for the full gfortran/
         ! pflogger explanation.
         msg = 'GraphBuilder: no registered extension provider for mismatched characteristic ' // &
              entry%component_name // ':' // entry%short_name // ':' // entry%reason
         call lgr%warning(msg)
      end do

      ! openspec/changes/phase-10d-ordinary-phase-method-node, design.md
      ! D2: constructed here. This node's own argument bindings are
      ! sourced only from this component's own advertised import/export
      ! items (already resolved at GENERIC_INIT_ADVERTISE time, design.md
      ! D3), never from a resolved connection, so there is no ordering
      ! dependency on freeze (which no longer happens in this hook at
      ! all, tasks.md Group 9). Construction only - see
      ! graphbuilder_build_run_phase_node's own header comment for why
      ! nothing here invokes the constructed node.
      call graphbuilder_build_run_phase_node(this, status)
      call report_if_failed(this, 'build_run_phase_node', status)
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
