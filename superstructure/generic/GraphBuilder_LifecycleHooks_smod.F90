#include "MAPL.h"

! Lifecycle-hook wrappers. Each reports failures via report_if_failed
! and continues, so a GraphBuilder defect never breaks initialization.

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
         ! pflogger %a substitution of allocatable-character actuals is
         ! unreliable under gfortran (nondeterministic hang/corruption);
         ! pass one pre-concatenated string instead.
         msg = 'GraphBuilder: no ordinary-match export found for unresolved import ' // &
              entry%component_name // ':' // entry%short_name
         call lgr%warning(msg)
      end do
   end subroutine graphbuilder_run_activate_hook

   ! connect()-time analog of legacy's two-pass resolution. Freeze and
   ! the unresolved-import convergence check are not done here: they
   ! run once, hierarchy-wide, in graphbuilder_run_graph_freeze_hook,
   ! because per-component freezing is unsafe when imports/exports can
   ! be mirrored from a child not yet visited in this ACCEPT_TRANSFER
   ! pass.
    module subroutine graphbuilder_run_connect_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(GraphResolutionEntryVector) :: unsupported, conflicts
      integer :: i
      type(GraphResolutionEntry), pointer :: entry
      class(Logger), pointer :: lgr
      character(:), allocatable :: msg

      ! A component fills its DEFERRED characteristics in its own init phase;
      ! pick those up so a connection that waited on them can be wired.
      call graphbuilder_harvest_deferred(this, rc=status)
      call report_if_failed(this, 'harvest_deferred', status)

      call graphbuilder_resolve_connections(this, unsupported_characteristics=unsupported, &
           connection_conflicts=conflicts, rc=status)
      call report_if_failed(this, 'resolve_connections', status)

      lgr => this%get_logger()
      do i = 1, unsupported%size()
         entry => unsupported%of(i)
         ! Pre-concatenated string; see graphbuilder_run_activate_hook.
         msg = 'GraphBuilder: no registered extension provider for mismatched characteristic ' // &
              entry%component_name // ':' // entry%short_name // ':' // entry%reason
         call lgr%warning(msg)
      end do

      do i = 1, conflicts%size()
         entry => conflicts%of(i)
         msg = 'GraphBuilder: connection conflict ' // &
              entry%component_name // ':' // entry%short_name // ': ' // entry%reason
         call lgr%warning(msg)
      end do

      ! Argument bindings come only from this component's advertised
      ! items (resolved at ADVERTISE time), so there is no ordering
      ! dependency on freeze. Construction only; see
      ! graphbuilder_build_run_phase_node.
      call graphbuilder_build_run_phase_node(this, status)
      call report_if_failed(this, 'build_run_phase_node', status)
   end subroutine graphbuilder_run_connect_hook


   ! GENERIC_INIT_ACCEPT_TRANSFER entry point into
   ! graphbuilder_populate_state.
   module subroutine graphbuilder_run_populate_state_hook(this, multi_state)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state

      integer :: status

      call graphbuilder_populate_state(this, multi_state, status)
      call report_if_failed(this, 'populate_state', status)
   end subroutine graphbuilder_run_populate_state_hook

   ! Separate procedure so the check has its own rc to pass to
   ! report_if_failed.
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
