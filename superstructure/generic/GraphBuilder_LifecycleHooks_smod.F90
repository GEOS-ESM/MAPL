#include "MAPL.h"

! Lifecycle-hook wrappers. Each reports failures via report_if_failed
! and continues, so a GraphBuilder defect never breaks initialization.

submodule (mapl_GraphBuilder_mod) GraphBuilder_LifecycleHooks_smod
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
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
    module subroutine graphbuilder_run_connect_hook(this, fallback_warnings)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(GraphResolutionEntryVector), optional, intent(out) :: &
           fallback_warnings

      integer :: status
      type(GraphResolutionEntryVector) :: unsupported, conflicts, warnings
      type(GraphResolutionEntryVector) :: fallback
      type(GraphResolutionEntryVector) :: no_fallback_warnings
      integer :: i
      type(GraphResolutionEntry), pointer :: entry
      class(Logger), pointer :: lgr
      character(:), allocatable :: msg

      if (present(fallback_warnings)) then
         fallback_warnings = no_fallback_warnings
      end if
      lgr => this%get_logger()

      ! A component fills its DEFERRED characteristics in its own init phase;
      ! pick those up so a connection that waited on them can be wired.
      call graphbuilder_harvest_deferred(this, rc=status)
      call report_if_failed(this, 'harvest_deferred', status)

      ! A characteristic still DEFERRED here is one the component never
      ! supplied; most kinds stay DEFERRED (freeze fails on them later), but
      ! one whose is_deferred_fatal() says no (e.g. QuantityType, by default
      ! in either validation_mode) falls back to UNCHECKED here instead.
      call graphbuilder_fallback_unsupplied_deferred(this, fallback, status)
      call report_if_failed(this, 'fallback_unsupplied_deferred', status)
      if (present(fallback_warnings)) fallback_warnings = fallback

      do i = 1, fallback%size()
         entry => fallback%of(i)
         msg = 'GraphBuilder: ' // entry%component_name // ': ' // entry%reason
         call lgr%warning(msg)
      end do

      call graphbuilder_resolve_connections(this, unsupported_characteristics=unsupported, &
           connection_conflicts=conflicts, connection_warnings=warnings, rc=status)
      call report_if_failed(this, 'resolve_connections', status)

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

      do i = 1, warnings%size()
         entry => warnings%of(i)
         msg = 'GraphBuilder: ' // entry%component_name // ':' // entry%short_name // ': ' // entry%reason
         call lgr%warning(msg)
      end do

      ! Argument bindings come only from this component's advertised
      ! items (resolved at ADVERTISE time), so there is no ordering
      ! dependency on freeze. Construction only; see
      ! graphbuilder_build_run_phase_node.
      call graphbuilder_build_run_phase_node(this, status)
      call report_if_failed(this, 'build_run_phase_node', status)
   end subroutine graphbuilder_run_connect_hook

   ! Walks this component's own StateItemNodes (not its children's - each
   ! gets its own run_connect_hook call) for a characteristic still DEFERRED
   ! whose is_deferred_fatal() is false, converts it to UNCHECKED, and
   ! reports one summary GraphResolutionEntry per characteristic kind that
   ! fell back: the count and the first few item names, so the fallback
   ! itself never names a particular kind.
   module subroutine graphbuilder_fallback_unsupplied_deferred(this, &
        warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(GraphResolutionEntryVector), intent(inout) :: warnings
      integer, optional, intent(out) :: rc

      integer :: status, i, g
      type(ComponentGraph), pointer :: graph
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: iter
      class(GraphNode), pointer :: node
      type(StateItemDescriptor), pointer :: descriptor
      type(StateItemCharacteristicKind), allocatable :: fallen_back(:)
      type(StateItemCharacteristicKind), allocatable :: kinds_seen(:)
      type(StringVector), allocatable :: names_by_kind(:)
      character(:), allocatable :: component_name, item_name, reason

      component_name = this%get_name(_RC)
      allocate(kinds_seen(0))
      allocate(names_by_kind(0))

      graph => this%get_component_graph()
      node_ids = graph%get_node_ids()
      associate (e => node_ids%ftn_end())
         iter = node_ids%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node => graph%get_node(iter%of())
            if (.not. associated(node)) cycle
            select type (node)
            class is (StateItemNode)
               descriptor => node%get_descriptor()
               call descriptor%fallback_unsupplied_deferred(fallen_back, _RC)
               if (size(fallen_back) == 0) cycle
               if (node%has_short_name()) then
                  item_name = node%get_short_name()
               else
                  item_name = '<extension>'
               end if
               do i = 1, size(fallen_back)
                  g = index_of_kind(kinds_seen, fallen_back(i))
                  if (g == 0) then
                     kinds_seen = [kinds_seen, fallen_back(i)]
                     call grow_names_by_kind(names_by_kind)
                     g = size(kinds_seen)
                  end if
                  call names_by_kind(g)%push_back(item_name)
               end do
            end select
         end do
      end associate

      do g = 1, size(kinds_seen)
         reason = summarize_fallback(kinds_seen(g), names_by_kind(g))
         call warnings%push_back( &
              GraphResolutionEntry(component_name, '', reason))
      end do

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_fallback_unsupplied_deferred

   integer function index_of_kind(kinds, kind) result(idx)
      type(StateItemCharacteristicKind), intent(in) :: kinds(:)
      type(StateItemCharacteristicKind), intent(in) :: kind

      integer :: i

      idx = 0
      do i = 1, size(kinds)
         if (kinds(i) == kind) then
            idx = i
            return
         end if
      end do
   end function index_of_kind

   ! gfortran/NAG both grow a plain array via `arr = [arr, new]`, but a
   ! StringVector element makes that combinator unreliable; grow by explicit
   ! copy instead (same idiom as CsvProfileReporter%add_column).
   subroutine grow_names_by_kind(names)
      type(StringVector), allocatable, intent(inout) :: names(:)

      type(StringVector), allocatable :: temp(:)
      integer :: n

      n = size(names)
      allocate(temp(n))
      temp(:) = names(:)
      deallocate(names)
      allocate(names(n + 1))
      names(1:n) = temp(:)
   end subroutine grow_names_by_kind

   ! "<n> items fell back to UNCHECKED (<KIND> not supplied): <first 5
   ! names>[, ...]".
   function summarize_fallback(kind, names) result(reason)
      type(StateItemCharacteristicKind), intent(in) :: kind
      type(StringVector), intent(in) :: names
      character(:), allocatable :: reason

      integer :: n, shown, i
      character(:), allocatable :: item_list
      character(20) :: count_buffer

      n = names%size()
      shown = min(5, n)
      item_list = ''
      do i = 1, shown
         if (i > 1) item_list = item_list // ', '
         item_list = item_list // names%of(i)
      end do
      if (n > shown) item_list = item_list // ', ...'

      write(count_buffer, '(I0)') n
      reason = trim(count_buffer) // ' items fell back to UNCHECKED (' // &
           kind%to_string() // ' not supplied): ' // item_list
   end function summarize_fallback

   ! GENERIC_INIT_ACCEPT_TRANSFER entry point into
   ! graphbuilder_populate_state.
   module subroutine graphbuilder_run_populate_state_hook(this, &
        multi_state, own_only)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      logical, optional, intent(in) :: own_only

      integer :: status

      call graphbuilder_populate_state(this, multi_state, status, own_only)
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
