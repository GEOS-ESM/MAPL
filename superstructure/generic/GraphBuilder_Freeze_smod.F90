#include "MAPL.h"

! Task 5: Validate and freeze
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_Freeze_smod
   use mapl_ErrorHandling_mod
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use pflogger, only: Logger
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

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md Group 9
   ! (design.md D7, "Freeze becomes one hierarchy-wide call"): the
   ! GENERIC_INIT_GRAPH_FREEZE phase's own entry point
   ! (OuterMetaComponent/initialize_graph_freeze.F90 calls this
   ! unconditionally; THAT file's own is_hierarchy_root check is what
   ! makes the overall effect "only the true root does real work" -
   ! this procedure itself does not re-check root-ness, trusting its
   ! only caller). Performs exactly ONE explicit, non-ESMF-recursive
   ! walk of the whole component tree rooted at `this` (freeze_subtree_,
   ! below), freezing every component's own ComponentGraph exactly
   ! once, THEN walks the tree a second time to collect every
   ! still-unresolved required import anywhere in the hierarchy and
   ! fails hard if any remain - the direct hierarchy-wide replacement
   ! for graphbuilder_run_connect_hook's own former per-component
   ! "freeze, then immediately check that one component's own imports"
   ! sequence (GraphBuilder_LifecycleHooks_smod.F90, see that file's own
   ! updated header comment on graphbuilder_run_connect_hook for why
   ! per-component freeze-then-check was unsafe once imports/exports can
   ! be mirrored across components within a single ACCEPT_TRANSFER
   ! pass).
   !
   ! Two separate tree walks (freeze everywhere, THEN check everywhere)
   ! rather than one combined walk: a component's own unresolved-import
   ! report can legitimately depend on a SIBLING subtree's own mirrored
   ! exports/imports (the same cross-subtree concern
   ! graphbuilder_run_propagate_hook's own header comment documents for
   ! import mirroring) - checking convergence only after every
   ! component's graph is frozen (hence structurally complete) avoids
   ! any walk-order sensitivity in the convergence result.
   module subroutine graphbuilder_run_graph_freeze_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(GraphResolutionEntryVector) :: unresolved

      call freeze_subtree_(this, status)
      call report_if_failed(this, 'graph_freeze:freeze_subtree', status)

      call collect_unresolved_subtree_(this, unresolved, status)
      call report_if_failed(this, 'graph_freeze:collect_unresolved_subtree', status)
      if (status == 0) then
         call assert_converged(unresolved, status)
         call report_if_failed(this, 'graph_freeze:convergence_check', status)
      end if
   end subroutine graphbuilder_run_graph_freeze_hook

   ! Freezes `this`'s own ComponentGraph, then recurses into every
   ! direct child - an explicit, ordinary Fortran recursive walk (NOT an
   ! ESMF phase dispatch / NOT this%recurse()'s own
   ! GridCompInitialize-based recursion), since this is called exactly
   ! once, from the true hierarchy root's own non-recursive
   ! GENERIC_INIT_GRAPH_FREEZE phase handler, which by construction
   ! never fires for any non-root component (see
   ! initialize_graph_freeze.F90). Any single component's own freeze
   ! failure is reported (report_if_failed convention) and that one
   ! component's subtree is skipped - matching every other hook in this
   ! module's existing "never let a GraphBuilder defect abort real
   ! component initialization" convention (design.md Risks).
   recursive subroutine freeze_subtree_(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta

      graph => this%get_component_graph()
      call graph%freeze(status)
      call report_if_failed(this, 'graph_freeze:freeze', status)
      _RETURN_IF(status /= 0)

      num_children = this%get_num_children()
      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'graph_freeze:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'graph_freeze:get_child_outer_meta', status)
         if (status /= 0) cycle

         call freeze_subtree_(child_meta, status)
      end do

      _RETURN(_SUCCESS)
   end subroutine freeze_subtree_

   ! Collects every component's own still-unresolved required import,
   ! walking the whole tree rooted at `this` exactly once - the
   ! hierarchy-wide replacement for graphbuilder_run_connect_hook's own
   ! former single, per-component graphbuilder_check_unsatisfied_imports
   ! call (see graphbuilder_run_graph_freeze_hook's own header comment
   ! for why this must run as a SECOND, separate walk, after every
   ! component is already frozen).
   recursive subroutine collect_unresolved_subtree_(this, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphResolutionEntryVector) :: this_unresolved
      integer :: i, j, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(GraphResolutionEntry), pointer :: entry

      call graphbuilder_check_unsatisfied_imports(this, unresolved_imports=this_unresolved, rc=status)
      call report_if_failed(this, 'graph_freeze:check_unsatisfied_imports', status)
      if (status == 0) then
         do j = 1, this_unresolved%size()
            entry => this_unresolved%of(j)
            call unresolved%push_back(entry)
         end do
      end if

      num_children = this%get_num_children()
      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'graph_freeze:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'graph_freeze:get_child_outer_meta', status)
         if (status /= 0) cycle

         call collect_unresolved_subtree_(child_meta, unresolved, status)
      end do

      _RETURN(_SUCCESS)
   end subroutine collect_unresolved_subtree_

end submodule GraphBuilder_Freeze_smod

