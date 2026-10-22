#include "MAPL.h"

! Validate and freeze.

submodule (mapl_GraphBuilder_mod) GraphBuilder_Freeze_smod
   use mapl_ErrorHandling_mod
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use pflogger, only: Logger
   implicit none(type, external)

contains

   ! Thin wrapper: ComponentGraph%freeze() validates first and sets the
   ! frozen flag only on success.
   module subroutine graphbuilder_freeze(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph

      graph => this%get_component_graph()
      call graph%freeze(_RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_freeze

   ! GENERIC_INIT_GRAPH_FREEZE entry point. The caller
   ! (initialize_graph_freeze.F90) checks is_hierarchy_root, so only the
   ! true root does real work; this procedure does not re-check.
   ! Makes two separate walks of the component tree: first freeze every
   ! component's graph, then collect unresolved required imports and
   ! fail if any remain. Two walks because a component's unresolved
   ! imports can depend on a sibling subtree's mirrored exports/imports,
   ! so convergence is checked only once every graph is frozen.
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

   ! Freezes `this`'s graph, then recurses into every child. A plain
   ! recursive walk, not an ESMF phase dispatch or this%recurse(): it is
   ! called once from the root's non-recursive freeze phase handler. A
   ! failure is reported and that subtree skipped, so a GraphBuilder
   ! defect never aborts component initialization.
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

   ! Collects every component's unresolved required imports over the
   ! whole tree. Must run after freeze_subtree_ (see
   ! graphbuilder_run_graph_freeze_hook).
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

