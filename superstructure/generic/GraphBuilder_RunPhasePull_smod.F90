#include "MAPL.h"

! openspec/changes/graph-child-state-population, design.md D7: re-propagation pull
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_RunPhasePull_smod
   use esmf, only: operator(==)
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Found during implementation of this change's own YAML-configured
   ! demonstration scenario (proposal.md Why; design.md D7):
   ! graphbuilder_populate_state only ever runs once, at
   ! GENERIC_INIT_ACCEPT_TRANSFER (init time) - nothing re-populates a
   ! child's own real import state after a later run phase changes the
   ! value feeding an extension-chain transform. This hook closes that
   ! gap WITHOUT reopening Phase 10d's own double-invocation hazard: it
   ! calls `ComponentGraph%update()` directly on each of `this`
   ! component's own declared children's import proxies - the SAME
   ! demand-driven update entry point the equivalence harness/callback
   ! wiring already use - never constructing or invoking a
   ! MethodGraphNode, never calling a GriddedComponentDriver%run() a
   ! second time. method-graph-node capability's own REQ-NODE-007
   ! ("Method node execution is never scheduled by demand-driven
   ! update") already guarantees graph%update()'s own dispatch
   ! (ComponentGraph_DemandDrivenUpdate.F90's class default branch)
   ! skips anything that is not a StateItemNode/TransformGraphNode - a
   ! MethodGraphNode is categorically unreachable from this call, by
   ! construction, not merely by convention.
   !
   ! A no-op outside GRAPH_MODE_GRAPH, matching every other new code
   ! path this change adds. MUST be called only from run_user.F90's
   ! own GENERIC::RUN_USER-phase body, immediately after the existing
   ! legacy run call - mirrors graphbuilder_build_run_phase_node's own
   ! header-comment convention for documenting its one intended call
   ! site.
   module subroutine graphbuilder_run_pull_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(ComponentGraph), pointer :: child_graph
      type(ComponentSpec), pointer :: child_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: import_id
      character(:), allocatable :: child_name
      integer :: n, i

      _RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)

      graph => this%get_component_graph()

      n = this%get_num_children()
      do i = 1, n
         child_name = this%get_child_name(i, _RC)
         child_spec => this%get_child_component_spec(child_name, _RC)
         ! openspec/changes/hierarchy-wide-component-graph, tasks.md
         ! 3.1/3.2: the child's own real import NodeId, in the child's
         ! own graph - no parent-local proxy exists anymore. The
         ! dependency edge feeding it still lives in `this`'s own
         ! default network (graph%update's own network_id argument,
         ! below), since `this` is the component that declared the
         ! connection; graph%update() finds that edge correctly
         ! regardless of which ComponentGraph instance's own
         ! get_resource_index() resolved the starting NodeId, because
         ! node/network storage is shared hierarchy-wide.
         child_graph => this%get_child_component_graph(child_name, _RC)
         associate (e => child_spec%var_specs%ftn_end())
            iter = child_spec%var_specs%ftn_begin()
            do while (iter /= e)
               call iter%next()
               var_spec => iter%of()
               if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT)) cycle
               import_id => child_graph%get_resource_index(item_key(ESMF_STATEINTENT_IMPORT, var_spec%short_name))
               if (.not. associated(import_id)) cycle
               call graph%update(graph%get_default_network_id(), import_id, _RC)
            end do
         end associate
      end do

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_run_pull_hook

end submodule GraphBuilder_RunPhasePull_smod
