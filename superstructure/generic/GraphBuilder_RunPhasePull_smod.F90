#include "MAPL.h"

! Run-phase re-propagation pull for children's import states.

submodule (mapl_GraphBuilder_mod) GraphBuilder_RunPhasePull_smod
   use esmf, only: operator(==)
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! graphbuilder_populate_state runs once, at init, so nothing otherwise
   ! re-populates a child's import state after a later run phase changes
   ! the value feeding an extension-chain transform. This hook calls
   ! ComponentGraph%update() on each child's import nodes. It never
   ! builds or invokes a MethodGraphNode (and never runs a driver a
   ! second time): update() skips anything that is not a
   ! StateItemNode/TransformGraphNode.
   !
   ! No-op outside GRAPH_MODE_GRAPH. Call only from run_user.F90's
   ! GENERIC::RUN_USER body, immediately after the legacy run call.
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
         ! The import NodeId lives in the child's graph, but the edge
         ! feeding it lives in `this`'s default network (`this` declared
         ! the connection). update() still finds it because node/network
         ! storage is shared hierarchy-wide.
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
