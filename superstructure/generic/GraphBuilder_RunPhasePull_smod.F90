#include "MAPL.h"

! Run-phase pull of a component's own imports and advance of its own exports.

submodule (mapl_GraphBuilder_mod) GraphBuilder_RunPhasePull_smod
   use esmf, only: operator(==)
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! graphbuilder_populate_state runs once, at init, so nothing otherwise
   ! brings an import fed through a transform up to date after a producer
   ! runs again. Before its own run, a component pulls each of its own
   ! advertised imports through the hierarchy's forward network, which
   ! reaches the producer wherever the connection was declared. A plain
   ! ComponentGraph%update(): it never builds or invokes a MethodGraphNode
   ! and never runs a driver a second time.
   !
   ! No-op outside GRAPH_MODE_GRAPH. Call only from run_user.F90, right
   ! before the user run call.
   module subroutine graphbuilder_run_pull_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(ComponentSpec), pointer :: spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: import_id

      _RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)

      graph => this%get_component_graph()
      spec => this%get_component_spec()
      associate (e => spec%var_specs%ftn_end())
         iter = spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT)) cycle
            import_id => graph%get_resource_index(item_key(ESMF_STATEINTENT_IMPORT, var_spec%short_name))
            if (.not. associated(import_id)) cycle
            call graph%update(import_id, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_run_pull_hook

   ! After a successful run, advances the revision of each of this
   ! component's own advertised exports, so a transform fed by one
   ! re-executes on its next pull. No-op outside GRAPH_MODE_GRAPH. Call
   ! only from run_user.F90, right after the user run call succeeds.
   module subroutine graphbuilder_run_advance_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(ComponentSpec), pointer :: spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: export_id
      class(GraphNode), pointer :: node

      _RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)

      graph => this%get_component_graph()
      spec => this%get_component_spec()
      associate (e => spec%var_specs%ftn_end())
         iter = spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle
            export_id => graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, var_spec%short_name))
            if (.not. associated(export_id)) cycle
            node => graph%get_node(export_id)
            if (.not. associated(node)) cycle
            select type (node)
            class is (StateItemNode)
               call node%advance_revision(_RC)
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_run_advance_hook

end submodule GraphBuilder_RunPhasePull_smod
