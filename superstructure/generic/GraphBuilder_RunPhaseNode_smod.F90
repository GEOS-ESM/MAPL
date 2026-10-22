#include "MAPL.h"

! Ordinary-phase MethodGraphNode construction.

submodule (mapl_GraphBuilder_mod) GraphBuilder_RunPhaseNode_smod
   use esmf, only: operator(==)
   use ESMF, only: ESMF_METHOD_RUN
   use mapl_AccessSpec_mod, only: MAPL_ACCESS_IN, MAPL_ACCESS_OUT
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GridCompDriverPhaseInvoker_mod, only: GridCompDriverPhaseInvoker
   use mapl_GridCompMethodInvocation_mod, only: GridCompMethodInvocation
   use mapl_MethodGraphNode_mod, only: MethodGraphNode
   use mapl_OuterMetaComponentDriverResolver_mod, only: OuterMetaComponentDriverResolver
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Builds one run-phase MethodGraphNode for `this`, wrapping its
   ! GENERIC::RUN_USER phase, and stores its NodeId on `this`. Arguments
   ! come from the component's advertised import/export VariableSpecs.
   ! No-op in registry graph mode; idempotent, since
   ! graphbuilder_run_connect_hook may run more than once per the
   ! two-pass init schedule.
   !
   ! CONSTRUCTION ONLY: nothing may invoke this node. Doing so would run
   ! the user's run phase a second time after run_user.F90's legacy
   ! call. Safe invocation needs (a) a graph-mode skip of the legacy run
   ! call and (b) graph-only population working end to end; neither
   ! exists yet.
   module subroutine graphbuilder_build_run_phase_node(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeId) :: existing_node_id
      type(ComponentSpec), pointer :: comp_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId) :: method_node_id
      type(MethodGraphNode) :: method_node
      type(OuterMetaComponentDriverResolver) :: resolver
      type(GridCompDriverPhaseInvoker) :: invoker
      type(NodeId), pointer :: member_id
      character(:), allocatable :: msg

      _RETURN_IF(graph_mode() == GRAPH_MODE_REGISTRY)

      ! get_run_phase_method_node_id returns a NodeId by value, so test
      ! is_valid(), not association.
      existing_node_id = this%get_run_phase_method_node_id()
      if (existing_node_id%is_valid()) then
         _RETURN(_SUCCESS)
      end if

      graph => this%get_component_graph()
      comp_spec => this%get_component_spec()

      method_node_id = graph%next_node_id(_RC)
      resolver = OuterMetaComponentDriverResolver(this)
      invoker = GridCompDriverPhaseInvoker(resolver, ESMF_METHOD_RUN)
      ! The node always targets `this` component's own driver, never a
      ! child's. phase_idx = 1 is the GENERIC::RUN_USER phase index used
      ! by every configuration in the fixtures.
      method_node = MethodGraphNode(method_node_id, &
           GridCompMethodInvocation(SELF_COMPONENT_NAME, 1, invoker))

      ! Declare/bind on the local value before register_node(), which
      ! copies it into the graph's node map.
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()

            if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT) then
               member_id => graph%get_resource_index(item_key(var_spec%state_intent, var_spec%short_name))
               msg = 'GraphBuilder: build_run_phase_node - advertised import has no resolved NodeId: ' // var_spec%short_name
               _ASSERT(associated(member_id), msg)
               call method_node%declare_argument(var_spec%short_name, MAPL_ACCESS_IN, rc=status)
               _VERIFY(status)
               call method_node%bind_argument(var_spec%short_name, member_id, rc=status)
               _VERIFY(status)
            else if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
               member_id => graph%get_resource_index(item_key(var_spec%state_intent, var_spec%short_name))
               msg = 'GraphBuilder: build_run_phase_node - advertised export has no resolved NodeId: ' // var_spec%short_name
               _ASSERT(associated(member_id), msg)
               call method_node%declare_argument(var_spec%short_name, MAPL_ACCESS_OUT, rc=status)
               _VERIFY(status)
               call method_node%bind_argument(var_spec%short_name, member_id, rc=status)
               _VERIFY(status)
            end if
            ! INTERNAL items are neither IN nor OUT at the component
            ! boundary, so they are skipped.
         end do
      end associate

      call graph%register_node(method_node, _RC)
      call this%set_run_phase_method_node_id(method_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_build_run_phase_node

end submodule GraphBuilder_RunPhaseNode_smod
