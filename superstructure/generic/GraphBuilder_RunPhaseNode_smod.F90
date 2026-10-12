#include "MAPL.h"

! openspec/changes/phase-10d-ordinary-phase-method-node: ordinary-phase MethodGraphNode construction
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

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

   ! ============================================================
   ! openspec/changes/phase-10d-ordinary-phase-method-node: ordinary-
   ! phase MethodGraphNode construction.
   ! ============================================================
   !
   ! CONSTRUCTION ONLY - this procedure never invokes the node it
   ! builds, and no other code in this repo does either. A real,
   ! driver-targeting adapter is attached (design.md D4) so the node is
   ! not permanently uninvocable, but calling invoke()/
   ! invoke_on_default_network() on it today would execute the user's
   ! own GridComp run phase a SECOND time, back-to-back with
   ! run_user.F90's own existing legacy call on the same driver, same
   ! phase - a real double-invocation of production user code, not
   ! inert bookkeeping (proposal.md Why). Making that safe requires (a)
   ! a legacy-run-call skip mechanism (mirroring
   ! initialize_accept_transfer.F90's own GRAPH_MODE_GRAPH skip for
   ! add_to_states, but for the run call - does not exist yet) and (b)
   ! GRAPH_MODE_GRAPH-alone population actually working end-to-end
   ! (confirmed broken today, roadmap §20.4.8's own "Explicitly out of
   ! scope"). Do not add an invocation call site against this node
   ! without resolving both first.
   !
   ! Builds exactly one run-phase MethodGraphNode for `this` component,
   ! wrapping its own GENERIC::RUN_USER phase, and stores the
   ! constructed node's NodeId on `this` (OuterMetaComponent%
   ! set_run_phase_method_node_id). Keyed to the component's own
   ! advertised import/export VariableSpecs (design.md D3), not a
   ! CallbackInterface's declared arguments - mirrors
   ! build_callback_method_binding's own declare_argument/bind_argument/
   ! register_node construction shape, differing only in where the
   ! argument list comes from. A no-op under the default (registry)
   ! graph mode, and idempotent (re-running after a NodeId is already
   ! stored does nothing further) - matches advertise_one's own
   ! re-advertisement guard, since graphbuilder_run_connect_hook may run
   ! more than once per the fixed two-pass init schedule
   ! (GENERIC_INIT_PHASE_SEQUENCE).
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

      ! Idempotent: graphbuilder_run_connect_hook (this procedure's own
      ! caller) may run more than once per the fixed two-pass
      ! GENERIC_INIT_PHASE_SEQUENCE schedule - a NodeId already stored
      ! from an earlier pass means this component's run-phase node
      ! already exists, matching advertise_one's own "already indexed -
      ! nothing to do" guard. NodeId%is_valid() (not association) is
      ! the correct "already set" test - get_run_phase_method_node_id
      ! returns by value (OuterMetaComponent.F90's own field-declaration
      ! comment: a NodeId is a tiny value type, not something worth a
      ! pointer-and-null-check accessor).
      existing_node_id = this%get_run_phase_method_node_id()
      if (existing_node_id%is_valid()) then
         _RETURN(_SUCCESS)
      end if

      graph => this%get_component_graph()
      comp_spec => this%get_component_spec()

      method_node_id = graph%next_node_id(_RC)
      resolver = OuterMetaComponentDriverResolver(this)
      invoker = GridCompDriverPhaseInvoker(resolver, ESMF_METHOD_RUN)
      ! driver_key = SELF_COMPONENT_NAME: this node always targets
      ! `this` component's own driver (never a child's) - phase_idx = 1,
      ! the fixed GENERIC::RUN_USER lifecycle-phase index every
      ! configuration in this repo's own fixture set uses today. A real
      ! adapter is attached so the node is not permanently uninvocable
      ! (design.md D4) - see this procedure's own header comment for
      ! why nothing calls invoke() on it yet.
      method_node = MethodGraphNode(method_node_id, &
           GridCompMethodInvocation(SELF_COMPONENT_NAME, 1, invoker))

      ! Not registered yet (same ordering build_callback_method_binding
      ! itself follows): declare_argument/bind_argument run on this
      ! local value before it is handed to register_node(), which
      ! copies it into the graph's own node map.
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
            ! INTERNAL items: skipped (neither IN nor OUT at the
            ! component boundary - mirrors advertise_one's own
            ! "INTERNAL items get neither [port]" treatment).
         end do
      end associate

      call graph%register_node(method_node, _RC)
      call this%set_run_phase_method_node_id(method_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_build_run_phase_node

end submodule GraphBuilder_RunPhaseNode_smod
