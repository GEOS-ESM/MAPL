!------------------------------------------------------------------------------
! MethodInvocationAdapter: abstract invocation-adapter type hiding the
! ESMF call-signature difference between a GridComp phase invocation and
! an attached-State callback method invocation behind one deferred
! invoke() entry point (spec/12-methods-and-drivers.md REQ-MTH-001/002,
! spec/17-open-questions.md Q2). MethodGraphNode (mapl_MethodGraphNode_mod)
! holds exactly one of these and delegates invocation to it unchanged -
! the node itself never branches on, or otherwise needs to know, which
! concrete adapter kind is attached.
!
! Deliberately decoupled from mapl_MethodGraphNode_mod (this module has
! no dependency on it, matching Transform's own one-directional
! dependency on TransformGraphNode, Transform.F90) - invoke() takes the
! current argument declarations/bindings as plain values, not a
! reference back to the owning node.
!
! Also declares the two small injectable abstract interfaces
! (GridCompPhaseInvoker/StateMethodInvoker) that the two concrete
! adapters (GridCompMethodInvocation.F90/StateMethodInvocation.F90)
! delegate to, so REQ-MTH-003 ("must not duplicate invocation logic
! already in GriddedComponentDriver") is satisfied by construction: a
! concrete adapter's own invoke() does nothing but gather bound
! arguments and call the one injected entry point. This change supplies
! only synthetic test-double implementations of these two interfaces;
! Phase 4b/4c supply the real GriddedComponentDriver-backed/
! ESMF_MethodExecute-backed implementations (design.md Decisions).
!------------------------------------------------------------------------------
module mapl_MethodInvocationAdapter_mod
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_NodeId_mod, only: NodeId
   use ESMF, only: ESMF_Clock
   implicit none(type, external)
   private

   public :: MethodInvocationAdapter
   public :: GridCompPhaseInvoker
   public :: StateMethodInvoker

   type, abstract :: MethodInvocationAdapter
   contains
      procedure(adapter_invoke_interface), deferred :: invoke
   end type MethodInvocationAdapter

   abstract interface
      subroutine adapter_invoke_interface(this, arguments, bindings, clock, rc)
         import :: MethodInvocationAdapter, ArgumentSpecMap, StateItemMemberMap, ESMF_Clock
         class(MethodInvocationAdapter), intent(inout) :: this
         type(ArgumentSpecMap), intent(in) :: arguments
         type(StateItemMemberMap), intent(in) :: bindings
         type(ESMF_Clock), optional, intent(in) :: clock
         integer, optional, intent(out) :: rc
      end subroutine adapter_invoke_interface
   end interface

   ! Injected by GridCompMethodInvocation - "the one existing driver
   ! entry point" (Q2) a real Phase 4b implementation backs with
   ! GriddedComponentDriver's own initialize/run/finalize calls.
   type, abstract :: GridCompPhaseInvoker
   contains
      procedure(gridcomp_invoke_phase_interface), deferred :: invoke_phase
   end type GridCompPhaseInvoker

   abstract interface
      subroutine gridcomp_invoke_phase_interface(this, driver_key, phase_name, arguments, bindings, clock, rc)
         import :: GridCompPhaseInvoker, ArgumentSpecMap, StateItemMemberMap, ESMF_Clock
         class(GridCompPhaseInvoker), intent(inout) :: this
         character(*), intent(in) :: driver_key
         character(*), intent(in) :: phase_name
         type(ArgumentSpecMap), intent(in) :: arguments
         type(StateItemMemberMap), intent(in) :: bindings
         type(ESMF_Clock), optional, intent(in) :: clock
         integer, optional, intent(out) :: rc
      end subroutine gridcomp_invoke_phase_interface
   end interface

   ! Injected by StateMethodInvocation - "the one existing attachment
   ! entry point" (Q2) a real Phase 4c/4d implementation backs with
   ! ESMF_MethodExecute. No clock parameter (REQ-CB-001's attached State
   ! callback methods have no clock argument).
   type, abstract :: StateMethodInvoker
   contains
      procedure(state_invoke_method_interface), deferred :: invoke_method
   end type StateMethodInvoker

   abstract interface
      subroutine state_invoke_method_interface(this, state_node_id, method_name, arguments, bindings, rc)
         import :: StateMethodInvoker, NodeId, ArgumentSpecMap, StateItemMemberMap
         class(StateMethodInvoker), intent(inout) :: this
         type(NodeId), intent(in) :: state_node_id
         character(*), intent(in) :: method_name
         type(ArgumentSpecMap), intent(in) :: arguments
         type(StateItemMemberMap), intent(in) :: bindings
         integer, optional, intent(out) :: rc
      end subroutine state_invoke_method_interface
   end interface

end module mapl_MethodInvocationAdapter_mod
