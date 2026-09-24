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
! arguments and call the one injected entry point. Phase 4a supplied
! only synthetic test-double implementations of these two interfaces;
! griddedcomponentdriver-integration-lifecycle (Phase 4b) supplies the
! real GriddedComponentDriver-backed GridCompPhaseInvoker
! (GridCompDriverPhaseInvoker.F90); the real ESMF_MethodExecute-backed
! StateMethodInvoker remains Phase 4c/4d's job.
!
! gridcomp_invoke_phase_interface's phase_idx (integer) fixes a mismatch
! Phase 4a's own design.md flagged in its Risks section: the real driver
! call this must eventually reach (GriddedComponentDriver%run/initialize/
! finalize, superstructure/component/GriddedComponentDriver.F90) takes
! phase_idx : integer, never a phase name - see
! griddedcomponentdriver-integration-lifecycle's design.md Decisions
! ("GridCompMethodInvocation gains phase_idx in place of phase_name").
!
! DriverResolver: the REQ-MTH-009 "stable local identifier -> driver"
! resolution abstraction (griddedcomponentdriver-integration-lifecycle
! design.md Decisions - "A new DriverResolver abstraction decouples the
! real GridCompPhaseInvoker from OuterMetaComponent"). Declared here,
! alongside the other small injection points this module already owns,
! rather than in mapl_OuterMetaComponent_mod, so this module keeps its
! existing independence from OuterMetaComponent - only the return type
! (GriddedComponentDriver) is needed, not anything OuterMetaComponent-
! shaped. The concrete implementation resolving a real driver_key
! against a real OuterMetaComponent's own driver/children lives in
! superstructure/generic/OuterMetaComponentDriverResolver.F90.
!------------------------------------------------------------------------------
module mapl_MethodInvocationAdapter_mod
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_NodeId_mod, only: NodeId
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use ESMF, only: ESMF_Clock
   implicit none(type, external)
   private

   public :: MethodInvocationAdapter
   public :: GridCompPhaseInvoker
   public :: StateMethodInvoker
   public :: DriverResolver

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
   ! entry point" (Q2). Real implementation:
   ! GridCompDriverPhaseInvoker.F90, backed by GriddedComponentDriver's
   ! own initialize/run/finalize calls.
   type, abstract :: GridCompPhaseInvoker
   contains
      procedure(gridcomp_invoke_phase_interface), deferred :: invoke_phase
   end type GridCompPhaseInvoker

   abstract interface
      subroutine gridcomp_invoke_phase_interface(this, driver_key, phase_idx, arguments, bindings, clock, rc)
         import :: GridCompPhaseInvoker, ArgumentSpecMap, StateItemMemberMap, ESMF_Clock
         class(GridCompPhaseInvoker), intent(inout) :: this
         character(*), intent(in) :: driver_key
         integer, intent(in) :: phase_idx
         type(ArgumentSpecMap), intent(in) :: arguments
         type(StateItemMemberMap), intent(in) :: bindings
         type(ESMF_Clock), optional, intent(in) :: clock
         integer, optional, intent(out) :: rc
      end subroutine gridcomp_invoke_phase_interface
   end interface

   ! REQ-MTH-009: resolves a stable driver_key to the concrete driver it
   ! names, without the resolved pointer ever being retained by any
   ! graph-visible object (griddedcomponentdriver-integration-lifecycle
   ! design.md Decisions). The real implementation
   ! (OuterMetaComponentDriverResolver.F90) resolves against a real
   ! OuterMetaComponent's own driver/children; a test double MAY resolve
   ! against anything else entirely.
   type, abstract :: DriverResolver
   contains
      procedure(driver_resolver_resolve_interface), deferred :: resolve
   end type DriverResolver

   abstract interface
      function driver_resolver_resolve_interface(this, driver_key, rc) result(driver)
         import :: DriverResolver, GriddedComponentDriver
         class(DriverResolver), intent(in) :: this
         character(*), intent(in) :: driver_key
         integer, optional, intent(out) :: rc
         class(GriddedComponentDriver), pointer :: driver
      end function driver_resolver_resolve_interface
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
