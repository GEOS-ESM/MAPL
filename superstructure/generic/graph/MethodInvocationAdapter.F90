!------------------------------------------------------------------------------
! MethodInvocationAdapter: abstract invocation-adapter type hiding the ESMF
! call-signature difference between a GridComp phase invocation and an
! attached-State callback method invocation behind one deferred invoke()
! entry point (REQ-MTH-001/002). MethodGraphNode (mapl_MethodGraphNode_mod)
! holds exactly one of these and delegates invocation to it unchanged; the
! node never branches on which concrete adapter kind is attached.
!
! Deliberately decoupled from mapl_MethodGraphNode_mod (no dependency on it,
! like Transform's one-directional dependency on TransformGraphNode):
! invoke() takes the current argument declarations/bindings as plain values,
! not a reference back to the owning node.
!
! Also declares the two small injectable abstract interfaces
! (GridCompPhaseInvoker/StateMethodInvoker) that the concrete adapters
! (GridCompMethodInvocation.F90/StateMethodInvocation.F90) delegate to, so
! REQ-MTH-003 ("must not duplicate invocation logic already in
! GriddedComponentDriver") holds by construction: a concrete adapter's
! invoke() only gathers bound arguments and calls the one injected entry
! point. GridCompDriverPhaseInvoker.F90 is the GriddedComponentDriver-backed
! GridCompPhaseInvoker. No ESMF_MethodExecute-backed StateMethodInvoker
! exists yet; tests use synthetic doubles.
!
! gridcomp_invoke_phase_interface takes phase_idx (integer), not a phase
! name, because GriddedComponentDriver%run/initialize/finalize
! (superstructure/component/GriddedComponentDriver.F90) take phase_idx.
!
! DriverResolver: the REQ-MTH-009 "stable local identifier -> driver"
! resolution abstraction. Declared here, not in mapl_OuterMetaComponent_mod,
! so this module stays independent of OuterMetaComponent (only the return
! type, GriddedComponentDriver, is needed). The concrete implementation that
! resolves a driver_key against a real OuterMetaComponent's driver/children
! is in superstructure/generic/OuterMetaComponentDriverResolver.F90.
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

   ! Injected by GridCompMethodInvocation. The GriddedComponentDriver-backed
   ! implementation is GridCompDriverPhaseInvoker.F90.
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

   ! REQ-MTH-009: resolves a stable driver_key to the concrete driver it names,
   ! without the resolved pointer ever being retained by any graph-visible
   ! object. The real implementation (OuterMetaComponentDriverResolver.F90)
   ! resolves against a real OuterMetaComponent's driver/children; a test double
   ! MAY resolve against anything else.
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

   ! Injected by StateMethodInvocation. No clock parameter (REQ-CB-001's
   ! attached State callback methods have no clock argument).
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
