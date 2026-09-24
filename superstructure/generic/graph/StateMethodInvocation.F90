#include "MAPL.h"

!------------------------------------------------------------------------------
! StateMethodInvocation: MethodInvocationAdapter concrete subtype
! modeling an attached ESMF State callback method invocation
! (spec/12-methods-and-drivers.md REQ-MTH-001, spec/15-callbacks.md
! REQ-CB-001). Carries the callback State's NodeId and a method name;
! delegates to an injected StateMethodInvoker rather than calling
! anything ESMF-specific itself, satisfying REQ-MTH-003 by construction.
!
! This change supplies only a synthetic test-double StateMethodInvoker
! (superstructure/generic/graph/tests/Test_StateMethodInvocation.pf).
! Phase 4c/4d supply the real ESMF_MethodExecute-backed implementation
! once CallbackInterfaceRegistry/CallbackStateBinding exist - not built
! here (design.md Decisions).
!
! invoke() accepts a clock argument for interface uniformity with
! MethodInvocationAdapter, but ignores it - REQ-CB-001's attached State
! callback methods have no clock parameter, so StateMethodInvoker%
! invoke_method() has none either.
!------------------------------------------------------------------------------
module mapl_StateMethodInvocation_mod
   use mapl_MethodInvocationAdapter_mod, only: MethodInvocationAdapter, StateMethodInvoker
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_NodeId_mod, only: NodeId
   use ESMF, only: ESMF_Clock
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateMethodInvocation

   type, extends(MethodInvocationAdapter) :: StateMethodInvocation
      private
      type(NodeId) :: state_node_id
      character(:), allocatable :: method_name
      class(StateMethodInvoker), allocatable :: invoker
   contains
      procedure :: invoke => state_method_invoke
      procedure :: get_state_node_id => state_method_get_state_node_id
      procedure :: get_method_name => state_method_get_method_name
   end type StateMethodInvocation

   interface StateMethodInvocation
      module procedure new_StateMethodInvocation
   end interface StateMethodInvocation

contains

   ! invoker is optional so a caller (or a test exercising the
   ! not-yet-attached case) MAY construct an adapter with no invoker
   ! attached yet; invoke() then fails loudly (below) rather than
   ! silently doing nothing.
   function new_StateMethodInvocation(state_node_id, method_name, invoker) result(adapter)
      type(NodeId), intent(in) :: state_node_id
      character(*), intent(in) :: method_name
      class(StateMethodInvoker), optional, intent(in) :: invoker
      type(StateMethodInvocation) :: adapter

      adapter%state_node_id = state_node_id
      adapter%method_name = method_name
      if (present(invoker)) allocate(adapter%invoker, source=invoker)
   end function new_StateMethodInvocation

   function state_method_get_state_node_id(this) result(state_node_id)
      class(StateMethodInvocation), intent(in) :: this
      type(NodeId) :: state_node_id

      state_node_id = this%state_node_id
   end function state_method_get_state_node_id

   function state_method_get_method_name(this) result(method_name)
      class(StateMethodInvocation), intent(in) :: this
      character(:), allocatable :: method_name

      method_name = this%method_name
   end function state_method_get_method_name

   ! REQ-MTH-003: gathers bound arguments, calls the one injected
   ! StateMethodInvoker entry point, returns. Nothing else. clock is
   ! accepted but unused (no clock in the underlying call, see header).
   subroutine state_method_invoke(this, arguments, bindings, clock, rc)
      class(StateMethodInvocation), intent(inout) :: this
      type(ArgumentSpecMap), intent(in) :: arguments
      type(StateItemMemberMap), intent(in) :: bindings
      type(ESMF_Clock), optional, intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(allocated(this%invoker), 'StateMethodInvocation: invoke called with no StateMethodInvoker attached')

      call this%invoker%invoke_method(this%state_node_id, this%method_name, arguments, bindings, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine state_method_invoke

end module mapl_StateMethodInvocation_mod
