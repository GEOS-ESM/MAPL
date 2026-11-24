#include "MAPL.h"

!------------------------------------------------------------------------------
! EsmfStateMethodInvoker: the ESMF_MethodExecute-backed StateMethodInvoker
! (MethodInvocationAdapter.F90). Resolves state_node_id to the real
! ESMF_State already attached to its StateItemNode (filled by the time any
! method can run) and calls ESMF_MethodExecute directly.
! arguments/bindings are accepted (the StateMethodInvoker interface) but
! unused: the ESMF attached-method signature (I_CallBackMethod,
! infrastructure/esmf/ESMF_Interfaces.F90) takes only the State and an rc,
! the same reason StateMethodInvocation%invoke() already ignores its own
! clock argument for this adapter kind.
!
! Holds a ComponentGraph pointer directly rather than an injected resolver
! like GridCompDriverPhaseInvoker's DriverResolver: ComponentGraph is
! already graph-neutral (docs/graph/CONVENTIONS.md section 2), so no
! cross-layering abstraction is needed to reach it from a StateMethodInvoker
! that lives in the same graph-neutral tree.
!
! existflag is not passed to ESMF_MethodExecute: an unattached method name
! is a caller-ordering bug (the interface declared the method; nothing
! bound it to this State), surfaced as ESMF's own default non-existflag
! failure rather than silently doing nothing.
!------------------------------------------------------------------------------
module mapl_EsmfStateMethodInvoker_mod
   use ESMF, only: ESMF_State, ESMF_MethodExecute
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_MethodInvocationAdapter_mod, only: StateMethodInvoker
   use mapl_NodeId_mod, only: NodeId
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: EsmfStateMethodInvoker

   type, extends(StateMethodInvoker) :: EsmfStateMethodInvoker
      private
      type(ComponentGraph), pointer :: graph => null()
   contains
      procedure :: invoke_method => esmf_invoke_method
   end type EsmfStateMethodInvoker

   interface EsmfStateMethodInvoker
      module procedure new_EsmfStateMethodInvoker
   end interface EsmfStateMethodInvoker

contains

   ! graph MUST be a pointer to the persistent ComponentGraph a real
   ! component owns (OuterMetaComponent%get_component_graph()), not a
   ! throwaway value copy: invoke_method runs at RUN phase, long after the
   ! graph that built this invoker was constructed, and ComponentGraph's own
   ! lifecycle_status/own_node_ids are tracked per instance, not through the
   ! shared hierarchy storage its node/network lookups otherwise read from.
   function new_EsmfStateMethodInvoker(graph) result(invoker)
      type(ComponentGraph), pointer, intent(in) :: graph
      type(EsmfStateMethodInvoker) :: invoker

      invoker%graph => graph
   end function new_EsmfStateMethodInvoker

   subroutine esmf_invoke_method(this, state_node_id, method_name, arguments, bindings, rc)
      class(EsmfStateMethodInvoker), intent(inout) :: this
      type(NodeId), intent(in) :: state_node_id
      character(*), intent(in) :: method_name
      type(ArgumentSpecMap), intent(in) :: arguments
      type(StateItemMemberMap), intent(in) :: bindings
      integer, optional, intent(out) :: rc

      integer :: status, userRc
      class(GraphNode), pointer :: generic_node
      type(GraphStateItem), pointer :: payload
      type(ESMF_State) :: state
      character(:), allocatable :: msg

      _ASSERT(associated(this%graph), 'EsmfStateMethodInvoker: invoke_method - no ComponentGraph attached')

      generic_node => this%graph%get_node(state_node_id)
      msg = 'EsmfStateMethodInvoker: invoke_method - callback State NodeId not found for method: ' // method_name
      _ASSERT(associated(generic_node), msg)
      select type (generic_node)
      class is (StateItemNode)
         payload => generic_node%get_payload_ptr()
      class default
         _FAIL('EsmfStateMethodInvoker: invoke_method - callback State NodeId is not a StateItemNode: ' // method_name)
      end select

      state = payload%get_state(_RC)

      call ESMF_MethodExecute(state, label=method_name, userRc=userRc, rc=status)
      _VERIFY(status)
      _VERIFY(userRc)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(arguments)
      _UNUSED_DUMMY(bindings)
   end subroutine esmf_invoke_method

end module mapl_EsmfStateMethodInvoker_mod
