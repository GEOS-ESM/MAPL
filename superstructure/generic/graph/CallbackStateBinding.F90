#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackStateBinding: explicit storage of one callback State's realized
! contract (spec/15-callbacks.md REQ-CB-007) - the CallbackInterfaceId it
! implements, the NodeId of the callback State's own StateItemNode, an
! argument name -> member NodeId map (reusing the existing
! StateItemMemberMap type unchanged), and a method name -> method
! attachment map (reusing the existing StateMethodInvocation adapter
! type unchanged, callback-data-model-registry design.md Decision 5).
!
! Constructed with a CallbackInterfaceId and a CallbackInterface value
! together (rather than resolving the interface itself from a
! CallbackInterfaceRegistry lookup) - the caller (a test today; Phase
! 4d's GraphBuilder wiring later) is expected to have already resolved
! the id via the registry, the same "caller resolves, this module just
! validates the result" split MethodGraphNode.bind_argument already uses
! for kind-constraint checks (design.md Decision 6). bind_argument/
! bind_method both reject a name outside that interface's own
! declarations (spec scenarios "Binding an argument/method name outside
! the interface is rejected").
!
! Does not itself resolve member NodeIds from a real composite callback
! State's declared member tree - member NodeIds and method invokers are
! caller-supplied, matching MethodGraphNode's own established scope
! (proposal.md - Why, design.md Non-Goals).
!
! invoke_method() materializes a fresh ArgumentSpecMap at call time from
! the bound interface's own CallbackArgumentSpec (expected kind) and the
! target method's own CallbackMethodSpec (per-method AccessSpec) -
! required because StateMethodInvocation%invoke() (Phase 4a) takes an
! ArgumentSpecMap, and REQ-CB-004's per-method access model means the
! same argument can carry a different AccessSpec for a different method
! on the same binding, so no single persisted ArgumentSpecMap could
! serve every method.
!------------------------------------------------------------------------------
module mapl_CallbackStateBinding_mod
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_AccessSpecMap_mod, only: AccessSpecMap, AccessSpecMapIterator, operator(/=)
   use mapl_ArgumentSpec_mod, only: ArgumentSpec
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_StateMethodInvocation_mod, only: StateMethodInvocation
   use mapl_CallbackMethodAttachmentMap_mod, only: CallbackMethodAttachmentMap
   use mapl_MethodInvocationAdapter_mod, only: StateMethodInvoker
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackStateBinding

   type :: CallbackStateBinding
      private
      type(CallbackInterfaceId) :: interface_id
      type(CallbackInterface) :: iface
      type(NodeId) :: state_node_id
      type(StateItemMemberMap) :: argument_bindings
      type(CallbackMethodAttachmentMap) :: method_attachments
   contains
      procedure :: get_interface_id => binding_get_interface_id
      procedure :: get_state_node_id => binding_get_state_node_id
      procedure :: bind_argument => binding_bind_argument
      procedure :: get_argument_binding => binding_get_argument_binding
      procedure :: bind_method => binding_bind_method
      procedure :: get_method_attachment => binding_get_method_attachment
      procedure :: invoke_method => binding_invoke_method
   end type CallbackStateBinding

   interface CallbackStateBinding
      module procedure new_CallbackStateBinding
   end interface CallbackStateBinding

contains

   function new_CallbackStateBinding(interface_id, iface, state_node_id) result(binding)
      type(CallbackInterfaceId), intent(in) :: interface_id
      type(CallbackInterface), intent(in) :: iface
      type(NodeId), intent(in) :: state_node_id
      type(CallbackStateBinding) :: binding

      binding%interface_id = interface_id
      binding%iface = iface
      binding%state_node_id = state_node_id
   end function new_CallbackStateBinding

   function binding_get_interface_id(this) result(interface_id)
      class(CallbackStateBinding), intent(in) :: this
      type(CallbackInterfaceId) :: interface_id

      interface_id = this%interface_id
   end function binding_get_interface_id

   function binding_get_state_node_id(this) result(state_node_id)
      class(CallbackStateBinding), intent(in) :: this
      type(NodeId) :: state_node_id

      state_node_id = this%state_node_id
   end function binding_get_state_node_id

   ! REQ-CB-007: binds argument name -> member_id. Requires name to be
   ! declared on the bound interface (spec scenario "Binding an argument
   ! name outside the interface is rejected") and rejects rebinding an
   ! already-bound name (no-silent-replace, matching MethodGraphNode.
   ! bind_argument's own convention).
   subroutine binding_bind_argument(this, name, member_id, rc)
      class(CallbackStateBinding), intent(inout) :: this
      character(*), intent(in) :: name
      type(NodeId), intent(in) :: member_id
      integer, optional, intent(out) :: rc

      _ASSERT(this%iface%is_argument(name), 'CallbackStateBinding: bind_argument - argument name not declared on interface')
      _ASSERT(this%argument_bindings%count(name) == 0, 'CallbackStateBinding: bind_argument - argument name is already bound')

      call this%argument_bindings%insert(name, member_id)

      _RETURN(_SUCCESS)
   end subroutine binding_bind_argument

   function binding_get_argument_binding(this, name) result(member_id)
      class(CallbackStateBinding), target, intent(in) :: this
      character(*), intent(in) :: name
      type(NodeId), pointer :: member_id

      member_id => this%argument_bindings%at(name)
   end function binding_get_argument_binding

   ! REQ-CB-007: binds method name -> a StateMethodInvocation attachment
   ! constructed from this binding's own state_node_id (so the caller
   ! never supplies it redundantly, design.md Decision 5). Requires name
   ! to be declared on the bound interface (spec scenario "Binding a
   ! method name outside the interface is rejected") and rejects
   ! rebinding an already-bound name. invoker is optional - an
   ! attachment with none configured fails loudly on invoke() (spec
   ! scenario "Method attachment with no configured implementation fails
   ! loudly on invocation"), StateMethodInvocation's own established
   ! behavior.
   subroutine binding_bind_method(this, name, rc, invoker)
      class(CallbackStateBinding), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      class(StateMethodInvoker), optional, intent(in) :: invoker

      _ASSERT(this%iface%is_method(name), 'CallbackStateBinding: bind_method - method name not declared on interface')
      _ASSERT(this%method_attachments%count(name) == 0, 'CallbackStateBinding: bind_method - method name is already bound')

      if (present(invoker)) then
         call this%method_attachments%insert(name, StateMethodInvocation(this%state_node_id, name, invoker))
      else
         call this%method_attachments%insert(name, StateMethodInvocation(this%state_node_id, name))
      end if

      _RETURN(_SUCCESS)
   end subroutine binding_bind_method

   function binding_get_method_attachment(this, name) result(attachment)
      class(CallbackStateBinding), target, intent(in) :: this
      character(*), intent(in) :: name
      type(StateMethodInvocation), pointer :: attachment

      attachment => this%method_attachments%at(name)
   end function binding_get_method_attachment

   ! REQ-CB-007/REQ-CB-001: invokes the named method's attachment with
   ! this binding's current argument member bindings. Materializes a
   ! fresh ArgumentSpecMap for this method call (module header) - each
   ! argument's AccessSpec comes from the target method's own
   ! CallbackMethodSpec, its expected kind from the interface's own
   ! CallbackArgumentSpec. Fails loudly if the method is unbound or (via
   ! the attachment's own invoke()) if no invoker is attached.
   subroutine binding_invoke_method(this, name, rc)
      class(CallbackStateBinding), target, intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(StateMethodInvocation), pointer :: attachment
      type(CallbackMethodSpec) :: method_spec
      type(ArgumentSpecMap) :: arguments
      integer :: status

      attachment => this%method_attachments%at(name)
      _ASSERT(associated(attachment), 'CallbackStateBinding: invoke_method - method not bound')

      method_spec = this%iface%get_method(name, _RC)
      call build_arguments_for_method(this%iface, method_spec, arguments, _RC)

      call attachment%invoke(arguments, this%argument_bindings, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine binding_invoke_method

   ! Builds one method's ArgumentSpecMap on the fly: for every argument
   ! name the target method declares an AccessSpec for, look up that
   ! argument's expected kind on the owning interface and combine both
   ! into an ArgumentSpec (module header - why this cannot be a single
   ! persisted map shared across every method on the binding).
   subroutine build_arguments_for_method(iface, method_spec, arguments, rc)
      type(CallbackInterface), target, intent(in) :: iface
      type(CallbackMethodSpec), intent(in) :: method_spec
      type(ArgumentSpecMap), intent(out) :: arguments
      integer, optional, intent(out) :: rc

      type(AccessSpecMap), target :: accesses
      type(AccessSpecMapIterator) :: iter
      type(AccessSpec) :: access
      type(CallbackArgumentSpec) :: arg_spec
      character(:), allocatable :: argument_name
      integer :: status

      accesses = method_spec%get_argument_accesses()

      iter = accesses%ftn_begin()
      do while (iter /= accesses%ftn_end())
         call iter%next()
         argument_name = iter%first()
         access = iter%second()

         arg_spec = iface%get_argument(argument_name, _RC)
         call arguments%insert(argument_name, ArgumentSpec(argument_name, access, arg_spec%get_expected_kind()))
      end do

      _RETURN(_SUCCESS)
   end subroutine build_arguments_for_method

end module mapl_CallbackStateBinding_mod
