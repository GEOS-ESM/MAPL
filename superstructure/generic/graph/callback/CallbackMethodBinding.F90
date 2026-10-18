#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackMethodBinding: the method-level binding record connecting one
! callback method's invocation to the graph structure it operates on
! (spec/15-callbacks.md REQ-CB-018/019, openspec/changes/callback-wiring
! design.md Decision 6) - completing 15-callbacks.md sec 15.2's metadata-
! concept list (CallbackMethodAttachment was already resolved by Phase 4c
! as "reuse StateMethodInvocation, no new type"; this is the other
! concept 4c explicitly left to this change).
!
! Stores the invoked MethodGraphNode's own NodeId (REQ-CB-019 - the
! caller is responsible for having registered that node, with a
! StateMethodInvocation adapter attached, in the destination component's
! own ComponentGraph) plus, for the get and put dependency networks
! REQ-CB-018 establishes, each one's DependencyNetworkId and its own
! argument name -> NodeId endpoint map. get_bindings/put_bindings are two
! separate StateItemMemberMaps, one per direction, rather than one map
! keyed by a compound (argument_name, direction) key - mirrors
! CallbackStateBinding's own established "one plain gFTL map per
! concept" precedent (design.md Decision 6).
!
! For an argument that is only ever read (a "get"-only argument) or only
! ever written (a "put"-only argument), the binding's own put_network_id/
! get_network_id and the corresponding empty map are simply unused - this
! type does not itself enforce that every argument appears in exactly one
! direction; that is the caller's (GraphBuilder's) own wiring
! responsibility.
!------------------------------------------------------------------------------
module mapl_CallbackMethodBinding_mod
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_NodeId_mod, only: NodeId
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackMethodBinding

   type :: CallbackMethodBinding
      private
      type(NodeId) :: method_node_id
      type(DependencyNetworkId) :: get_net_id
      type(DependencyNetworkId) :: put_net_id
      type(StateItemMemberMap) :: get_bindings
      type(StateItemMemberMap) :: put_bindings
   contains
      procedure :: get_method_node_id => binding_get_method_node_id
      procedure :: get_network_id => binding_get_network_id
      procedure :: get_get_network_id => binding_get_get_network_id
      procedure :: get_put_network_id => binding_get_put_network_id
      procedure :: bind_get_argument => binding_bind_get_argument
      procedure :: bind_put_argument => binding_bind_put_argument
      procedure :: get_get_argument_binding => binding_get_get_argument_binding
      procedure :: get_put_argument_binding => binding_get_put_argument_binding
      procedure :: get_get_bindings => binding_get_get_bindings
      procedure :: get_put_bindings => binding_get_put_bindings
   end type CallbackMethodBinding

   interface CallbackMethodBinding
      module procedure new_CallbackMethodBinding
   end interface CallbackMethodBinding

contains

   function new_CallbackMethodBinding(method_node_id, get_network_id, put_network_id) result(binding)
      type(NodeId), intent(in) :: method_node_id
      type(DependencyNetworkId), intent(in) :: get_network_id
      type(DependencyNetworkId), intent(in) :: put_network_id
      type(CallbackMethodBinding) :: binding

      binding%method_node_id = method_node_id
      binding%get_net_id = get_network_id
      binding%put_net_id = put_network_id
   end function new_CallbackMethodBinding

   function binding_get_method_node_id(this) result(method_node_id)
      class(CallbackMethodBinding), intent(in) :: this
      type(NodeId) :: method_node_id

      method_node_id = this%method_node_id
   end function binding_get_method_node_id

   ! "The" network identity (spec REQ-CB-019/scenario "Method-level
   ! binding exposes network...") - the get network is the primary one
   ! reported here; get_get_network_id()/get_put_network_id() below are
   ! the unambiguous accessors for either specific direction (module
   ! header).
   function binding_get_network_id(this) result(network_id)
      class(CallbackMethodBinding), intent(in) :: this
      type(DependencyNetworkId) :: network_id

      network_id = this%get_net_id
   end function binding_get_network_id

   function binding_get_get_network_id(this) result(network_id)
      class(CallbackMethodBinding), intent(in) :: this
      type(DependencyNetworkId) :: network_id

      network_id = this%get_net_id
   end function binding_get_get_network_id

   function binding_get_put_network_id(this) result(network_id)
      class(CallbackMethodBinding), intent(in) :: this
      type(DependencyNetworkId) :: network_id

      network_id = this%put_net_id
   end function binding_get_put_network_id

   subroutine binding_bind_get_argument(this, argument_name, source_id, rc)
      class(CallbackMethodBinding), intent(inout) :: this
      character(*), intent(in) :: argument_name
      type(NodeId), intent(in) :: source_id
      integer, optional, intent(out) :: rc

      _ASSERT(this%get_bindings%count(argument_name) == 0, 'CallbackMethodBinding: bind_get_argument - argument name is already bound')

      call this%get_bindings%insert(argument_name, source_id)

      _RETURN(_SUCCESS)
   end subroutine binding_bind_get_argument

   subroutine binding_bind_put_argument(this, argument_name, target_id, rc)
      class(CallbackMethodBinding), intent(inout) :: this
      character(*), intent(in) :: argument_name
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

      _ASSERT(this%put_bindings%count(argument_name) == 0, 'CallbackMethodBinding: bind_put_argument - argument name is already bound')

      call this%put_bindings%insert(argument_name, target_id)

      _RETURN(_SUCCESS)
   end subroutine binding_bind_put_argument

   function binding_get_get_argument_binding(this, argument_name) result(source_id)
      class(CallbackMethodBinding), target, intent(in) :: this
      character(*), intent(in) :: argument_name
      type(NodeId), pointer :: source_id

      source_id => this%get_bindings%at(argument_name)
   end function binding_get_get_argument_binding

   function binding_get_put_argument_binding(this, argument_name) result(target_id)
      class(CallbackMethodBinding), target, intent(in) :: this
      character(*), intent(in) :: argument_name
      type(NodeId), pointer :: target_id

      target_id => this%put_bindings%at(argument_name)
   end function binding_get_put_argument_binding

   function binding_get_get_bindings(this) result(bindings)
      class(CallbackMethodBinding), intent(in) :: this
      type(StateItemMemberMap) :: bindings

      bindings = this%get_bindings
   end function binding_get_get_bindings

   function binding_get_put_bindings(this) result(bindings)
      class(CallbackMethodBinding), intent(in) :: this
      type(StateItemMemberMap) :: bindings

      bindings = this%put_bindings
   end function binding_get_put_bindings

end module mapl_CallbackMethodBinding_mod
