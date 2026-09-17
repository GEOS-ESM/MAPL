#include "MAPL.h"

!------------------------------------------------------------------------------
! PortBindingTable: the external (DependencyNetworkId, NodeId) -> port
! name -> NodeId table settled by spec/10-transforms-and-ports.md
! REQ-XFORM-005. Wraps a gFTL map of PortBindingKey ->
! StateItemMemberMap, enforcing "no silent replace" for both the
! per-(network,node) entry and individual port-name bindings within it,
! matching every other registration API in this repo (ComponentGraph's
! node/port registration, GraphStateItem's membership maps).
!
! This type does not itself know about ComponentGraph ownership or
! TransformGraphNode port declarations - those checks (network/node id
! ownership, "does this node actually declare this port name",
! kind-constraint matching) are ComponentGraph%bind_port()'s job, one
! layer up, keeping this type a plain, reusable keyed-storage container
! analogous to DependencyNetwork's own "no ownership knowledge" stance.
!------------------------------------------------------------------------------
module mapl_PortBindingTable_mod
   use mapl_PortBindingKey_mod, only: PortBindingKey
   use mapl_PortBindingKeyPortMap_mod, only: PortBindingKeyPortMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_NodeId_mod, only: NodeId
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: PortBindingTable

   type :: PortBindingTable
      private
      type(PortBindingKeyPortMap) :: bindings
   contains
      procedure :: bind => table_bind
      procedure :: get_binding => table_get_binding
      procedure :: get_bindings => table_get_bindings
      procedure :: has_binding => table_has_binding
   end type PortBindingTable

contains

   ! Rejects rebinding an already-bound port name for the same
   ! (network, node) pair; leaves the existing binding unchanged.
   subroutine table_bind(this, network_id, node_id, port_name, target_id, rc)
      class(PortBindingTable), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

      type(PortBindingKey) :: key
      type(StateItemMemberMap), pointer :: port_map
      type(StateItemMemberMap) :: empty_map

      key = PortBindingKey(network_id, node_id)
      if (this%bindings%count(key) == 0) call this%bindings%insert(key, empty_map)

      port_map => this%bindings%at(key)
      _ASSERT(port_map%count(port_name) == 0, 'PortBindingTable: port name is already bound for this (network, node) pair')
      call port_map%insert(port_name, target_id)

      _RETURN(_SUCCESS)
   end subroutine table_bind

   ! Pointer to the bound NodeId, or null if no binding exists for this
   ! (network, node, port name) combination - REQ-VIZ-012's "missing
   ! binding degrades gracefully" relies on this returning null rather
   ! than failing.
   function table_get_binding(this, network_id, node_id, port_name) result(target_id)
      class(PortBindingTable), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), pointer :: target_id

      type(PortBindingKey) :: key
      type(StateItemMemberMap), pointer :: port_map

      target_id => null()
      key = PortBindingKey(network_id, node_id)
      port_map => this%bindings%at(key)
      if (.not. associated(port_map)) return
      target_id => port_map%at(port_name)
   end function table_get_binding

   ! Copy of the whole port name -> NodeId map for one (network, node)
   ! pair; empty if none bound.
   function table_get_bindings(this, network_id, node_id) result(port_map_copy)
      class(PortBindingTable), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      type(StateItemMemberMap) :: port_map_copy

      type(PortBindingKey) :: key
      type(StateItemMemberMap), pointer :: found

      key = PortBindingKey(network_id, node_id)
      found => this%bindings%at(key)
      if (associated(found)) port_map_copy = found
   end function table_get_bindings

   logical function table_has_binding(this, network_id, node_id, port_name) result(has)
      class(PortBindingTable), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name

      type(NodeId), pointer :: found

      found => this%get_binding(network_id, node_id, port_name)
      has = associated(found)
   end function table_has_binding

end module mapl_PortBindingTable_mod
