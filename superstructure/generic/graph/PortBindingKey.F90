!------------------------------------------------------------------------------
! PortBindingKey: composite gFTL map key pairing a DependencyNetworkId
! with a NodeId, used by PortBindingTable to key the external
! (DependencyNetworkId, NodeId) -> port name -> NodeId table required by
! spec/10-transforms-and-ports.md REQ-XFORM-005.
!------------------------------------------------------------------------------
module mapl_PortBindingKey_mod
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId, operator(==), operator(<)
   use mapl_NodeId_mod, only: NodeId, operator(==), operator(<)
   implicit none(type, external)
   private

   public :: PortBindingKey
   public :: operator(==)
   public :: operator(<)

   type :: PortBindingKey
      private
      type(DependencyNetworkId) :: network_id
      type(NodeId) :: node_id
   contains
      procedure :: get_network_id => key_get_network_id
      procedure :: get_node_id => key_get_node_id
   end type PortBindingKey

   interface PortBindingKey
      module procedure new_PortBindingKey
   end interface PortBindingKey

   interface operator(==)
      module procedure key_equal
   end interface operator(==)

   interface operator(<)
      module procedure key_less_than
   end interface operator(<)

contains

   function new_PortBindingKey(network_id, node_id) result(key)
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      type(PortBindingKey) :: key

      key%network_id = network_id
      key%node_id = node_id
   end function new_PortBindingKey

   function key_get_network_id(this) result(network_id)
      class(PortBindingKey), intent(in) :: this
      type(DependencyNetworkId) :: network_id

      network_id = this%network_id
   end function key_get_network_id

   function key_get_node_id(this) result(node_id)
      class(PortBindingKey), intent(in) :: this
      type(NodeId) :: node_id

      node_id = this%node_id
   end function key_get_node_id

   pure logical function key_equal(left, right) result(equal)
      type(PortBindingKey), intent(in) :: left, right

      equal = (left%network_id == right%network_id) .and. (left%node_id == right%node_id)
   end function key_equal

   ! Lexicographic ordering: network id first, then node id - sufficient
   ! for use as a gFTL map key (only a strict weak ordering is required,
   ! not a semantically meaningful one).
   pure logical function key_less_than(left, right) result(less)
      type(PortBindingKey), intent(in) :: left, right

      if (left%network_id < right%network_id) then
         less = .true.
      else if (right%network_id < left%network_id) then
         less = .false.
      else
         less = left%node_id < right%node_id
      end if
   end function key_less_than

end module mapl_PortBindingKey_mod
