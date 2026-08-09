#include "MAPL.h"
module mapl_DependencyNetwork_mod
   use mapl_EdgeId_mod
   use mapl_EdgeIdSet_mod
   use mapl_NodeId_mod, only: NodeId, operator(==)
   use mapl_GraphEdge_mod, only: GraphEdge
   use mapl_NodeId_EdgeIdSetMap_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: DependencyNetwork

   type :: PortBinding
      private
      type(NodeId) :: transform_id
      character(:), allocatable :: port_name
      type(EdgeId) :: edge_id
   end type PortBinding

   type :: DependencyNetwork
      private
      type(EdgeIdSet) :: edges_
      type(NodeId_EdgeIdSetMap) :: incoming_edges
      type(NodeId_EdgeIdSetMap) :: outgoing_edges
      type(PortBinding), allocatable :: input_bindings(:)
      type(PortBinding), allocatable :: output_bindings(:)
    contains
       procedure :: add_edge_membership
       procedure :: get_in_edges
       procedure :: get_out_edges
       procedure :: edge_ids
       procedure :: contains_edge
       procedure :: input_edge
       procedure :: output_edge
       procedure :: input_binding_count
       procedure :: output_binding_count
       procedure :: get_input_binding
       procedure :: get_output_binding
       procedure :: validate
   end type DependencyNetwork

   interface DependencyNetwork
      module procedure new_dependency_network
   end interface DependencyNetwork

contains

   function new_dependency_network() result(network)
      type(DependencyNetwork) :: network

      network%edges_ = EdgeIdSet()
      allocate(network%input_bindings(0))
      allocate(network%output_bindings(0))
   end function new_dependency_network

   ! Low-level mutation. Caller must validate graph topology before invoking it.
    subroutine add_edge_membership(this, edge_id, edge, rc, transform_id, port_name, is_input)
      class(DependencyNetwork), target, intent(inout) :: this
      type(EdgeId), intent(in) :: edge_id
      type(GraphEdge), intent(in) :: edge
       type(NodeId), optional, intent(in) :: transform_id
       character(*), optional, intent(in) :: port_name
       logical, optional, intent(in) :: is_input
       integer, optional, intent(out) :: rc

      type(EdgeIdSet), pointer :: edge_ids
      type(NodeId) :: source, target
      integer :: status
       type(EdgeId) :: id
       logical :: has_binding

      _ASSERT(edge_id%is_valid(), &
         'Invalid EdgeId passed to DependencyNetwork%add_edge_membership.')
      source = edge%source()
      target = edge%target()
      _ASSERT(source%is_valid(), 'Edge source is invalid.')
      _ASSERT(target%is_valid(), 'Edge target is invalid.')
       _ASSERT(.not. this%contains_edge(edge_id), 'Edge already belongs to dependency network.')
       has_binding = present(transform_id) .or. present(port_name) .or. present(is_input)
       _ASSERT(has_binding .eqv. (present(transform_id) .and. present(port_name) .and. present(is_input)), &
          'Transform binding arguments must be supplied together.')
       if (has_binding) then
          _ASSERT(transform_id%is_valid(), 'Transform NodeId is invalid.')
          _ASSERT(len(port_name) > 0, 'Transform port name must not be empty.')
          if (is_input) then
             id = this%input_edge(transform_id, port_name)
             _ASSERT(.not. id%is_valid(), 'Transform input port is already bound.')
          else
             id = this%output_edge(transform_id, port_name)
             _ASSERT(.not. id%is_valid(), 'Transform output port is already bound.')
          end if
       end if

      call this%edges_%insert(edge_id)

      if (this%outgoing_edges%count(source) == 0) then
         call this%outgoing_edges%insert(source, EdgeIdSet())
      end if
      edge_ids => this%outgoing_edges%at(source)
      call edge_ids%insert(edge_id)

      if (this%incoming_edges%count(target) == 0) then
         call this%incoming_edges%insert(target, EdgeIdSet())
      end if
      edge_ids => this%incoming_edges%at(target)
      call edge_ids%insert(edge_id)

       if (has_binding) then
          if (is_input) then
             call append_binding(this%input_bindings, transform_id, port_name, edge_id)
          else
             call append_binding(this%output_bindings, transform_id, port_name, edge_id)
          end if
       end if

      _RETURN(_SUCCESS)
   end subroutine add_edge_membership

   function get_in_edges(this, node_id) result(edge_ids)
      class(DependencyNetwork), target, intent(in) :: this
      type(NodeId), intent(in) :: node_id
       type(EdgeIdSet), pointer :: edge_ids

       nullify(edge_ids)
       if (this%incoming_edges%count(node_id) > 0) then
          edge_ids => this%incoming_edges%at(node_id)
       end if
   end function get_in_edges

   function get_out_edges(this, node_id) result(edge_ids)
      class(DependencyNetwork), target, intent(in) :: this
      type(NodeId), intent(in) :: node_id
       type(EdgeIdSet), pointer :: edge_ids

       nullify(edge_ids)
       if (this%outgoing_edges%count(node_id) > 0) then
          edge_ids => this%outgoing_edges%at(node_id)
       end if
   end function get_out_edges

   function edge_ids(this) result(ids)
      class(DependencyNetwork), intent(in) :: this
      type(EdgeIdSet) :: ids

      ids = this%edges_
   end function edge_ids

   logical function contains_edge(this, edge_id)
      class(DependencyNetwork), intent(in) :: this
      type(EdgeId), intent(in) :: edge_id

      contains_edge = this%edges_%count(edge_id) > 0
   end function contains_edge

   function input_edge(this, transform_id, port_name) result(edge_id)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: transform_id
      character(*), intent(in) :: port_name
      type(EdgeId) :: edge_id

      edge_id = find_binding(this%input_bindings, transform_id, port_name)
   end function input_edge

   function output_edge(this, transform_id, port_name) result(edge_id)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: transform_id
      character(*), intent(in) :: port_name
      type(EdgeId) :: edge_id

      edge_id = find_binding(this%output_bindings, transform_id, port_name)
   end function output_edge

   integer function input_binding_count(this)
      class(DependencyNetwork), intent(in) :: this

      input_binding_count = 0
      if (allocated(this%input_bindings)) input_binding_count = size(this%input_bindings)
   end function input_binding_count

   integer function output_binding_count(this)
      class(DependencyNetwork), intent(in) :: this

      output_binding_count = 0
      if (allocated(this%output_bindings)) output_binding_count = size(this%output_bindings)
   end function output_binding_count

   subroutine get_input_binding(this, index, transform_id, port_name, edge_id)
      class(DependencyNetwork), intent(in) :: this
      integer, intent(in) :: index
      type(NodeId), intent(out) :: transform_id
      character(:), allocatable, intent(out) :: port_name
      type(EdgeId), intent(out) :: edge_id

      transform_id = this%input_bindings(index)%transform_id
      port_name = this%input_bindings(index)%port_name
      edge_id = this%input_bindings(index)%edge_id
   end subroutine get_input_binding

   subroutine get_output_binding(this, index, transform_id, port_name, edge_id)
      class(DependencyNetwork), intent(in) :: this
      integer, intent(in) :: index
      type(NodeId), intent(out) :: transform_id
      character(:), allocatable, intent(out) :: port_name
      type(EdgeId), intent(out) :: edge_id

      transform_id = this%output_bindings(index)%transform_id
      port_name = this%output_bindings(index)%port_name
      edge_id = this%output_bindings(index)%edge_id
   end subroutine get_output_binding

   subroutine validate(this, rc)
      class(DependencyNetwork), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      ! This verifies membership/index consistency only. Graph-level validation
      ! must resolve EdgeIds to check endpoints and acyclicity.
      call validate_index(this, this%incoming_edges, 'Incoming', _RC)
      call validate_index(this, this%outgoing_edges, 'Outgoing', _RC)
      call validate_bindings(this, _RC)

      _RETURN(_SUCCESS)
   end subroutine validate

   subroutine validate_index(this, index, name, rc)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId_EdgeIdSetMap), target, intent(in) :: index
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(NodeId_EdgeIdSetMapIterator) :: map_iter
      type(EdgeIdSetIterator) :: edge_iter
      type(EdgeIdSet), pointer :: indexed_edges
      type(EdgeIdSet) :: seen
      integer :: status

      seen = EdgeIdSet()
      associate (map_end => index%ftn_end())
         map_iter = index%ftn_begin()
         do while (map_iter /= map_end)
            indexed_edges => map_iter%second()
            associate (set_end => indexed_edges%ftn_end())
               edge_iter = indexed_edges%ftn_begin()
               do while (edge_iter /= set_end)
                  _ASSERT(this%contains_edge(edge_iter%of()), &
                     name // '-edge index contains an unknown EdgeId.')
                  _ASSERT(seen%count(edge_iter%of()) == 0, &
                     name // '-edge index contains a duplicate EdgeId.')
                  call seen%insert(edge_iter%of())
                  call edge_iter%next()
               end do
            end associate
            call map_iter%next()
         end do
      end associate

      _ASSERT(seen%size() == this%edges_%size(), &
         name // '-edge index does not contain every network EdgeId.')

      _RETURN(_SUCCESS)
   end subroutine validate_index

   subroutine append_binding(bindings, transform_id, port_name, edge_id)
      type(PortBinding), allocatable, intent(inout) :: bindings(:)
      type(NodeId), intent(in) :: transform_id
      character(*), intent(in) :: port_name
      type(EdgeId), intent(in) :: edge_id
      type(PortBinding) :: binding

      binding%transform_id = transform_id
      binding%port_name = port_name
      binding%edge_id = edge_id
      if (allocated(bindings)) then
         bindings = [bindings, binding]
      else
         bindings = [binding]
      end if
   end subroutine append_binding

   function find_binding(bindings, transform_id, port_name) result(edge_id)
      type(PortBinding), allocatable, intent(in) :: bindings(:)
      type(NodeId), intent(in) :: transform_id
      character(*), intent(in) :: port_name
      type(EdgeId) :: edge_id
      integer :: i

      if (.not. allocated(bindings)) return
      do i = 1, size(bindings)
         if (bindings(i)%transform_id == transform_id .and. &
             bindings(i)%port_name == port_name) then
            edge_id = bindings(i)%edge_id
            return
         end if
      end do
   end function find_binding

   subroutine validate_bindings(this, rc)
      class(DependencyNetwork), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(EdgeIdSet) :: seen
      integer :: i, j, status

      seen = EdgeIdSet()
      if (allocated(this%input_bindings)) then
         do i = 1, size(this%input_bindings)
            call validate_binding(this, this%input_bindings(i), seen, _RC)
            do j = i + 1, size(this%input_bindings)
               _ASSERT(.not. same_port(this%input_bindings(i), this%input_bindings(j)), &
                  'Transform input port has multiple bindings.')
            end do
         end do
      end if
      if (allocated(this%output_bindings)) then
         do i = 1, size(this%output_bindings)
            call validate_binding(this, this%output_bindings(i), seen, _RC)
            do j = i + 1, size(this%output_bindings)
               _ASSERT(.not. same_port(this%output_bindings(i), this%output_bindings(j)), &
                  'Transform output port has multiple bindings.')
            end do
         end do
      end if
      _ASSERT(seen%size() == this%edges_%size(), &
         'Port bindings do not contain every network EdgeId.')

      _RETURN(_SUCCESS)
   end subroutine validate_bindings

   subroutine validate_binding(this, binding, seen, rc)
      class(DependencyNetwork), intent(in) :: this
      type(PortBinding), intent(in) :: binding
      type(EdgeIdSet), intent(inout) :: seen
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(binding%transform_id%is_valid(), 'Port binding has invalid Transform NodeId.')
      _ASSERT(len(binding%port_name) > 0, 'Port binding has empty port name.')
      _ASSERT(this%contains_edge(binding%edge_id), 'Port binding contains unknown EdgeId.')
      _ASSERT(seen%count(binding%edge_id) == 0, 'EdgeId has multiple port bindings.')
      call seen%insert(binding%edge_id)

      _RETURN(_SUCCESS)
   end subroutine validate_binding

   logical function same_port(lhs, rhs)
      type(PortBinding), intent(in) :: lhs, rhs

      same_port = lhs%transform_id == rhs%transform_id .and. &
         lhs%port_name == rhs%port_name
   end function same_port

end module mapl_DependencyNetwork_mod
