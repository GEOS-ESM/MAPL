#include "MAPL.h"
module mapl_ComponentGraph_mod
   use mapl_NodeId_mod
   use mapl_NodeIdSet_mod
   use mapl_GraphNode_mod
   use mapl_ValueGraphNode_mod
   use mapl_TransformGraphNode_mod
   use mapl_GraphNodeMap_mod
   use mapl_EdgeId_mod
   use mapl_EdgeIdSet_mod
   use mapl_GraphEdge_mod
   use mapl_GraphEdgeMap_mod
   use mapl_DependencyNetwork_mod
   use mapl_DependencyNetworkId_mod
   use mapl_DependencyNetworkMap_mod
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod

   implicit none(type,external)
   private

   public :: ComponentGraph

   type :: ComponentGraph
      private
      type(GraphNodeMap) :: nodes
      type(GraphEdgeMap) :: edges
      type(DependencyNetworkMap) :: dependency_networks

      type(NodeIdGenerator) :: node_id_generator
      type(EdgeIdGenerator) :: edge_id_generator
      type(DependencyNetworkIdGenerator) :: network_id_generator
       type(DependencyNetworkId) :: default_network_id

      logical :: is_initialized_ = .false.
      logical :: is_frozen_ = .false.

   contains
      procedure :: add_node
      procedure :: has_node
      procedure :: get_node

      procedure :: add_edge
      procedure :: has_edge
      procedure :: get_edge
      procedure, private :: would_create_cycle

      procedure :: add_network
      procedure :: has_network
      procedure, private :: get_network

      procedure :: freeze
      procedure :: is_frozen

      procedure :: initialize
      procedure :: is_initialized

      procedure :: mark_updated
      procedure :: mark_valid
      procedure :: invalidate
      procedure :: invalidate_downstream
      procedure :: update
      procedure :: mark_outputs_valid

      procedure :: validate

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type ComponentGraph

contains

   subroutine initialize(this, rc)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(.not. this%is_initialized(), 'ComponentGraph already initialized')
      this%default_network_id = this%add_network(DependencyNetwork(), _RC)
      this%is_initialized_ = .true.

      _RETURN(_SUCCESS)
   end subroutine initialize

   logical function is_initialized(this)
      class(ComponentGraph), intent(in) :: this
      is_initialized = this%is_initialized_
   end function is_initialized

   logical function has_node(this, id)
      class(ComponentGraph), intent(in) :: this
      type(NodeId), intent(in) :: id

      has_node = this%nodes%count(id) > 0
   end function has_node

   logical function has_edge(this, id)
      class(ComponentGraph), intent(in) :: this
      type(EdgeId), intent(in) :: id

      has_edge = this%edges%count(id) > 0
   end function has_edge

   logical function has_network(this, id)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id

      has_network = this%dependency_networks%count(id) > 0
   end function has_network

   logical function is_frozen(this)
      class(ComponentGraph), intent(in) :: this

      is_frozen = this%is_frozen_
   end function is_frozen

   function add_network(this, network, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      type(DependencyNetwork), intent(in) :: network
      integer, optional, intent(out) :: rc
      type(DependencyNetworkId) :: id

      integer :: status

      _ASSERT(.not. this%is_frozen(), 'Cannot add dependency network to frozen graph.')
      id = this%network_id_generator%next(_RC)
      call this%dependency_networks%insert(id, network)

      _RETURN(_SUCCESS)
   end function add_network

   function get_network(this, id, rc) result(network)
      class(ComponentGraph), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: id
      type(DependencyNetwork), pointer :: network
      integer, optional, intent(out) :: rc

      integer :: status
      
      _ASSERT(id%is_valid(), 'Invalid DependencyNetworkId passed to get_network.')
      _ASSERT(this%has_network(id), 'DependencyNetworkId not found in graph.')
       network => this%dependency_networks%at(id, _RC)

      _RETURN(_SUCCESS)
   end function get_network

   function add_node(this, node, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      class(GraphNode), intent(in) :: node
      integer, optional, intent(out) :: rc
      type(NodeId) :: id

      integer :: status

      _ASSERT(this%is_initialized(), 'ComponentGraph must be initialized before adding nodes.')
      _ASSERT(.not. this%is_frozen(), 'Cannot add node to frozen graph.')
      id = this%node_id_generator%next(_RC)
      call this%nodes%insert(id, node)

      _RETURN(_SUCCESS)
   end function add_node

    function add_edge(this, edge, unusable, network_id, rc) result(edge_id)
      class(ComponentGraph), target, intent(inout) :: this
      class(GraphEdge), intent(in) :: edge
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(DependencyNetworkId), optional, intent(in) :: network_id
       integer, optional, intent(out) :: rc

       integer :: status
       type(EdgeId) :: edge_id
       type(DependencyNetworkId) :: network_id_
       type(DependencyNetwork), pointer :: dependency_network
       class(GraphNode), pointer :: source_node, target_node

       _UNUSED_DUMMY(unusable)
       _ASSERT(this%is_initialized(), 'ComponentGraph must be initialized before adding edges.')
      _ASSERT(.not. this%is_frozen(), 'Cannot add edge to frozen graph.')

      associate (src_id => edge%source(), tgt_id => edge%target())
        _ASSERT(src_id%is_valid(), 'Edge source is invalid.')
        _ASSERT(tgt_id%is_valid(), 'Edge target is invalid.')
        _ASSERT(this%has_node(src_id), 'Edge source node not found in graph.')
        _ASSERT(this%has_node(tgt_id), 'Edge target node not found in graph.')
      end associate
      network_id_ = this%default_network_id
      if (present(network_id)) network_id_ = network_id
       dependency_network => this%get_network(network_id_, _RC)

      _ASSERT(.not. this%would_create_cycle(dependency_network, edge), 'Edge would make dependency network cyclic.')

       edge_id = this%edge_id_generator%next(_RC)
       call this%edges%insert(edge_id, edge)
       source_node => this%get_node(edge%source(), _RC)
       target_node => this%get_node(edge%target(), _RC)
       call source_node%add_out_edge(edge_id)
       call target_node%add_in_edge(edge_id)
       call dependency_network%add_edge_membership(edge_id, edge, _RC)

      _RETURN(_SUCCESS)
    end function add_edge

   logical function would_create_cycle(this, network, edge)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetwork), intent(in) :: network
      class(GraphEdge), intent(in) :: edge

      type(NodeIdSet) :: visited

      would_create_cycle = path_exists(this, network, edge%target(), edge%source(), visited)
   end function would_create_cycle

   recursive logical function path_exists(this, network, source, target, visited) result(found)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetwork), intent(in) :: network
      type(NodeId), intent(in) :: source, target
      type(NodeIdSet), intent(inout) :: visited

       type(EdgeIdSet), pointer :: edge_ids
      type(EdgeIdSetIterator) :: edge_iter
      type(GraphEdge), pointer :: edge

      found = source == target
      if (found .or. visited%count(source) > 0) return

      call visited%insert(source)
       edge_ids => network%get_out_edges(source)
       if (.not. associated(edge_ids)) return
      associate (edge_end => edge_ids%ftn_end())
         edge_iter = edge_ids%ftn_begin()
         do while (edge_iter /= edge_end)
            edge => this%edges%at(edge_iter%of())
            if (path_exists(this, network, edge%target(), target, visited)) then
               found = .true.
               return
            end if
            call edge_iter%next()
         end do
      end associate
   end function path_exists

   function get_node(this, id, rc) result(node)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeId), intent(in) :: id
      integer, optional, intent(out) :: rc
      class(GraphNode), pointer :: node

      integer :: status

      _ASSERT(id%is_valid(), 'Invalid NodeId passed to get_node.')
      _ASSERT(this%nodes%count(id) > 0, 'NodeId not found in graph.')

       node => this%nodes%at(id, _RC)

      _RETURN(_SUCCESS)

   end function get_node

   function get_edge(this, id, rc) result(edge)
      class(ComponentGraph), target, intent(in) :: this
      type(EdgeId), intent(in) :: id
      integer, optional, intent(out) :: rc

      type(GraphEdge), pointer :: edge
      integer :: status

      _ASSERT(id%is_valid(), 'Invalid EdgeId passed to get_edge.')
      _ASSERT(this%has_edge(id), 'EdgeId not found in graph.')

       edge => this%edges%at(id, _RC)
      _RETURN(_SUCCESS)
   end function get_edge

   subroutine mark_updated(graph, node_id, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      integer :: status

      call graph%mark_valid(node_id, _RC)

      call graph%invalidate_downstream(node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine mark_updated

   subroutine mark_valid(this, node_id, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      class(GraphNode), pointer :: node
      integer :: status

      _ASSERT(node_id%is_valid(), 'Invalid NodeId passed to mark_valid.')
      _ASSERT(this%has_node(node_id), 'NodeId not found in graph.')

       node => this%nodes%at(node_id, _RC)
      select type (node)
      type is (ValueGraphNode)
         call node%advance_revision(_RC)
      end select
      call node%mark_valid()

      _RETURN(_SUCCESS)
   end subroutine mark_valid

   subroutine invalidate(graph, node_id, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node

       node => graph%get_node(node_id, _RC)
      call node%mark_invalid()

      call graph%invalidate_downstream(node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine invalidate

   recursive subroutine invalidate_downstream(graph, node_id, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
       integer, optional, intent(out) :: rc

       type(EdgeIdSet), pointer :: out_edge_ids
       class(GraphNode), pointer :: node, dst_node
       type(GraphEdge), pointer :: edge
       type(EdgeIdSetIterator) :: iter
       type(NodeId) :: dst_id
       integer :: status

       node => graph%get_node(node_id, _RC)
       out_edge_ids => node%get_out_edges()

       associate (e => out_edge_ids%ftn_end())
         iter = out_edge_ids%ftn_begin()
         do while (iter /= e)
             call iter%next()
             edge => graph%edges%at(iter%of(), _RC)
             dst_id = edge%target()
             dst_node => graph%get_node(dst_id, _RC)
             call dst_node%mark_invalid()
             call graph%invalidate_downstream(dst_id, _RC)
         end do
       end associate

      _RETURN(_SUCCESS)
   end subroutine invalidate_downstream

   recursive subroutine update(graph, node_id, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      type(EdgeIdSet), pointer :: in_edge_ids
      class(GraphEdge), pointer :: edge
      type(NodeId) :: src_id
      class(GraphNode), pointer :: node
      class(GraphNode), pointer :: src_node
      type(EdgeIdSetIterator) :: iter
      integer :: status

       node => graph%get_node(node_id, _RC)
      _RETURN_IF(node%is_valid())

      in_edge_ids => node%get_in_edges()

      associate (e => in_edge_ids%ftn_end())
        iter = in_edge_ids%ftn_begin()
        do while (iter /= e)
            edge => graph%edges%at(iter%of(), _RC)
           src_id = edge%source()
           call graph%update(src_id, _RC)
            src_node => graph%get_node(src_id, _RC)
            call iter%next()
        end do
      end associate

      if (node%is_transform()) then
         call node%update(_RC)
         call graph%mark_outputs_valid(node_id, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine mark_outputs_valid(this, transform_id, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(NodeId), intent(in) :: transform_id
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
       type(EdgeIdSet), pointer :: edge_ids
      type(EdgeIdSetIterator) :: iter
      type(GraphEdge), pointer :: edge
      class(GraphNode), pointer :: output
      integer :: status

       network => this%get_network(this%default_network_id, _RC)
       edge_ids => network%get_out_edges(transform_id)
       if (.not. associated(edge_ids)) then
          _RETURN(_SUCCESS)
       end if

      associate (e => edge_ids%ftn_end())
         iter = edge_ids%ftn_begin()
         do while (iter /= e)
             edge => this%edges%at(iter%of(), _RC)
             output => this%nodes%at(edge%target(), _RC)
            select type (output)
            type is (ValueGraphNode)
               call this%mark_valid(edge%target(), _RC)
            class default
               _FAIL('Transform output must be a ValueGraphNode.')
            end select
            call iter%next()
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine mark_outputs_valid

   subroutine freeze(this, rc)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      _ASSERT(this%is_initialized(), 'ComponentGraph must be initialized before freezing.')
      call this%validate(_RC)
      this%is_frozen_ = .true.
      _RETURN(_SUCCESS)
   end subroutine freeze

   subroutine validate(this, rc)
      class(ComponentGraph), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphNodeMapIterator) :: node_iter
      type(GraphEdgeMapIterator) :: edge_iter
      type(GraphEdge), pointer :: edge
      integer :: i

      associate (e => this%nodes%ftn_end())
         node_iter = this%nodes%ftn_begin()
         do
            call node_iter%next()
            if (node_iter == e) exit
            select type (node_value => node_iter%second())
            type is (ValueGraphNode)
               associate (constituents => node_value%constituents())
                 do i = 1, size(constituents)
                    _ASSERT(this%has_node(constituents(i)), 'ValueNode constituent not found in graph.')
                 end do
               end associate
            end select
         end do
      end associate

      associate (e => this%edges%ftn_end())
         edge_iter = this%edges%ftn_begin()
         do
            call edge_iter%next()
            if (edge_iter == e) exit
            edge => edge_iter%second()
            _ASSERT(this%has_node(edge%source()), 'Edge source node not found in graph.')
            _ASSERT(this%has_node(edge%target()), 'Edge target node not found in graph.')
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine validate

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(ComponentGraph), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      call write_formatted_inner(this, unit, iostat, iomsg)

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)

   contains

      subroutine write_formatted_inner(this, unit, iostat, iomsg)
         class(ComponentGraph), target, intent(in) :: this
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(GraphNodeMapIterator) :: iter
         integer :: n_transforms

         write(unit, *, iostat=iostat, iomsg=iomsg) 'ComponentGraph: '
         if (iostat /= 0) return

         write(unit, *, iostat=iostat, iomsg=iomsg) '    # nodes:      ', this%nodes%size()
         if (iostat /= 0) return

         write(unit, *, iostat=iostat, iomsg=iomsg) '    # edges:      ', this%edges%size()
         if (iostat /= 0) return

         n_transforms = 0
         associate (e => this%nodes%ftn_end())
            iter = this%nodes%ftn_begin()
            do while (iter /= e)
               select type (node => iter%second())
               type is (TransformGraphNode)
                  n_transforms = n_transforms + 1
               end select
               call iter%next()
            end do
         end associate

         write(unit, *, iostat=iostat, iomsg=iomsg) '    # transforms: ', n_transforms
      end subroutine write_formatted_inner

   end subroutine write_formatted

end module mapl_ComponentGraph_mod
