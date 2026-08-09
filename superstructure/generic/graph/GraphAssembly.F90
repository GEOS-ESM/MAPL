#include "MAPL.h"
module mapl_GraphAssembly_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_FieldGraphValue_mod, only: FieldGraphValue
   use mapl_GeomGraphValue_mod, only: GeomGraphValue
   use mapl_GeomSpec_mod, only: GeomSpec
   use mapl_GraphEdge_mod, only: GraphEdge
   use mapl_EdgeId_mod, only: EdgeId
   use mapl_ItemSpec_mod, only: ItemSpec
   use mapl_NodeId_mod, only: NodeId
   use mapl_ValueGraphNode_mod, only: ValueGraphNode
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphAssembly

   type :: Endpoint
      character(:), allocatable :: name
      type(NodeId) :: node
   end type Endpoint

   type :: Connection
      character(:), allocatable :: source
      character(:), allocatable :: target
      logical :: resolved = .false.
   end type Connection

   type :: GraphAssembly
      private
      type(ComponentGraph) :: graph
      type(Endpoint), allocatable :: endpoints(:)
      type(Connection), allocatable :: connections(:)
      logical :: initialized_ = .false.
      logical :: advertised_ = .false.
   contains
      procedure :: initialize
      procedure :: declare_field
      procedure :: declare_geom
      procedure :: declare_connection
      procedure :: modify_advertise
      procedure :: unresolved_count
      procedure :: get_unresolved_connection
      procedure :: build
   end type GraphAssembly

contains

   subroutine initialize(this, rc)
      class(GraphAssembly), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%graph%initialize(_RC)
      this%initialized_ = .true.
      _RETURN(_SUCCESS)
   end subroutine initialize

   function declare_field(this, name, spec, rc) result(node)
      class(GraphAssembly), intent(inout) :: this
      character(*), intent(in) :: name
      type(ItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node

      integer :: status

      _ASSERT(this%initialized_, 'GraphAssembly must be initialized before declaration.')
      _ASSERT(.not. this%advertised_, 'Cannot declare field after advertisement.')
      _ASSERT(find_endpoint(this%endpoints, name) == 0, 'Graph endpoint already declared.')

      node = this%graph%add_node(ValueGraphNode(FieldGraphValue(name, spec)), _RC)
      call append_endpoint(this%endpoints, Endpoint(name, node))

      _RETURN(_SUCCESS)
   end function declare_field

   function declare_geom(this, name, geom_spec, rc) result(node)
      class(GraphAssembly), intent(inout) :: this
      character(*), intent(in) :: name
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node

      integer :: status

      _ASSERT(this%initialized_, 'GraphAssembly must be initialized before declaration.')
      _ASSERT(.not. this%advertised_, 'Cannot declare geometry after advertisement.')
      _ASSERT(find_endpoint(this%endpoints, name) == 0, 'Graph endpoint already declared.')

      node = this%graph%add_node( &
         ValueGraphNode(GeomGraphValue(geom_spec, name)), _RC)
      call append_endpoint(this%endpoints, Endpoint(name, node))

      _RETURN(_SUCCESS)
   end function declare_geom

   subroutine declare_connection(this, source, target, rc)
      class(GraphAssembly), intent(inout) :: this
      character(*), intent(in) :: source, target
      integer, optional, intent(out) :: rc

      _ASSERT(this%initialized_, 'GraphAssembly must be initialized before declaration.')
      _ASSERT(.not. this%advertised_, 'Cannot declare connection after advertisement.')
      call append_connection(this%connections, Connection(source, target))

      _RETURN(_SUCCESS)
   end subroutine declare_connection

   subroutine modify_advertise(this, rc)
      class(GraphAssembly), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status, i, source_index, target_index
      logical :: progress
      type(NodeId) :: source, target
      type(EdgeId) :: edge_id

      _ASSERT(this%initialized_, 'GraphAssembly must be initialized before advertisement.')
      _ASSERT(.not. this%advertised_, 'GraphAssembly advertisement already complete.')

      do
         progress = .false.
         do i = 1, connection_count(this%connections)
            if (this%connections(i)%resolved) cycle
            source_index = find_endpoint(this%endpoints, this%connections(i)%source)
            target_index = find_endpoint(this%endpoints, this%connections(i)%target)
            if (source_index == 0 .or. target_index == 0) cycle

            source = this%endpoints(source_index)%node
            target = this%endpoints(target_index)%node
            edge_id = this%graph%add_edge(GraphEdge(source, target), rc=status)
            _UNUSED_DUMMY(edge_id)
            _ASSERT(status == _SUCCESS, 'Failed to add declared graph connection.')
            this%connections(i)%resolved = .true.
            progress = .true.
         end do
         if (.not. progress) exit
      end do

      this%advertised_ = .true.
      _RETURN(_SUCCESS)
   end subroutine modify_advertise

   integer function unresolved_count(this)
      class(GraphAssembly), intent(in) :: this
      integer :: i

      unresolved_count = 0
      do i = 1, connection_count(this%connections)
         if (.not. this%connections(i)%resolved) unresolved_count = unresolved_count + 1
      end do
   end function unresolved_count

   subroutine get_unresolved_connection(this, index, source, target)
      class(GraphAssembly), intent(in) :: this
      integer, intent(in) :: index
      character(:), allocatable, intent(out) :: source, target
      integer :: i, count

      count = 0
      do i = 1, connection_count(this%connections)
         if (this%connections(i)%resolved) cycle
         count = count + 1
         if (count == index) then
            source = this%connections(i)%source
            target = this%connections(i)%target
            return
         end if
      end do
      source = ''
      target = ''
   end subroutine get_unresolved_connection

   subroutine build(this, graph, rc)
      class(GraphAssembly), intent(inout) :: this
      type(ComponentGraph), intent(out) :: graph
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%advertised_, 'GraphAssembly must complete advertisement before build.')
      call this%graph%freeze(_RC)
      graph = this%graph

      _RETURN(_SUCCESS)
   end subroutine build

   integer function find_endpoint(endpoints, name)
      type(Endpoint), allocatable, intent(in) :: endpoints(:)
      character(*), intent(in) :: name
      integer :: i

      find_endpoint = 0
      if (.not. allocated(endpoints)) return
      do i = 1, size(endpoints)
         if (endpoints(i)%name == name) then
            find_endpoint = i
            return
         end if
      end do
   end function find_endpoint

   subroutine append_endpoint(endpoints, endpoint_arg)
      type(Endpoint), allocatable, intent(inout) :: endpoints(:)
      type(Endpoint), intent(in) :: endpoint_arg

      if (allocated(endpoints)) then
         endpoints = [endpoints, endpoint_arg]
      else
         endpoints = [endpoint_arg]
      end if
   end subroutine append_endpoint

   subroutine append_connection(connections, connection_arg)
      type(Connection), allocatable, intent(inout) :: connections(:)
      type(Connection), intent(in) :: connection_arg

      if (allocated(connections)) then
         connections = [connections, connection_arg]
      else
         connections = [connection_arg]
      end if
   end subroutine append_connection

   pure integer function connection_count(connections)
      type(Connection), allocatable, intent(in) :: connections(:)

      if (allocated(connections)) then
         connection_count = size(connections)
      else
         connection_count = 0
      end if
   end function connection_count

end module mapl_GraphAssembly_mod
