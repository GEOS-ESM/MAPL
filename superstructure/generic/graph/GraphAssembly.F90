#include "MAPL.h"
module mapl_GraphAssembly_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphValue_mod, only: GraphValue
   use esmf, only: ESMF_Field, ESMF_State, ESMF_StateGet
   use gFTL2_StringStringMap, only: StringStringMap
   use mapl_GraphAssemblyStatus_mod, only: GraphAssemblyStatus
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
    public :: GRAPH_GEOM_ROLE_NONE, GRAPH_GEOM_ROLE_A, GRAPH_GEOM_ROLE_B
    public :: GRAPH_GEOM_ROLE_PROVIDER, GRAPH_GEOM_ROLE_FROM_PARENT
    public :: GRAPH_GEOM_ROLE_FROM_CHILD

    integer, parameter :: GRAPH_GEOM_ROLE_NONE = 0
    integer, parameter :: GRAPH_GEOM_ROLE_A = 1
    integer, parameter :: GRAPH_GEOM_ROLE_B = 2
    integer, parameter :: GRAPH_GEOM_ROLE_PROVIDER = 3
    integer, parameter :: GRAPH_GEOM_ROLE_FROM_PARENT = 4
    integer, parameter :: GRAPH_GEOM_ROLE_FROM_CHILD = 5

    type :: Endpoint
       character(:), allocatable :: name
       type(NodeId) :: node
       integer :: geometry_role = GRAPH_GEOM_ROLE_NONE
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
       type(StringStringMap) :: aliases_
       type(Connection), allocatable :: connections(:)
       type(GraphAssemblyStatus) :: status_
   contains
       procedure :: initialize
       procedure :: declare_field
       procedure :: declare_geom
       procedure :: declare_alias
       procedure :: declare_connection
       procedure :: advertise
       procedure :: modify_advertise
       procedure :: is_ready
       procedure :: is_advertised
       procedure :: is_realized
       procedure :: unresolved_count
       procedure :: get_unresolved_connection
       procedure :: get_geometry_role
       procedure :: realize_fields
       procedure :: realize
       procedure :: build
   end type GraphAssembly

contains

   subroutine initialize(this, rc)
      class(GraphAssembly), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%graph%initialize(_RC)
       call this%status_%mark_initialized()
      _RETURN(_SUCCESS)
   end subroutine initialize

   function declare_field(this, name, spec, rc) result(node)
      class(GraphAssembly), intent(inout) :: this
      character(*), intent(in) :: name
      type(ItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc
      type(NodeId) :: node

      integer :: status

       _ASSERT(this%status_%is_declaring(), 'GraphAssembly must be initialized before declaration.')
       _ASSERT(find_endpoint(this%endpoints, name) == 0, 'Graph endpoint already declared.')
       _ASSERT(this%aliases_%count(name) == 0, 'Graph alias already declared.')

      node = this%graph%add_node(ValueGraphNode(FieldGraphValue(name, spec)), _RC)
      call append_endpoint(this%endpoints, Endpoint(name, node))

      _RETURN(_SUCCESS)
   end function declare_field

    function declare_geom(this, name, geom_spec, rc, role) result(node)
       class(GraphAssembly), intent(inout) :: this
       character(*), intent(in) :: name
       class(GeomSpec), intent(in) :: geom_spec
       integer, optional, intent(out) :: rc
       integer, optional, intent(in) :: role
       type(NodeId) :: node

       integer :: status, geometry_role

       _ASSERT(this%status_%is_declaring(), 'GraphAssembly must be initialized before declaration.')
       _ASSERT(find_endpoint(this%endpoints, name) == 0, 'Graph endpoint already declared.')
       _ASSERT(this%aliases_%count(name) == 0, 'Graph alias already declared.')
       if (present(role)) then
          _ASSERT(role >= GRAPH_GEOM_ROLE_NONE .and. role <= GRAPH_GEOM_ROLE_FROM_CHILD, &
             'Invalid graph geometry role.')
       end if

       node = this%graph%add_node( &
          ValueGraphNode(GeomGraphValue(geom_spec, name)), _RC)
       geometry_role = GRAPH_GEOM_ROLE_NONE
       if (present(role)) geometry_role = role
       call append_endpoint(this%endpoints, Endpoint(name, node, geometry_role))

      _RETURN(_SUCCESS)
    end function declare_geom

    subroutine declare_alias(this, alias, target, rc)
       class(GraphAssembly), intent(inout) :: this
       character(*), intent(in) :: alias, target
       integer, optional, intent(out) :: rc

       integer :: status

       _ASSERT(this%status_%is_declaring(), 'GraphAssembly must be initialized before declaration.')
       _ASSERT(find_endpoint(this%endpoints, alias) == 0, 'Graph endpoint already declared.')
       _ASSERT(this%aliases_%count(alias) == 0, 'Graph alias already declared.')
       call this%aliases_%insert(alias, target)

       _RETURN(_SUCCESS)
    end subroutine declare_alias

    subroutine declare_connection(this, source, target, rc)
      class(GraphAssembly), intent(inout) :: this
      character(*), intent(in) :: source, target
      integer, optional, intent(out) :: rc

       _ASSERT(this%status_%is_declaring(), 'GraphAssembly must be initialized before declaration.')
      call append_connection(this%connections, Connection(source, target))

       _RETURN(_SUCCESS)
    end subroutine declare_connection

    subroutine advertise(this, rc)
       class(GraphAssembly), intent(inout) :: this
       integer, optional, intent(out) :: rc

       integer :: status

       _ASSERT(this%status_%is_declaring(), 'GraphAssembly must be initialized before advertisement.')
       call this%status_%mark_advertised()

       _RETURN(_SUCCESS)
    end subroutine advertise

    subroutine modify_advertise(this, rc, progress, ready)
       class(GraphAssembly), intent(inout) :: this
       integer, optional, intent(out) :: rc
       logical, optional, intent(out) :: progress, ready

       integer :: status, i, source_index, target_index, initial_unresolved
       logical :: made_progress
      type(NodeId) :: source, target
      type(EdgeId) :: edge_id

       _ASSERT(this%status_%is_initialized(), 'GraphAssembly must be initialized before advertisement.')
       _ASSERT(.not. this%status_%is_modified(), 'GraphAssembly modify_advertise already complete.')
       _ASSERT(.not. this%status_%is_realized(), 'Cannot modify realized GraphAssembly.')
       ! Preserve legacy callers that enter directly at modify_advertise.
       if (this%status_%is_declaring()) call this%status_%mark_advertised()

       initial_unresolved = this%unresolved_count()
       made_progress = .false.

       ! Resolve until one complete pass makes no changes.  Readiness means
       ! every declared connection resolved during this fixed point.
       do
          do i = 1, connection_count(this%connections)
            if (this%connections(i)%resolved) cycle
             source_index = find_resolved_endpoint(this%endpoints, this%aliases_, &
                this%connections(i)%source)
             target_index = find_resolved_endpoint(this%endpoints, this%aliases_, &
                this%connections(i)%target)
            if (source_index == 0 .or. target_index == 0) cycle

            source = this%endpoints(source_index)%node
            target = this%endpoints(target_index)%node
            edge_id = this%graph%add_edge(GraphEdge(source, target), rc=status)
            _UNUSED_DUMMY(edge_id)
            _ASSERT(status == _SUCCESS, 'Failed to add declared graph connection.')
             this%connections(i)%resolved = .true.
             made_progress = .true.
          end do
          if (.not. made_progress) exit
          made_progress = .false.
       end do

       call this%status_%mark_modified()
       if (present(progress)) progress = this%unresolved_count() < initial_unresolved
       if (present(ready)) ready = this%unresolved_count() == 0
       _RETURN(_SUCCESS)
    end subroutine modify_advertise

    logical function is_advertised(this)
       class(GraphAssembly), intent(in) :: this
       is_advertised = this%status_%is_advertised() .or. this%status_%is_modified() .or. &
          this%status_%is_realized()
    end function is_advertised

    logical function is_ready(this)
       class(GraphAssembly), intent(in) :: this
       is_ready = this%status_%is_modified() .and. this%unresolved_count() == 0
    end function is_ready

    logical function is_realized(this)
       class(GraphAssembly), intent(in) :: this
       is_realized = this%status_%is_realized()
    end function is_realized

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

    subroutine get_geometry_role(this, name, role, found)
       class(GraphAssembly), intent(in) :: this
       character(*), intent(in) :: name
       integer, intent(out) :: role
       logical, optional, intent(out) :: found
       integer :: index

       index = find_endpoint(this%endpoints, name)
       if (index == 0) then
          role = GRAPH_GEOM_ROLE_NONE
          if (present(found)) found = .false.
       else
          role = this%endpoints(index)%geometry_role
          if (present(found)) found = .true.
       end if
    end subroutine get_geometry_role

    subroutine realize_fields(this, state, rc)
       class(GraphAssembly), target, intent(inout) :: this
       type(ESMF_State), intent(inout) :: state
       integer, optional, intent(out) :: rc

       integer :: status, i
       type(ESMF_Field) :: field
       class(GraphNode), pointer :: node
       class(GraphValue), pointer :: value

       _ASSERT(this%is_ready(), 'GraphAssembly must be ready before field realization.')

       do i = 1, endpoint_count(this%endpoints)
          node => this%graph%get_node(this%endpoints(i)%node, _RC)
          select type (node)
          type is (ValueGraphNode)
             value => node%value()
             select type (value)
             type is (FieldGraphValue)
                call ESMF_StateGet(state, itemName=value%name(), field=field, _RC)
                call value%bind_field(field)
             end select
          end select
       end do

       _RETURN(_SUCCESS)
    end subroutine realize_fields

    subroutine realize(this, graph, rc)
       class(GraphAssembly), intent(inout) :: this
       type(ComponentGraph), intent(out) :: graph
       integer, optional, intent(out) :: rc

       integer :: status

       _ASSERT(this%is_ready(), 'GraphAssembly has unresolved connections.')
       _ASSERT(.not. this%status_%is_realized(), 'GraphAssembly already realized.')
       call this%graph%freeze(_RC)
       graph = this%graph
       call this%status_%mark_realized()

       _RETURN(_SUCCESS)
    end subroutine realize

   subroutine build(this, graph, rc)
      class(GraphAssembly), intent(inout) :: this
      type(ComponentGraph), intent(out) :: graph
      integer, optional, intent(out) :: rc

      integer :: status

       call this%realize(graph, _RC)

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

    integer function find_resolved_endpoint(endpoints, aliases, name)
       type(Endpoint), allocatable, intent(in) :: endpoints(:)
       type(StringStringMap), intent(in) :: aliases
       character(*), intent(in) :: name
       character(:), allocatable :: target

       find_resolved_endpoint = find_endpoint(endpoints, name)
       if (find_resolved_endpoint /= 0) return

       if (aliases%count(name) == 0) return
       target = aliases%at(name)
       if (aliases%count(target) /= 0) then
          ! Alias chains remain metadata-only until their final endpoint exists.
          target = aliases%at(target)
       end if
       find_resolved_endpoint = find_endpoint(endpoints, target)
    end function find_resolved_endpoint

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

    pure integer function endpoint_count(endpoints)
       type(Endpoint), allocatable, intent(in) :: endpoints(:)

       if (allocated(endpoints)) then
          endpoint_count = size(endpoints)
       else
          endpoint_count = 0
       end if
    end function endpoint_count

end module mapl_GraphAssembly_mod
