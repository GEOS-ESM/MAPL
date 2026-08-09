#include "MAPL.h"
module mapl_GraphBuilder_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_NodeId_mod, only: NodeId, operator(==)
   use mapl_EdgeId_mod, only: EdgeId
   use mapl_GraphEdge_mod, only: GraphEdge
   use mapl_GraphTransform_mod, only: GraphTransform, GraphPortSpec, GraphValueRef
   use mapl_GraphValueSpec_mod, only: GraphValueSpec
   use mapl_ValueGraphNode_mod, only: ValueGraphNode
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use mapl_ItemSpec_mod, only: ItemSpec, operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   integer, parameter, public :: GRAPH_BUILDER_SUCCESS = 0
   integer, parameter, public :: GRAPH_BUILDER_UNKNOWN_SOURCE = 101
   integer, parameter, public :: GRAPH_BUILDER_INCOMPATIBLE_CONNECTION = 102
   integer, parameter, public :: GRAPH_BUILDER_INVALID_GRAPH = 103
   integer, parameter, public :: GRAPH_BUILDER_ALREADY_BUILT = 104

   integer, parameter :: TRANSFORM_PRECISION = 1
   integer, parameter :: TRANSFORM_UNITS = 2

   public :: GraphBuilder

   type :: Representation
      type(NodeId) :: node
      type(ItemSpec) :: spec
   end type Representation

   type :: TransformDescriptor
      integer :: kind = 0
      type(ItemSpec) :: target_spec
   end type TransformDescriptor

   type :: TransformResult
      type(NodeId) :: source
      type(TransformDescriptor) :: descriptor
      type(NodeId) :: result
   end type TransformResult

   type, extends(GraphTransform) :: DescriptorTransform
      type(ItemSpec) :: source_spec
      type(ItemSpec) :: target_spec
   contains
      procedure :: input_specs => descriptor_input_specs
      procedure :: output_specs => descriptor_output_specs
      procedure :: execute => descriptor_execute
   end type DescriptorTransform

   type :: GraphBuilder
      private
       type(ComponentGraph) :: graph
      type(Representation), allocatable :: representations(:)
      type(TransformResult), allocatable :: transform_results(:)
      logical :: built = .false.
   contains
      procedure :: register_representation
      procedure :: satisfy
      procedure :: build
   end type GraphBuilder

contains

   function register_representation(this, spec, status) result(node)
      class(GraphBuilder), intent(inout) :: this
      type(ItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: status
      type(NodeId) :: node
      integer :: graph_status

      if (this%built) then
         call set_status(status, GRAPH_BUILDER_ALREADY_BUILT)
         return
      end if

      node = this%graph%add_node(ValueGraphNode(spec), graph_status)
       if (graph_status /= GRAPH_BUILDER_SUCCESS) then
         call set_status(status, GRAPH_BUILDER_INVALID_GRAPH)
         return
      end if
      call append_representation(this%representations, Representation(node, spec))
      call set_status(status, GRAPH_BUILDER_SUCCESS)
   end function register_representation

   function satisfy(this, source, required_spec, status) result(result)
      class(GraphBuilder), intent(inout) :: this
      type(NodeId), intent(in) :: source
      type(ItemSpec), intent(in) :: required_spec
      integer, optional, intent(out) :: status
      type(NodeId) :: result
      type(ItemSpec) :: current_spec, next_spec
      type(NodeId) :: current, next
      integer :: source_index, local_status

      if (this%built) then
         call set_status(status, GRAPH_BUILDER_ALREADY_BUILT)
         return
      end if

      source_index = find_representation(this%representations, source)
      if (source_index == 0) then
         call set_status(status, GRAPH_BUILDER_UNKNOWN_SOURCE)
         return
      end if

      current = source
      current_spec = this%representations(source_index)%spec
      if (current_spec == required_spec) then
         result = source
         call set_status(status, GRAPH_BUILDER_SUCCESS)
         return
      end if
      if (.not. current_spec%structurally_compatible(required_spec)) then
         call set_status(status, GRAPH_BUILDER_INCOMPATIBLE_CONNECTION)
         return
      end if

      if (current_spec%precision() /= required_spec%precision()) then
         next_spec = current_spec%with_precision(required_spec%precision())
         call get_or_add_transform(this, current, current_spec, &
            TransformDescriptor(TRANSFORM_PRECISION, next_spec), next, local_status)
         if (local_status /= GRAPH_BUILDER_SUCCESS) then
            call set_status(status, local_status)
            return
         end if
         current = next
         current_spec = next_spec
      end if

      if (current_spec%units() /= required_spec%units()) then
         next_spec = current_spec%with_units(required_spec%units())
         call get_or_add_transform(this, current, current_spec, &
            TransformDescriptor(TRANSFORM_UNITS, next_spec), next, local_status)
         if (local_status /= GRAPH_BUILDER_SUCCESS) then
            call set_status(status, local_status)
            return
         end if
         current = next
      end if

      result = current
      call set_status(status, GRAPH_BUILDER_SUCCESS)
   end function satisfy

   subroutine build(this, graph, status)
      class(GraphBuilder), intent(inout) :: this
       type(ComponentGraph), intent(out) :: graph
      integer, intent(out) :: status
      integer :: graph_status

      if (this%built) then
         status = GRAPH_BUILDER_ALREADY_BUILT
         return
      end if

      call this%graph%freeze(graph_status)
       if (graph_status /= GRAPH_BUILDER_SUCCESS) then
         status = GRAPH_BUILDER_INVALID_GRAPH
         return
      end if
      this%built = .true.
      graph = this%graph
      status = GRAPH_BUILDER_SUCCESS
   end subroutine build

   subroutine get_or_add_transform(this, source, source_spec, descriptor, result, status)
      class(GraphBuilder), intent(inout) :: this
      type(NodeId), intent(in) :: source
      type(ItemSpec), intent(in) :: source_spec
      type(TransformDescriptor), intent(in) :: descriptor
      type(NodeId), intent(out) :: result
      integer, intent(out) :: status
       type(NodeId) :: transform
       type(EdgeId) :: edge_id
       integer :: i, graph_status

      do i = 1, transform_result_count(this%transform_results)
         if (this%transform_results(i)%source == source .and. &
             same_descriptor(this%transform_results(i)%descriptor, descriptor)) then
            result = this%transform_results(i)%result
            status = GRAPH_BUILDER_SUCCESS
            return
         end if
      end do

       block
          type(DescriptorTransform) :: operation
          operation%source_spec = source_spec
          operation%target_spec = descriptor%target_spec
          transform = this%graph%add_node(TransformGraphNode(operation), graph_status)
       end block
       if (graph_status /= GRAPH_BUILDER_SUCCESS) then
         status = GRAPH_BUILDER_INVALID_GRAPH
         return
      end if
      result = this%graph%add_node(ValueGraphNode(descriptor%target_spec), graph_status)
       if (graph_status /= GRAPH_BUILDER_SUCCESS) then
         status = GRAPH_BUILDER_INVALID_GRAPH
         return
      end if
       edge_id = add_graph_edge(this%graph, source, transform, graph_status)
       if (graph_status /= GRAPH_BUILDER_SUCCESS) then
         status = GRAPH_BUILDER_INVALID_GRAPH
         return
      end if
       edge_id = add_graph_edge(this%graph, transform, result, graph_status)
       if (graph_status /= GRAPH_BUILDER_SUCCESS) then
         status = GRAPH_BUILDER_INVALID_GRAPH
         return
      end if

      call append_representation(this%representations, Representation(result, descriptor%target_spec))
      call append_transform_result(this%transform_results, TransformResult(source, descriptor, result))
      status = GRAPH_BUILDER_SUCCESS
   end subroutine get_or_add_transform

   logical function same_descriptor(lhs, rhs)
      type(TransformDescriptor), intent(in) :: lhs, rhs
      same_descriptor = lhs%kind == rhs%kind .and. lhs%target_spec == rhs%target_spec
   end function same_descriptor

   function transform_name(descriptor) result(name)
      type(TransformDescriptor), intent(in) :: descriptor
      character(:), allocatable :: name

      select case (descriptor%kind)
      case (TRANSFORM_PRECISION)
         name = 'precision'
      case (TRANSFORM_UNITS)
         name = 'units'
      case default
         name = 'unknown'
      end select
   end function transform_name

   integer function find_representation(values, node) result(index)
      type(Representation), allocatable, intent(in) :: values(:)
      type(NodeId), intent(in) :: node
      integer :: i

      index = 0
      do i = 1, representation_count(values)
         if (values(i)%node == node) then
            index = i
            return
         end if
      end do
   end function find_representation

    function add_graph_edge(graph, source, target, status) result(edge_id)
       type(ComponentGraph), intent(inout) :: graph
       type(NodeId), intent(in) :: source, target
       integer, intent(out) :: status
       type(EdgeId) :: edge_id

       edge_id = graph%add_edge(GraphEdge(source, target), rc=status)
    end function add_graph_edge

   function descriptor_input_specs(this) result(specs)
      class(DescriptorTransform), intent(in) :: this
      type(GraphPortSpec), allocatable :: specs(:)

      allocate(specs(1))
      specs(1) = GraphPortSpec('input', GraphValueSpec(this%source_spec%category()))
   end function descriptor_input_specs

   function descriptor_output_specs(this) result(specs)
      class(DescriptorTransform), intent(in) :: this
      type(GraphPortSpec), allocatable :: specs(:)

      allocate(specs(1))
      specs(1) = GraphPortSpec('output', GraphValueSpec(this%target_spec%category()))
   end function descriptor_output_specs

   subroutine descriptor_execute(this, inputs, outputs, rc)
      class(DescriptorTransform), intent(inout) :: this
      type(GraphValueRef), intent(in) :: inputs(:)
      type(GraphValueRef), intent(inout) :: outputs(:)
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(inputs)
      _UNUSED_DUMMY(outputs)
      _RETURN(_SUCCESS)
   end subroutine descriptor_execute

   subroutine append_representation(values, value)
      type(Representation), allocatable, intent(inout) :: values(:)
      type(Representation), intent(in) :: value
      type(Representation), allocatable :: tmp(:)
      integer :: n

      n = representation_count(values)
      allocate(tmp(n + 1))
      if (n > 0) tmp(:n) = values
      tmp(n + 1) = value
      call move_alloc(tmp, values)
   end subroutine append_representation

   subroutine append_transform_result(values, value)
      type(TransformResult), allocatable, intent(inout) :: values(:)
      type(TransformResult), intent(in) :: value
      type(TransformResult), allocatable :: tmp(:)
      integer :: n

      n = transform_result_count(values)
      allocate(tmp(n + 1))
      if (n > 0) tmp(:n) = values
      tmp(n + 1) = value
      call move_alloc(tmp, values)
   end subroutine append_transform_result

   pure integer function representation_count(values)
      type(Representation), allocatable, intent(in) :: values(:)
      if (allocated(values)) then
         representation_count = size(values)
      else
         representation_count = 0
      end if
   end function representation_count

   pure integer function transform_result_count(values)
      type(TransformResult), allocatable, intent(in) :: values(:)
      if (allocated(values)) then
         transform_result_count = size(values)
      else
         transform_result_count = 0
      end if
   end function transform_result_count

   subroutine set_status(status, value)
      integer, optional, intent(out) :: status
      integer, intent(in) :: value
      if (present(status)) status = value
   end subroutine set_status

end module mapl_GraphBuilder_mod
