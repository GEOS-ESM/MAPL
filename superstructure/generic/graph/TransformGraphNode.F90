module mapl_TransformGraphNode_mod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphTransform_mod, only: GraphTransform, GraphPortSpec
   use mapl_NodeRevision_mod, only: NodeRevision
   implicit none(type, external)
   private

   public :: TransformGraphNode

   type, extends(GraphNode) :: TransformGraphNode
      private
      class(GraphTransform), allocatable :: transform_
      type(NodeRevision), allocatable :: input_revisions_(:)
   contains
      procedure :: transform
      procedure :: input_specs
      procedure :: output_specs
      procedure :: is_transform
   end type TransformGraphNode

   interface TransformGraphNode
      module procedure new_transform_graph_node
   end interface TransformGraphNode

contains

   function new_transform_graph_node(transform) result(node)
      class(GraphTransform), intent(in) :: transform
      type(TransformGraphNode) :: node

      allocate(node%transform_, source=transform)
   end function new_transform_graph_node

   function transform(this) result(operation)
      class(TransformGraphNode), target, intent(inout) :: this
      class(GraphTransform), pointer :: operation

      operation => this%transform_
   end function transform

   function input_specs(this) result(specs)
      class(TransformGraphNode), intent(in) :: this
      type(GraphPortSpec), allocatable :: specs(:)

      specs = this%transform_%input_specs()
   end function input_specs

   function output_specs(this) result(specs)
      class(TransformGraphNode), intent(in) :: this
      type(GraphPortSpec), allocatable :: specs(:)

      specs = this%transform_%output_specs()
   end function output_specs

   pure logical function is_transform(this)
      class(TransformGraphNode), intent(in) :: this

      is_transform = .true.
   end function is_transform

end module mapl_TransformGraphNode_mod
