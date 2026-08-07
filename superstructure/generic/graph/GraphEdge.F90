module mapl_GraphEdge_mod
   use mapl_NodeId_mod, only: NodeId
   implicit none(type, external)
   private

   public :: GraphEdge

   type :: GraphEdge
      private
      type(NodeId) :: source_
      type(NodeId) :: target_
   contains
      procedure :: source
       procedure :: target
   end type GraphEdge

   interface GraphEdge
      procedure new_edge
   end interface

contains

   function new_edge(source, target) result(edge_)
      type(NodeId), intent(in) :: source, target
      type(GraphEdge) :: edge_

      edge_%source_ = source
      edge_%target_ = target
   end function new_edge

   pure function source(this) result(id)
      class(GraphEdge), intent(in) :: this
      type(NodeId) :: id

      id = this%source_
   end function source

   pure function target(this) result(id)
      class(GraphEdge), intent(in) :: this
      type(NodeId) :: id

      id = this%target_
   end function target

end module mapl_GraphEdge_mod
