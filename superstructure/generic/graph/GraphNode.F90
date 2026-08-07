#include "MAPL.h"
module mapl_GraphNode_mod
   use mapl_EdgeId_mod, only: EdgeId
   use mapl_EdgeIdSet_mod, only: EdgeIdSet
   use mapl_NodeId_mod, only: NodeId
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphNode

   type, abstract :: GraphNode
      private
      logical :: is_valid_ = .false.
      type(EdgeIdSet) :: incoming_edges_
      type(EdgeIdSet) :: outgoing_edges_
   contains
      procedure :: mark_valid
      procedure :: mark_invalid
      procedure :: is_valid
      procedure :: add_in_edge
      procedure :: add_out_edge
      procedure :: get_in_edges
      procedure :: get_out_edges
      procedure :: is_transform
      procedure :: update
      procedure :: constituents
   end type GraphNode

contains

   subroutine mark_valid(this)
      class(GraphNode), intent(inout) :: this

      this%is_valid_ = .true.
   end subroutine mark_valid

   subroutine mark_invalid(this)
      class(GraphNode), intent(inout) :: this

      this%is_valid_ = .false.
   end subroutine mark_invalid

   pure logical function is_valid(this)
      class(GraphNode), intent(in) :: this

      is_valid = this%is_valid_
   end function is_valid

   subroutine add_in_edge(this, edge_id)
      class(GraphNode), intent(inout) :: this
      type(EdgeId), intent(in) :: edge_id

      call this%incoming_edges_%insert(edge_id)
   end subroutine add_in_edge

   subroutine add_out_edge(this, edge_id)
      class(GraphNode), intent(inout) :: this
      type(EdgeId), intent(in) :: edge_id

      call this%outgoing_edges_%insert(edge_id)
   end subroutine add_out_edge

   function get_in_edges(this) result(edge_ids)
      class(GraphNode), target, intent(in) :: this
      type(EdgeIdSet), pointer :: edge_ids

      edge_ids => this%incoming_edges_
   end function get_in_edges

   function get_out_edges(this) result(edge_ids)
      class(GraphNode), target, intent(in) :: this
      type(EdgeIdSet), pointer :: edge_ids

      edge_ids => this%outgoing_edges_
   end function get_out_edges

   pure logical function is_transform(this)
      class(GraphNode), intent(in) :: this

      is_transform = .false.
   end function is_transform

   subroutine update(this, rc)
      class(GraphNode), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine update

   function constituents(this) result(node_ids)
      class(GraphNode), intent(in) :: this
      type(NodeId), allocatable :: node_ids(:)

      allocate(node_ids(0))
   end function constituents

end module mapl_GraphNode_mod
