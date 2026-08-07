#include "MAPL.h"
module mapl_ValueGraphNode_mod
   use mapl_NodeRevision_mod, only: NodeRevision, INVALID_REVISION
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphValue_mod, only: GraphValue
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ValueGraphNode

   type, extends(GraphNode) :: ValueGraphNode
      private
      class(GraphValue), allocatable :: value_
      type(NodeRevision) :: revision_ = INVALID_REVISION
   contains
      procedure :: value
      procedure :: value_spec
      procedure :: revision
      procedure :: advance_revision
      procedure :: has_revision
   end type ValueGraphNode

   interface ValueGraphNode
      module procedure new_value_graph_node
   end interface ValueGraphNode

contains

   function new_value_graph_node(value) result(node)
      class(GraphValue), intent(in) :: value
      type(ValueGraphNode) :: node

      allocate(node%value_, source=value)
   end function new_value_graph_node

   function value(this) result(payload)
      class(ValueGraphNode), target, intent(inout) :: this
      class(GraphValue), pointer :: payload

      payload => this%value_
   end function value

   function value_spec(this) result(spec)
      use mapl_GraphValueSpec_mod, only: GraphValueSpec
      class(ValueGraphNode), intent(in) :: this
      type(GraphValueSpec) :: spec

      spec = this%value_%spec()
   end function value_spec

   pure function revision(this)
      class(ValueGraphNode), intent(in) :: this
      type(NodeRevision) :: revision

      revision = this%revision_
   end function revision

   subroutine advance_revision(this, rc)
      class(ValueGraphNode), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      
      call this%revision_%advance(_RC)

      _RETURN(_SUCCESS)
   end subroutine advance_revision
   
   pure logical function has_revision(this)
      class(ValueGraphNode), intent(in) :: this
      has_revision = this%revision_%is_valid()
   end function has_revision

end module mapl_ValueGraphNode_mod
