#include "MAPL.h"

!------------------------------------------------------------------------------
! NodeLabel: a data-only enrichment entry - a human-readable label - mapped
! one-per-NodeId by the NodeIdLabelMap container. Built by GraphBuilder's
! enrichment procedure (build_label_map in GraphBuilder.F90) and consumed,
! read-only, by GraphExport.F90's export_graph_dot/export_graph_json.
!------------------------------------------------------------------------------
module mapl_NodeLabel_mod
   implicit none(type, external)
   private

   public :: NodeLabel

   type :: NodeLabel
      private
      character(:), allocatable :: label
   contains
      procedure :: get_label => nodelabel_get_label
   end type NodeLabel

   interface NodeLabel
      module procedure new_NodeLabel
   end interface NodeLabel

contains

   function new_NodeLabel(label) result(this)
      character(*), intent(in) :: label
      type(NodeLabel) :: this

      this%label = label
   end function new_NodeLabel

   function nodelabel_get_label(this) result(label)
      class(NodeLabel), intent(in) :: this
      character(:), allocatable :: label

      label = this%label
   end function nodelabel_get_label

end module mapl_NodeLabel_mod
