#include "MAPL.h"

!------------------------------------------------------------------------------
! NodeLabel: a Data-only enrichment entry - a human-readable label plus
! an is_proxy flag - mapped one-per-NodeId by the NodeIdLabelMap
! container (visualization-enrichment-layer change, design.md
! Decisions "Proxy marking travels alongside the label"). Built by
! GraphBuilder's enrichment procedure (GraphBuilder.F90's
! build_label_map) and consumed, read-only, by GraphExport.F90's
! export_graph_dot/export_graph_json.
!------------------------------------------------------------------------------
module mapl_NodeLabel_mod
   implicit none(type, external)
   private

   public :: NodeLabel

   type :: NodeLabel
      private
      character(:), allocatable :: label
      logical :: is_proxy = .false.
   contains
      procedure :: get_label => nodelabel_get_label
      procedure :: get_is_proxy => nodelabel_get_is_proxy
   end type NodeLabel

   interface NodeLabel
      module procedure new_NodeLabel
   end interface NodeLabel

contains

   function new_NodeLabel(label, is_proxy) result(this)
      character(*), intent(in) :: label
      logical, optional, intent(in) :: is_proxy
      type(NodeLabel) :: this

      this%label = label
      this%is_proxy = .false.
      if (present(is_proxy)) this%is_proxy = is_proxy
   end function new_NodeLabel

   function nodelabel_get_label(this) result(label)
      class(NodeLabel), intent(in) :: this
      character(:), allocatable :: label

      label = this%label
   end function nodelabel_get_label

   logical function nodelabel_get_is_proxy(this) result(is_proxy)
      class(NodeLabel), intent(in) :: this

      is_proxy = this%is_proxy
   end function nodelabel_get_is_proxy

end module mapl_NodeLabel_mod
