#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_GraphMode_mod: single, global selector deciding which producer
! populates a component's real `ESMF_State` entries: the legacy
! StateRegistry-based imperative coupler, the graph-native
! ComponentGraph/GraphBuilder machinery, or both side by side with no
! shared-state collision. The three values:
!
!   - GRAPH_MODE_REGISTRY (default): only the legacy coupler populates a
!     component's real `ESMF_State`; no graph-native materialization or
!     state population occurs.
!   - GRAPH_MODE_BOTH: graph-native materialization runs alongside the
!     legacy coupler but never inserts into the shared `ESMF_State` the
!     legacy coupler populates; a graph-materialized item's value is
!     readable only out of band, from its owning node's payload. This lets
!     graph-native behavior be compared against legacy with no
!     duplicate-insertion conflict.
!   - GRAPH_MODE_GRAPH: graph-native state population is the sole producer
!     of a component's `ESMF_State` entries; the legacy coupler's
!     population step is skipped for that component.
!
! Represented as a plain character value so call-site comparisons
! (`graph_mode() == GRAPH_MODE_GRAPH`) use the same words as the scenario
! vocabulary ("registry"/"graph"/"both"), with no to-string mapping to keep
! in sync.
!
! Unless set_graph_mode() is called (test code only), every run behaves as
! GRAPH_MODE_REGISTRY.
module mapl_GraphMode_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: set_graph_mode
   public :: graph_mode

   public :: GRAPH_MODE_REGISTRY
   public :: GRAPH_MODE_GRAPH
   public :: GRAPH_MODE_BOTH

   character(*), parameter :: GRAPH_MODE_REGISTRY = 'registry'
   character(*), parameter :: GRAPH_MODE_GRAPH = 'graph'
   character(*), parameter :: GRAPH_MODE_BOTH = 'both'

   character(:), allocatable, save :: current_mode

contains

   ! Test-only setter (no production code calls it). Only the three named
   ! values above are accepted; anything else is a caller defect, reported
   ! rather than normalized to a default.
   subroutine set_graph_mode(mode, rc)
      character(*), intent(in) :: mode
      integer, optional, intent(out) :: rc

      logical :: is_valid_mode
      character(:), allocatable :: msg

      is_valid_mode = any([mode == GRAPH_MODE_REGISTRY, mode == GRAPH_MODE_GRAPH, mode == GRAPH_MODE_BOTH])
      msg = 'mapl_GraphMode_mod: invalid graph mode <'//mode//'> - must be one of "'// &
           GRAPH_MODE_REGISTRY//'", "'//GRAPH_MODE_GRAPH//'", "'//GRAPH_MODE_BOTH//'"'
      _ASSERT(is_valid_mode, msg)

      current_mode = mode

      _RETURN(_SUCCESS)
   end subroutine set_graph_mode

   function graph_mode() result(mode)
      character(:), allocatable :: mode

      if (.not. allocated(current_mode)) then
         mode = GRAPH_MODE_REGISTRY
      else
         mode = current_mode
      end if
   end function graph_mode

end module mapl_GraphMode_mod
