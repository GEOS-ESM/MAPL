#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_GraphMode_mod: single, global selector deciding which producer
! populates a component's real `ESMF_State` entries - the legacy
! StateRegistry-based imperative coupler, the graph-native
! ComponentGraph/GraphBuilder machinery under development
! (docs/graph/spec/20-implementation-roadmap.md), or both running side
! by side with no shared-state collision.
!
! openspec/changes/phase-10a-state-materialization: replaces this
! module's earlier binary flag (set_graph_native_enabled/
! graph_native_enabled) with a 3-way selector, per that change's
! design.md "mapl_GraphMode_mod's 3-way selector replaces, rather than
! wraps, the old binary API" decision. The three values:
!
!   - GRAPH_MODE_REGISTRY (default): zero behavior change from before
!     this module existed - only the legacy imperative coupler populates
!     a component's real `ESMF_State`; no graph-native materialization
!     or state population occurs.
!   - GRAPH_MODE_BOTH: graph-native materialization runs alongside the
!     legacy coupler, but never inserts into the shared `ESMF_State` the
!     legacy coupler populates - a graph-materialized item's value is
!     readable only out of band, directly from its owning node's
!     payload (openspec/changes/phase-10a-state-materialization specs/
!     graph/graph-mode/spec.md "both mode runs both producers without a
!     shared-state collision"). This is what lets later Phase 10
!     sub-changes compare graph-native behavior against legacy with no
!     risk of a duplicate-insertion conflict in a real `ESMF_State`.
!   - GRAPH_MODE_GRAPH: the true-replacement mode - graph-native state
!     population is the sole real producer of a component's `ESMF_State`
!     entries; the legacy coupler's own population step is skipped
!     entirely for that component. Not turned on end-to-end against any
!     real fixture by any 10a-10d sub-change (see that phase's own
!     roadmap section, "demonstrate before replace") - the selector and
!     every call site gated on it are directly unit-testable regardless.
!
! Represented as a plain, constrained character value (not a derived
! enumerated type) - design.md left the exact representation to the
! implementer; a character value keeps every call site's comparison
! (`graph_mode() == GRAPH_MODE_GRAPH`) and every spec scenario's own
! vocabulary ("registry"/"graph"/"both") in the same words, with no
! separate to-string mapping to keep in sync.
!
! Default GRAPH_MODE_REGISTRY: every real production run behaves exactly
! as it did before this module existed, unless something explicitly
! calls set_graph_mode() - today that remains test code only, same as
! the binary flag this selector replaces.
!------------------------------------------------------------------------------
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

   ! Test-only (and future real-config-driven) setter. No production
   ! initialization code calls this today. Only the three named values
   ! above are accepted - anything else is a caller defect, reported
   ! rather than silently normalized to a default.
   subroutine set_graph_mode(mode, rc)
      character(*), intent(in) :: mode
      integer, optional, intent(out) :: rc

      _ASSERT(any([mode == GRAPH_MODE_REGISTRY, mode == GRAPH_MODE_GRAPH, mode == GRAPH_MODE_BOTH]), &
           'mapl_GraphMode_mod: invalid graph mode <'//mode//'> - must be one of '// &
           '"'//GRAPH_MODE_REGISTRY//'", "'//GRAPH_MODE_GRAPH//'", "'//GRAPH_MODE_BOTH//'"')

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
