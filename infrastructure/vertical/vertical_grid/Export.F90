! Export umbrella for the MAPL infrastructure/vertical/vertical_grid layer.
! Public API exposed to external consumers.
module mapl_vertical_grid_export

   use mapl_VerticalGrid_mod, only: VerticalGrid, VERTICAL_GRID_NOT_FOUND
   use mapl_VerticalGridSpec_mod, only: VerticalGridSpec
   use mapl_VerticalGridFactory_mod, only: VerticalGridFactory
   use mapl_VerticalStaggerLoc_mod
   use mapl_VerticalAlignment_mod
   ! mapl_BasicVerticalGrid_mod is intentionally NOT used here at module level.
   ! BasicVerticalGridFactory is a concrete subtype of VerticalGridFactory; using it here
   ! at module level embeds its vtable in every consumer's .mod file, causing non-deterministic
   ! GFortran vtable orderings.  Files that need BasicVerticalGrid* must use
   ! mapl_BasicVerticalGrid_mod directly.

   implicit none
   private

   ! Abstract base types
   public :: VerticalGrid
   public :: VerticalGridSpec
   public :: VerticalGridFactory

   ! Manager
   ! (VerticalGridManager is exposed via MAPL.F90 to avoid vtable pollution)

   ! Vertical stagger locations
   public :: VerticalStaggerLoc
   public :: operator(==)
   public :: operator(/=)
   public :: VERTICAL_STAGGER_NONE
   public :: VERTICAL_STAGGER_EDGE
   public :: VERTICAL_STAGGER_CENTER
   public :: VERTICAL_STAGGER_MIRROR
   public :: VERTICAL_STAGGER_INVALID

   ! Vertical alignment
   public :: VerticalAlignment
   public :: VALIGN_WITH_GRID
   public :: VALIGN_UP
   public :: VALIGN_DOWN
   public :: VALIGN_INVALID

   ! Parameters
   public :: VERTICAL_GRID_NOT_FOUND

end module mapl_vertical_grid_export
