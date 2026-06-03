! Export umbrella for the MAPL infrastructure/vertical/vertical_grid layer.
! Public API exposed to external consumers.
module mapl_vertical_grid_export

   use mapl_VerticalGrid_mod, only: VerticalGrid, VERTICAL_GRID_NOT_FOUND
   use mapl_VerticalGridSpec_mod, only: VerticalGridSpec
   use mapl_VerticalGridFactory_mod, only: VerticalGridFactory
   use mapl_BasicVerticalGrid_mod, only: BasicVerticalGrid, BasicVerticalGridSpec, BasicVerticalGridFactory
   use mapl_VerticalGridManager_mod, only: VerticalGridManager, get_vertical_grid_manager
   use mapl_VerticalStaggerLoc_mod
   use mapl_VerticalAlignment_mod

   implicit none
   private

   ! Abstract base types
   public :: VerticalGrid
   public :: VerticalGridSpec
   public :: VerticalGridFactory

   ! Concrete basic implementation
   public :: BasicVerticalGrid
   public :: BasicVerticalGridSpec
   public :: BasicVerticalGridFactory

   ! Manager
   public :: VerticalGridManager
   public :: get_vertical_grid_manager

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
