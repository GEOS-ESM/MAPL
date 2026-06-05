! Export umbrella for the MAPL infrastructure/vertical/vertical_grid layer.
! Public API exposed to external consumers.
module mapl_vertical_grid_api

   use mapl_VerticalGrid_mod, only: VerticalGrid, VERTICAL_GRID_NOT_FOUND
   use mapl_VerticalGridSpec_mod, only: VerticalGridSpec
   use mapl_VerticalGridFactory_mod, only: VerticalGridFactory
   use mapl_BasicVerticalGrid_mod, only: BasicVerticalGrid, BasicVerticalGridSpec, BasicVerticalGridFactory
   use mapl_VerticalGridManager_mod, only: VerticalGridManager, get_vertical_grid_manager

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

   ! Parameters
   public :: VERTICAL_GRID_NOT_FOUND

end module mapl_vertical_grid_api
