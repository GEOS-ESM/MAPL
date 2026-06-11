! Export umbrella for the MAPL infrastructure/vertical/vertical_grid layer.
! Public API exposed to external consumers.
module mapl_vertical_grid_api

   use mapl_VerticalGrid_mod, only: mapl_VerticalGrid => VerticalGrid
   use mapl_VerticalGrid_mod, only: MAPL_VERTICAL_GRID_NOT_FOUND => VERTICAL_GRID_NOT_FOUND
   use mapl_VerticalGridSpec_mod, only: mapl_VerticalGridSpec => VerticalGridSpec
   use mapl_VerticalGridFactory_mod, only: mapl_VerticalGridFactory => VerticalGridFactory
   use mapl_BasicVerticalGrid_mod, only: mapl_BasicVerticalGrid => BasicVerticalGrid
   use mapl_BasicVerticalGrid_mod, only: mapl_BasicVerticalGridSpec => BasicVerticalGridSpec
   use mapl_BasicVerticalGrid_mod, only: mapl_BasicVerticalGridFactory => BasicVerticalGridFactory
   use mapl_VerticalGridManager_mod, only: mapl_VerticalGridManager => VerticalGridManager
   use mapl_VerticalGridManager_mod, only: mapl_get_vertical_grid_manager => get_vertical_grid_manager

   implicit none
   private

   ! Abstract base types
   public :: mapl_VerticalGrid
   public :: mapl_VerticalGridSpec
   public :: mapl_VerticalGridFactory

   ! Concrete basic implementation
   public :: mapl_BasicVerticalGrid
   public :: mapl_BasicVerticalGridSpec
   public :: mapl_BasicVerticalGridFactory

   ! Manager
   public :: mapl_VerticalGridManager
   public :: mapl_get_vertical_grid_manager

   ! Parameters
   public :: MAPL_VERTICAL_GRID_NOT_FOUND

end module mapl_vertical_grid_api
