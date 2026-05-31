! Export umbrella for the MAPL infrastructure/vertical/vertical_grid layer.
! Public API exposed to external consumers.
module mapl_vertical_grid_export

   use mapl_VerticalGrid_mod, only: VerticalGrid, VERTICAL_GRID_NOT_FOUND
   use mapl_VerticalGridSpec_mod, only: VerticalGridSpec
   use mapl_VerticalGridFactory_mod, only: VerticalGridFactory
   use mapl_VerticalGridManager_mod, only: VerticalGridManager, get_vertical_grid_manager
   use mapl_IntegerPair_mod, only: IntegerPair
   use mapl_VerticalStaggerLoc_mod
   use mapl_VerticalAlignment_mod
   use mapl_BasicVerticalGrid_mod, only: BasicVerticalGrid, BasicVerticalGridSpec, BasicVerticalGridFactory

   implicit none
   private

   ! Abstract base types

   ! Manager

   ! Utility types

   ! Vertical stagger locations

   ! Vertical alignment

   ! Basic grid types

   ! Parameters

end module mapl_vertical_grid_export
