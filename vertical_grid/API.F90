module mapl3g_VerticalGrid_API
   use mapl3g_VerticalGrid, only: VerticalGrid
   use mapl3g_VerticalGrid, only: VERTICAL_GRID_NOT_FOUND
   use mapl3g_VerticalGridSpec, only: VerticalGridSpec
   use mapl3g_VerticalGridFactory, only: VerticalGridFactory
   use mapl3g_VerticalGridManager, only: VerticalGridManager
   use mapl3g_VerticalGridManager, only: get_vertical_grid_manager
   use mapl3g_IntegerPair, only: IntegerPair
   use mapl3g_VerticalStaggerLoc
   use mapl3g_BasicVerticalGrid, only: BasicVerticalGrid
   use mapl3g_BasicVerticalGrid, only: BasicVerticalGridSpec
   use mapl3g_BasicVerticalGrid, only: BasicVerticalGridFactory
   implicit none(type,external)
   private

   ! Abstract base types
   public :: VerticalGrid
   public :: VerticalGridSpec
   public :: VerticalGridFactory

   ! Manager
   public :: VerticalGridManager
   public :: get_vertical_grid_manager
   
   ! Utility types
   public :: IntegerPair
   
   ! Vertical stagger locations
   public :: VerticalStaggerLoc
   public :: operator(==), operator(/=)
   public :: VERTICAL_STAGGER_NONE
   public :: VERTICAL_STAGGER_EDGE
   public :: VERTICAL_STAGGER_CENTER
   public :: VERTICAL_STAGGER_MIRROR
   public :: VERTICAL_STAGGER_INVALID
   
   ! Basic grid types
   public :: BasicVerticalGrid
   public :: BasicVerticalGridSpec
   public :: BasicVerticalGridFactory

   ! Parameters
   public :: VERTICAL_GRID_NOT_FOUND
  
   
end module mapl3g_VerticalGrid_API
