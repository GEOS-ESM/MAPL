module mapl_VerticalGrid_API
   use mapl_VerticalGrid, only: VerticalGrid
   use mapl_VerticalGrid, only: VERTICAL_GRID_NOT_FOUND
   use mapl_VerticalGridSpec, only: VerticalGridSpec
   use mapl_VerticalGridFactory, only: VerticalGridFactory
   use mapl_VerticalGridManager, only: VerticalGridManager
   use mapl_VerticalGridManager, only: get_vertical_grid_manager
   use mapl_IntegerPair, only: IntegerPair
   use mapl_VerticalStaggerLoc
   use mapl_VerticalAlignment
   use mapl_BasicVerticalGrid, only: BasicVerticalGrid
   use mapl_BasicVerticalGrid, only: BasicVerticalGridSpec
   use mapl_BasicVerticalGrid, only: BasicVerticalGridFactory
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
   
   ! Vertical alignment
   public :: VerticalAlignment
   public :: VALIGN_WITH_GRID
   public :: VALIGN_UP
   public :: VALIGN_DOWN
   public :: VALIGN_INVALID
   
   ! Basic grid types
   public :: BasicVerticalGrid
   public :: BasicVerticalGridSpec
   public :: BasicVerticalGridFactory

   ! Parameters
   public :: VERTICAL_GRID_NOT_FOUND
  
   
end module mapl_VerticalGrid_API
