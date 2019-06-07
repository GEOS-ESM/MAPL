module MAPL_GridSpecMod
   use MAPL_GridTypeMod
   implicit none
   private

   public :: GridSpec
   public :: DimensionSpec
   ! parameters
   public :: MAPL_DimTopoCyclic
   public :: MAPL_DimTopoEdge
   public :: MAPL_DimTopoCenter


   integer, parameter :: NUM_DIMS = 2

   type :: DimensionSpec
      integer :: num_points
      integer :: topology
      real :: x_min
      real :: x_max
   contains
      procedure :: equals_dim
      procedure :: not_equals_dim
      generic :: operator(==) => equals_dim
      generic :: operator(/=) => not_equals_dim
   end type DimensionSpec


   type :: GridSpec
      type (GridType) :: grid_type
      type (DimensionSpec) :: dim_specs(NUM_DIMS)
   contains
      procedure :: equals_grid
      procedure :: not_equals_grid
      generic :: operator(==) => equals_grid
      generic :: operator(/=) => not_equals_grid
   end type GridSpec


   integer, parameter :: MAPL_DimTopoCyclic = 0
   integer, parameter :: MAPL_DimTopoEdge   = -1
   integer, parameter :: MAPL_DimTopoCenter = 1


contains

   
   impure elemental logical function equals_grid(a, b) result(equals)
      class (GridSpec), intent(in) :: a
      class (GridSpec), intent(in) :: b

      equals = all(a%dim_specs == b%dim_specs) .and. &
           & (a%grid_type == b%grid_type)

   end function equals_grid


   impure elemental logical function not_equals_grid(a, b) result(not_equals)
      class (GridSpec), intent(in) :: a
      class (GridSpec), intent(in) :: b

      not_equals = .not. (a == b)

   end function not_equals_grid
   

   impure elemental logical function equals_dim(a, b) result(equals)
      class (DimensionSpec), intent(in) :: a
      class (DimensionSpec), intent(in) :: b

      equals = &
           & (a%num_points == b%num_points) .and. &
           & (a%topology == b%topology) .and. &
           & (a%x_min == b%x_min) .and. &
           & (a%x_max == b%x_max)

   end function equals_dim


   impure elemental logical function not_equals_dim(a, b) result(not_equals)
      class (DimensionSpec), intent(in) :: a
      class (DimensionSpec), intent(in) :: b

      not_equals = .not. (a == b)

   end function not_equals_dim

   
end module MAPL_GridSpecMod
