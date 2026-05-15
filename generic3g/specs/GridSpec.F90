module mapl3g_GridSpec
   implicit none
   private

   public :: GridSpec

   public :: GRID_ORIGIN_FROM_PARENT
   public :: GRID_ORIGIN_MIRROR
   public :: GRID_ORIGIN_CUSTOM

   public :: GRID_CLASS_GRID
   public :: GRID_CLASS_LOCSTREAM
   
   enum, bind(c)
      enumerator :: GRID_ORIGIN_FROM_PARENT
      enumerator :: GRID_ORIGIN_MIRROR
      enumerator :: GRID_ORIGIN_CUSTOM
   end enum

   enum, bind(c)
      enumerator :: GRID_CLASS_GRID
      enumerator :: GRID_CLASS_LOCSTREAM
   end enum

   type :: GridSpec
      integer :: origin
!!$      integer :: class
!!$      character(len=:), allocatable :: label ! for custom grid
   end type GridSpec

contains

   function GridSpec_simple(origin) result(grid_spec)
      type(GridSpec) :: grid_spec
      integer, intent(in) :: origin

      grid_spec%origin = origin
   end function GridSpec_simple

   
end module mapl3g_GridSpec
