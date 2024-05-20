#include "MAPL_Generic.h"
module mapl3g_pFIOServerBounds
   use mapl_ErrorHandlingMod
   use esmf
   use pfio
   use gFTL2_StringVector
   use MAPL_BaseMod

   implicit none
   private

   public :: pFIOServerBounds

   integer, parameter :: grid_dims = 2

   type :: pFIOServerBounds
      private
      integer, allocatable :: local_start(:)
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
      integer, allocatable :: file_shape(:)
   contains
      procedure :: create_server_bounds
      procedure :: get_local_start
      procedure :: get_global_start
      procedure :: get_global_count
      procedure :: get_file_shape
   end type pFIOServerBounds

   contains

   function get_local_start(this) result(local_start)
      integer, allocatable :: local_start(:)
      class(pFIOServerBounds), intent(in) :: this
      local_start =this%local_start
   end function get_local_start

   function get_global_start(this) result(global_start)
      integer, allocatable :: global_start(:)
      class(pFIOServerBounds), intent(in) :: this
      global_start =this%global_start
   end function get_global_start

   function get_global_count(this) result(global_count)
      integer, allocatable :: global_count(:)
      class(pFIOServerBounds), intent(in) :: this
      global_count =this%global_count
   end function get_global_count

   function get_file_shape(this) result(file_shape)
      integer, allocatable :: file_shape(:)
      class(pFIOServerBounds), intent(in) :: this
      file_shape =this%file_shape
   end function get_file_shape

   subroutine create_server_bounds(this, grid, field_shape, time_index, rc)
      class(pFIOServerBounds), intent(inout) :: this
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      integer, intent(in), optional :: time_index
      integer, intent(out), optional :: rc

      integer :: status, tile_count, n_dims, ungrid_dims, tm, global_dim(3)
      integer :: i1, in, j1, jn, tile

      call ESMF_GridGet(grid, tileCount=tile_count, _RC)
      call MAPL_GridGetInterior(grid, i1,in, j1, jn)
      call MAPL_GridGet(grid, globalCellCountPerDim=global_dim, _RC)
      n_dims = size(field_shape)
      ungrid_dims = n_dims - grid_dims
      tm = 0
      if (present(time_index))  tm = 1

      if (tile_count == 6) then
         tile = 1 + (j1-1)/global_dim(1)
         allocate(this%file_shape(n_dims+1))
         allocate(this%global_start(n_dims+1+tm))
         allocate(this%global_count(n_dims+1+tm))
         allocate(this%local_start(n_dims+1+tm))

         this%file_shape(1:grid_dims+1) = [field_shape(1), field_shape(2) ,1]
         this%file_shape(grid_dims+2:grid_dims+ungrid_dims+1) = [field_shape(grid_dims+ungrid_dims:n_dims)]

         this%global_start(1:n_dims+1) = 1
         if(present(time_index)) this%global_start(n_dims+2) = time_index

         this%global_count(1:grid_dims+1) =[global_dim(1), global_dim(1), tile_count]
         this%global_count(grid_dims+2:grid_dims+ungrid_dims+1) = field_shape(grid_dims+1:n_dims)
         if (present(time_index)) this%global_count(n_dims+2) = 1 

         this%local_start = 1
         this%local_start(1:grid_dims+1) = [i1, j1-(tile-1)*global_dim(1), tile]


      else if (tile_count == 1) then
         allocate(this%global_start(n_dims+tm))
         allocate(this%global_count(n_dims+tm))
         allocate(this%local_start(n_dims+tm))
         
         this%file_shape = field_shape

         this%global_start(1:n_dims) = 1
         if (present(time_index)) this%global_start(n_dims+1) = time_index

         this%global_count(1:grid_dims) = [global_dim(1), global_dim(2)]
         this%global_count(grid_dims+1:grid_dims+ungrid_dims) = field_shape(grid_dims:n_dims)
         if (present(time_index)) this%global_count(n_dims+1) = 1

         this%local_start = 1
         this%local_start(1:grid_dims) = [i1,j1]

      else
         _FAIL("unsupported grid")
      end if
      _RETURN(_SUCCESS)

   end subroutine create_server_bounds
      
end module mapl3g_pFIOServerBounds

