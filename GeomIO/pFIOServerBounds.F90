#include "MAPL.h"

module mapl3g_pFIOServerBounds

   use mapl_ErrorHandlingMod
   use esmf
   use pfio
   use gFTL2_StringVector
   use MAPL_BaseMod, only: MAPL2_GridGet => MAPL_GridGet, MAPL2_GridGetInterior => MAPL_GridGetInterior

   implicit none
   private

   public :: pFIOServerBounds
   public :: PFIO_BOUNDS_READ
   public :: PFIO_BOUNDS_WRITE

   integer, parameter :: grid_dims = 2
   integer, parameter :: PFIO_BOUNDS_READ = 1
   integer, parameter :: PFIO_BOUNDS_WRITE = 2

   type :: pFIOServerBounds
      private
      integer, allocatable :: local_start(:)
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
      integer, allocatable :: corner_local_start(:)
      integer, allocatable :: corner_global_start(:)
      integer, allocatable :: corner_global_count(:)
      integer, allocatable :: file_shape(:)
   contains
      procedure :: get_local_start
      procedure :: get_global_start
      procedure :: get_global_count
      procedure :: get_corner_local_start
      procedure :: get_corner_global_start
      procedure :: get_corner_global_count
      procedure :: get_file_shape
   end type pFIOServerBounds

   interface pFIOServerBounds
      procedure new_pFIOServerBounds_grid
   end interface pFIOServerBounds

contains

   function get_local_start(this) result(local_start)
      integer, allocatable :: local_start(:)
      class(pFIOServerBounds), intent(in) :: this
      local_start = this%local_start
   end function get_local_start

   function get_global_start(this) result(global_start)
      integer, allocatable :: global_start(:)
      class(pFIOServerBounds), intent(in) :: this
      global_start = this%global_start
   end function get_global_start

   function get_global_count(this) result(global_count)
      integer, allocatable :: global_count(:)
      class(pFIOServerBounds), intent(in) :: this
      global_count = this%global_count
   end function get_global_count

   function get_corner_local_start(this) result(corner_local_start)
      integer, allocatable :: corner_local_start(:)
      class(pFIOServerBounds), intent(in) :: this
      corner_local_start = this%corner_local_start
   end function get_corner_local_start

   function get_corner_global_start(this) result(corner_global_start)
      integer, allocatable :: corner_global_start(:)
      class(pFIOServerBounds), intent(in) :: this
      corner_global_start = this%corner_global_start
   end function get_corner_global_start

   function get_corner_global_count(this) result(corner_global_count)
      integer, allocatable :: corner_global_count(:)
      class(pFIOServerBounds), intent(in) :: this
      corner_global_count = this%corner_global_count
   end function get_corner_global_count

   function get_file_shape(this) result(file_shape)
      integer, allocatable :: file_shape(:)
      class(pFIOServerBounds), intent(in) :: this
      file_shape = this%file_shape
   end function get_file_shape

   function new_pFIOServerBounds_grid(grid, field_shape, read_or_write, time_index, rc) result(server_bounds)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      integer, intent(in) :: read_or_write
      integer, intent(in), optional :: time_index
      integer, intent(out), optional :: rc
      type(pFIOServerBounds) :: server_bounds

      integer :: status
      logical :: vert_only

      vert_only = .false.
      if (size(field_shape) == 1) vert_only = .true.

      if (vert_only) then
         server_bounds = pFIOServerBounds_vert_only_field(field_shape(1), time_index, _RC)
      else
         server_bounds = pFIOServerBounds_gridded_field(grid, field_shape, read_or_write, time_index, _RC)
      end if

      _RETURN(_SUCCESS)
   end function new_pFIOServerBounds_grid

   function pFIOServerBounds_vert_only_field(num_field_levels, time_index, rc) result(server_bounds)
      integer, intent(in) :: num_field_levels
      integer, intent(in), optional :: time_index
      integer, intent(out), optional :: rc
      type(pFIOServerBounds) :: server_bounds ! result

      integer, parameter :: file_dims = 1
      integer :: tm

      tm = 0
      if (present(time_index)) tm = 1
      
      allocate(server_bounds%file_shape(1), source=num_field_levels)
      allocate(server_bounds%local_start(1+tm), source=1)
      allocate(server_bounds%global_start(1+tm), source=1)
      allocate(server_bounds%global_count(1+tm), source=1)
      server_bounds%global_count(1) = num_field_levels

      _RETURN(_SUCCESS)
   end function pFIOServerBounds_vert_only_field

   function pFIOServerBounds_gridded_field(grid, field_shape, read_or_write, time_index, rc) result(server_bounds)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      integer, intent(in) :: read_or_write
      integer, intent(in), optional :: time_index
      integer, intent(out), optional :: rc
      type(pFIOServerBounds) :: server_bounds ! field

      integer :: status, tile_count, n_dims, tm, global_dim(3)
      integer :: i1, in, j1, jn, tile, extra_file_dim, file_dims, new_grid_dims

      call ESMF_GridGet(grid, tileCount=tile_count, _RC)
      call MAPL2_GridGetInterior(grid, i1, in, j1, jn)
      call MAPL2_GridGet(grid, globalCellCountPerDim=global_dim, _RC)
      n_dims = size(field_shape)

      tm = 0
      if (present(time_index)) tm = 1

      extra_file_dim = 0
      if (tile_count == 6) extra_file_dim = 1

      new_grid_dims = grid_dims + extra_file_dim
      file_dims = n_dims + extra_file_dim

      allocate(server_bounds%file_shape(file_dims))
      allocate(server_bounds%global_start(file_dims+tm))
      allocate(server_bounds%global_count(file_dims+tm))
      allocate(server_bounds%local_start(file_dims+tm))
      allocate(server_bounds%corner_global_start(file_dims+tm))
      allocate(server_bounds%corner_global_count(file_dims+tm))
      allocate(server_bounds%corner_local_start(file_dims+tm))

      server_bounds%file_shape(new_grid_dims+1:file_dims) = field_shape(grid_dims+1:n_dims)

      server_bounds%global_start(1:file_dims) = 1
      server_bounds%corner_global_start(1:file_dims) = 1
      if(present(time_index)) server_bounds%global_start(file_dims+1) = time_index

      server_bounds%global_count(new_grid_dims+1:file_dims) = field_shape(grid_dims+1:n_dims)
      if (present(time_index)) server_bounds%global_count(file_dims+1) = 1

      server_bounds%local_start = 1
      if (read_or_write == PFIO_BOUNDS_READ) then
         if(present(time_index)) server_bounds%local_start(file_dims+1) = time_index
      end if

      select case (tile_count)
      case (6) ! Assume cubed-sphere

         tile = 1 + (j1-1)/global_dim(1)
         server_bounds%file_shape(1:new_grid_dims) = [field_shape(1), field_shape(2), 1]
         server_bounds%global_count(1:new_grid_dims) =[global_dim(1), global_dim(1), tile_count]
         server_bounds%local_start(1:new_grid_dims) = [i1, j1-(tile-1)*global_dim(1), tile]

         
         server_bounds%corner_global_count(1:new_grid_dims) =[global_dim(1)+1, global_dim(1)+1, tile_count]
         server_bounds%corner_local_start(1:new_grid_dims) = [i1, j1-(tile-1)*global_dim(1), tile]

      case (1)

         server_bounds%file_shape(1:new_grid_dims) = [field_shape(1), field_shape(2)]
         server_bounds%global_count(1:new_grid_dims) = [global_dim(1), global_dim(2)]
         server_bounds%local_start(1:new_grid_dims) = [i1,j1]

      case default

         _FAIL("unsupported grid")

      end select

      _RETURN(_SUCCESS)
   end function pFIOServerBounds_gridded_field

end module mapl3g_pFIOServerBounds
