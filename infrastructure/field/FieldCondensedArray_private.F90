#include "MAPL.h"
module mapl_FieldCondensedArray_private_mod

   use mapl_ErrorHandling_mod
   implicit none

   private
   public :: get_fptr_shape_private, ARRAY_RANK
   public :: get_fptr_shape_slice3d_private, SLICE3D_ARRAY_RANK

   integer, parameter :: ARRAY_RANK = 3
   ! Rank of the condensed array whose leading three dimensions form a 3D
   ! slice, i.e. (slice_dim1, slice_dim2, slice_dim3, ungridded).
   integer, parameter :: SLICE3D_ARRAY_RANK = 4

contains

   function get_fptr_shape_private(gridToFieldMap, localElementCount, has_vertical, rc) &
         &result(fptr_shape)
      integer :: fptr_shape(ARRAY_RANK)
      integer, intent(in) :: gridToFieldMap(:)
      integer, intent(in) :: localElementCount(:)
      logical, intent(in) :: has_vertical
      integer, optional, intent(out) :: rc
      integer :: rank, i
      integer, allocatable :: grid_dims(:)
      integer, allocatable :: ungridded_dims(:)
      integer :: horz_size, vert_size, ungridded_size
      integer :: vert_dim
      
      vert_dim = 0
      vert_size = 1

      rank = size(localElementCount)
      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      _ASSERT(all(grid_dims <= size(grid_dims)), 'MAPL expects geom dims before ungridded.')
      if(has_vertical) vert_dim = 1 
      if(size(grid_dims) > 0) vert_dim = maxval(grid_dims) + vert_dim
      ungridded_dims = pack([(i,i=1,rank)], [(all([vert_dim, grid_dims] /= i), i=1, rank)])
      horz_size = product([(localElementCount(grid_dims(i)), i=1, size(grid_dims))])
      if(has_vertical) vert_size = localElementCount(vert_dim)
      ungridded_size = product([(localElementCount(ungridded_dims(i)), i=1, size(ungridded_dims))])
      fptr_shape = [horz_size, vert_size, ungridded_size]
      _RETURN(_SUCCESS)

   end function get_fptr_shape_private

   ! Compute the shape of a rank-4 condensed array whose leading three
   ! dimensions form a 3D slice and whose trailing dimension enumerates the
   ! (collapsed) ungridded dimensions:
   !
   !    (slice_dim1, slice_dim2, slice_dim3, ungridded)
   !
   ! Unlike get_fptr_shape_private, the spatial (grid + vertical)
   ! dimensions are NOT collapsed into a single horizontal extent; they are
   ! preserved individually so that a service provider can operate on a
   ! genuine 3D block (e.g. a 4D field whose fourth dimension is ungridded).
   ! This requires exactly three non-ungridded dimensions (for example a 2D
   ! horizontal grid plus a vertical dimension, or a 3D grid).
   function get_fptr_shape_slice3d_private(gridToFieldMap, localElementCount, has_vertical, rc) &
         &result(fptr_shape)
      integer :: fptr_shape(SLICE3D_ARRAY_RANK)
      integer, intent(in) :: gridToFieldMap(:)
      integer, intent(in) :: localElementCount(:)
      logical, intent(in) :: has_vertical
      integer, optional, intent(out) :: rc
      integer :: rank, i
      integer, allocatable :: grid_dims(:)
      integer, allocatable :: ungridded_dims(:)
      integer :: vert_dim, n_spatial, ungridded_size

      vert_dim = 0

      rank = size(localElementCount)
      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      ! Grid dims must map to the leading field dimensions (before any
      ! ungridded dims); this is the same ordering check used by
      ! get_fptr_shape_private.  Combined with the n_spatial == 3 check
      ! below, it guarantees that field dimensions 1..3 are the (grid +
      ! vertical) spatial dimensions, so they can be read directly from
      ! localElementCount(1:3) regardless of the grid-dim ordering.
      _ASSERT(all(grid_dims <= size(grid_dims)), 'MAPL expects geom dims before ungridded.')
      if (has_vertical) vert_dim = 1
      if (size(grid_dims) > 0) vert_dim = maxval(grid_dims) + vert_dim
      n_spatial = size(grid_dims)
      if (has_vertical) n_spatial = n_spatial + 1
      _ASSERT(n_spatial == 3, 'A 3D slice requires exactly three non-ungridded (grid + vertical) dimensions.')
      _ASSERT(rank >= 3, 'field rank is too small for a 3D slice.')
      ungridded_dims = pack([(i,i=1,rank)], [(all([vert_dim, grid_dims] /= i), i=1, rank)])
      ! The three spatial dims occupy the leading field dimensions (grid dims
      ! followed by the vertical dim); the ungridded dims are collapsed.
      fptr_shape(1:3) = localElementCount(1:3)
      ungridded_size = product([(localElementCount(ungridded_dims(i)), i=1, size(ungridded_dims))])
      fptr_shape(SLICE3D_ARRAY_RANK) = ungridded_size
      _RETURN(_SUCCESS)

   end function get_fptr_shape_slice3d_private

end module mapl_FieldCondensedArray_private_mod
