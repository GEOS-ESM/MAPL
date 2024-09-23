#include "MAPL_Generic.h"
module mapl3g_FieldCondensedArray_private

   use MAPL_ExceptionHandling
   implicit none

   private
   public :: get_fptr_shape

contains

   function get_fptr_shape(gridToFieldMap, localElementCount, has_vertical, rc) &
         &result(fptr_shape)
      integer :: fptr_shape(3)
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
      _HERE, 'gridToFieldMap: ', gridToFieldMap
      _HERE, 'localElementCount: ', localElementCount
      _HERE, 'has_vertical: ', has_vertical
      rank = size(localElementCount)
      _HERE, 'rank: ', rank
      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      _HERE, 'grid_dims: ', grid_dims
      _HERE, 'size(grid_dims): ', size(grid_dims)
      _HERE, 'grid_dims <= size(grid_dims): ', (grid_dims <= size(grid_dims))
      _HERE, 'all(grid_dims <= size(grid_dims)): ', all(grid_dims <= size(grid_dims))
      _ASSERT(all(grid_dims <= size(grid_dims)), 'MAPL expects geom dims before ungridded.')
      _HERE
      if(has_vertical) vert_dim = 1 
      if(size(grid_dims) > 0) vert_dim = maxval(grid_dims) + vert_dim
      ungridded_dims = pack([(i,i=1,rank)], [(all([vert_dim, grid_dims] /= i), i=1, rank)])
      _HERE, 'ungridded_dims: ', ungridded_dims
      horz_size = product([(localElementCount(grid_dims(i)), i=1, size(grid_dims))])
      _HERE, 'horz_size: ', horz_size
      if(has_vertical) vert_size = localElementCount(vert_dim)
!      vert_size = product([(localElementCount(vert_dims(i)), i=1, size(vert_dims))])
      _HERE, 'vert_size: ', vert_size
      ungridded_size = product([(localElementCount(ungridded_dims(i)), i=1, size(ungridded_dims))])
      _HERE, 'ungridded_size: ', ungridded_size
      fptr_shape = [horz_size, vert_size, ungridded_size]
      _HERE, 'fptr_shape: ', fptr_shape
      _RETURN(_SUCCESS)

   end function get_fptr_shape

end module mapl3g_FieldCondensedArray_private
