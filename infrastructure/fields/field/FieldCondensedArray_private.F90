#include "MAPL.h"
module mapl3g_FieldCondensedArray_private

   use MAPL_ExceptionHandling
   implicit none

   private
   public :: get_fptr_shape_private, ARRAY_RANK

   integer, parameter :: ARRAY_RANK = 3

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

end module mapl3g_FieldCondensedArray_private
