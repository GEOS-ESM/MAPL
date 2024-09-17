#include "MAPL_Generic.h"
module mapl3g_FieldCondensedArray_private

   use MAPL_ExceptionHandling
   implicit none

   private
   public :: get_array_shape

contains

   function get_array_shape(gridToFieldMap, localElementCount, vert_dims, rc) &
         &result(array_shape)
      integer :: array_shape(3)
      integer, intent(in) :: gridToFieldMap(:)
      integer, intent(in) :: localElementCount(:)
      integer, optional, intent(in) :: vert_dims(:)
      integer, optional, intent(out) :: rc
      integer :: status, rank, i
      integer, allocatable :: grid_dims(:)
      integer, allocatable :: vert_dims_(:)
      integer, allocatable :: ungridded_dims(:)
      integer :: horz_size, vert_size, ungridded_size
      
      rank = size(localElementCount)
      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      _ASSERT(all(grid_dims <= size(grid_dims)), 'MAPL expects geom dims before ungridded')
      vert_dims_ = [integer::]
      if (present(vert_dims)) then
         if(size(vert_dims) > 0) vert_dims_ = vert_dims
      end if
      ungridded_dims = pack([(i,i=1,rank)], [(all([vert_dims_, grid_dims] /= i), i=1, rank)])
      horz_size = product([(localElementCount(grid_dims(i)), i=1, size(grid_dims))])
      vert_size = product([(localElementCount(vert_dims_(i)), i=1, size(vert_dims_))])
      ungridded_size = product([(localElementCount(ungridded_dims(i)), i=1, size(ungridded_dims))])
      array_shape = [horz_size, vert_size, ungridded_size]
      _RETURN(_SUCCESS)

   end function get_array_shape

end module mapl3g_FieldCondensedArray_private
