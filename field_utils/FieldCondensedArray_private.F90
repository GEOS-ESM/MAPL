module mapl3g_FieldCondensedArray_private

   use esmf
   implicit none

!   public :: ! public procedures, variables, types, etc.
   private
   public :: get_array_shape

contains

   function get_array_shape(gridToFieldMap, vert_dims) 
      integer :: array_shape(3)
      integer, intent(in) :: gridToFieldMap(:)
      integer, optional, intent(in) :: vert_dims(:)
      integer, allocatable :: grid_dims(:)
      integer, allocatable :: vert_dims_(:)
      integer, allocatable :: all_dims(:)
      integer, allocatable :: ungridded_dims(:)
      integer :: horz_size, vert_size, ungridded_size

      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      _ASSERT(all(grid_dims <= size(grid_dims)), 'MAPL expects geom dims before ungridded')

      vert_dims_ = [integer:: ] ! empty
      if (present(vert_dims))
         if(size(vert_dims) > 0) vert_dims_ = vert_dims
      end if

      all_dims = [(i,i=1,rank)]
      ungridded_dims = pack(all_dims, [(all([vert_dims,grid_dims] /= i),i=1,rank)])

      horz_size = product([localElementCount(grid_dims(i)), i=1, size(grid_dims) ] )
      vert_size = product([localElementCount(vert_dims(i)), i=1, size(vert_dims)])
      ungridded_size = product([localElementCount(vert_dims(i)), i=1, size(ungridded_dims)])

      array_shape = [horz_size, vert_size, ungridded_size]

   end function get_array_shape

end module mapl3g_FieldCondensedArray
