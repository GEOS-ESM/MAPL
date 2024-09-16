module mapl3g_FieldCondensedArray

   implicit none

!   public :: ! public procedures, variables, types, etc.
   private


contains

   function get_array_shape(field_in)
      integer :: array_shape(3)
      type(ESMF_Field), intent(in) :: field_in
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: grid_dims(:)
      integer, allocatable :: vert_dims(:)
      integer, allocatable :: all_dims(:)
      integer, allocatable :: ungridded_dims(:)
      integer :: horz_size, vert_size, ungridded_size

      call ESMF_FieldGet(field_in, gridToFieldMap=gridToFieldMap, _RC) 
      grid_dims = pack(gridToFieldMap, gridToFieldMap /= 0)
      _ASSERT(all(grid_dims <= size(grid_dims)), 'MAPL expects geom dims before ungridded')

      vert_dims = [integer:: ] ! empty
      if (<<field info has vertical>>) then
         vert_dims = [<<get vert_dim from info>>]
      end if

      all_dims = [(i,i=1,rank)]
      ungridded_dims = pack(all_dims, [(all([vert_dims,grid_dims] /= i),i=1,rank)])

      horz_size = product([localElementCount(grid_dims(i)), i=1, size(grid_dims) ] )
      vert_size = product([localElementCount(vert_dims(i)), i=1, size(vert_dims)])
      ungridded_size = product([localElementCount(vert_dims(i)), i=1, size(ungridded_dims)])

      array_shape = [horz_size, vert_size, ungridded_size]

   end function get_array_shape

end module mapl3g_FieldCondensedArray

