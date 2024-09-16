module mapl3g_FieldCondensedArray

   use mapl3g_FieldCondensedArray_private
   implicit none

!   public :: ! public procedures, variables, types, etc.
   private


contains

   function public_get_array_shape(field_in)
      integer :: array_shape(3)
      type(ESMF_Field), intent(in) :: field_in
      integer, allocatable :: gridToFieldMap(:)

      call ESMF_FieldGet(field_in, gridToFieldMap=gridToFieldMap, _RC) 
      array_shape = get_array_shape(gridToFieldMap)

   end function public_get_array_shape

end module mapl3g_FieldCondensedArray

