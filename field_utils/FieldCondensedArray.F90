#include "MAPL_Generic.h"
module mapl3g_FieldCondensedArray

   use mapl3g_output_info, only: get_num_levels
   use mapl3g_FieldCondensedArray_private, only: get_array_shape_private => get_array_shape
   use MAPL_ExceptionHandling
   use esmf, only: ESMF_Field, ESMF_FieldGet
   implicit none

   private

   public :: get_array_shape

contains

   function get_array_shape(field_in, rc) result(array_shape)
      integer :: array_shape(3)
      type(ESMF_Field), intent(in) :: field_in
      integer, optional, intent(out) :: rc
      integer :: status
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: localElementCount(:)
      integer, allocatable :: vertical_dimensions(:)
      integer :: num_levels
      integer :: rank

      num_levels = 0
      vertical_dimensions = [integer::]
      call ESMF_FieldGet(field_in, gridToFieldMap=gridToFieldMap, _RC) 
      call ESMF_FieldGet(field_in, rank=rank, _RC)
      allocate(localElementCount(rank))
!     Due to an ESMF bug, getting the localElementCount should use the module function.
!     For now, use this because of dependency issues.
      call ESMF_FieldGet(field_in, localElementCount=localElementCount, _RC)
!     See FieldGetLocalElementCount (specific function) comments in FieldPointerUtilities.
      !localElementCount = FieldGetLocalElementCount(f, _RC)
      num_levels = get_num_levels(field_in, _RC)
      if(num_levels > 0) vertical_dimensions = [num_levels]
      array_shape = get_array_shape_private(gridToFieldMap, localElementCount, vertical_dimensions, _RC)

   end function get_array_shape

end module mapl3g_FieldCondensedArray
