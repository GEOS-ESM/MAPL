#include "MAPL_Generic.h"
module mapl3g_FieldCondensedArray
   use mapl3g_FieldCondensedArray_private, only: get_fptr_private => get_fptr_shape
   use mapl3g_output_info, only: get_vertical_dim_spec_name
   use MAPL_FieldPointerUtilities, only: FieldGetLocalElementCount, FieldGetCptr
   use MAPL_ExceptionHandling
   use ESMF, only: ESMF_Field, ESMF_KIND_R4, ESMF_KIND_R8

   implicit none
   private
   public :: assign_fptr_rank3

   interface assign_fptr_rank3
      module procedure :: assign_fptr_r4_rank3
      module procedure :: assign_fptr_r8_rank3
   end interface assign_fptr_rank3

contains

   subroutine assign_fptr_r4_rank3(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:,:,:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:,:,:)
      integer :: status

      fp_shape = get_fptr_shape(x, _RC)
      call FieldGetCptr(x, cptr, _RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r4_rank3

   subroutine assign_fptr_r8_rank3(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:,:,:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:,:,:)
      integer :: status

      fp_shape = get_fptr_shape(x, _RC)
      call FieldGetCptr(x, cptr, _RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      _RETURN(_SUCCESS)
   end subroutine assign_fptr_r8_rank3

   function get_fptr_shape(f, rc) result(fptr_shape)
      integer :: fptr_shape(3)
      type(ESMF_Field), intent(inout) :: f
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: rank
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: localElementCount(:)
      logical :: has_vertical
      character(len=:), allocatable :: spec_name
      character(len=*), parameter :: VERTICAL_DIM_NONE_NAME = 'VERTICAL_DIM_NONE'
      !wdb fixme deleteme This seems fragile. We should probably make a utility function
      !that selects the type(VerticalDimSpec) parameter based on a string. Perhaps a
      !logical function in VerticalDimSpec.F90 that recogizes a VerticalDimSpec based on
      !the string from the ESMF_Info.

      call ESMF_FieldGet(f, gridToFieldMap=gridToFieldMap, _RC) 
      call ESMF_FieldGet(f, rank=rank, _RC)
      allocate(localElementCount(rank))
      !  Due to an ESMF bug, getting the localElementCount must use the module function.
      !  See FieldGetLocalElementCount (specific function) comments.
      localElementCount = FieldGetLocalElementCount(f, _RC)
      spec_name = get_vertical_dim_spec_name(f, _RC)
      has_vertical = spec_name /= VERTICAL_DIM_NONE_NAME
      fptr_shape = get_fptr_shape_private(gridToFieldMap, localElementCount, has_vertical, _RC)

   end function get_fptr_shape

end module mapl3g_FieldCondensedArray
