#include "MAPL.h"
module mapl3g_StateItemGetVerticalGrid
   use mapl3g_VerticalGrid
   use mapl3g_StateItemSpec
   use mapl3g_StateItemAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_FieldInfo, only: FieldInfoGetInternal
   use mapl3g_FieldBundleInfo, only: FieldBundleInfoGetInternal
   use mapl3g_AspectId
   use mapl3g_FieldInfo, only: FieldInfoGetInternal
   use mapl_ErrorHandling
   use esmf
   use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
   implicit none
   private

   public :: mapl_FieldGetVerticalGrid
!#   public :: mapl_FieldBundleGetVerticalGrid

   interface mapl_FieldGetVerticalGrid
      procedure :: field_get_vertical_grid
   end interface mapl_FieldGetVerticalGrid

!#   interface mapl_FieldBundleGetVerticalGrid
!#      procedure :: bundle_get_vertical_grid
!#   end interface mapl_FieldGetVerticalGrid


contains

   subroutine field_get_vertical_grid(field, vertical_grid, rc)
      type(esmf_Field), intent(inout) :: field
      class(VerticalGrid), allocatable, intent(out) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(c_ptr) :: spec_cptr
      type(StateItemSpec), pointer :: spec
      class(StateItemAspect), pointer :: aspect
      integer, allocatable :: spec_handle(:)
      type(esmf_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      call FieldBundleInfoGetInternal(info, spec_handle=spec_handle, _RC)

      spec_cptr = transfer(spec_handle, spec_cptr)
      call c_f_pointer(spec_cptr, spec)
     
      aspect => spec%get_aspect(VERTICAL_GRID_ASPECT_ID)
      if (.not. associated(aspect)) then
         _FAIL('null aspect pointer for VERTICAL_GRID_ASPECT_ID')
      end if
      select type(aspect)
      type is (VerticalGridAspect)
         vertical_grid = aspect%get_vertical_grid(_RC)
      class default
         _FAIL('Expected VerticalGridAspect but got different type')
      end select
   
      _RETURN(_SUCCESS)
   end subroutine field_get_vertical_grid
   
end module mapl3g_StateItemGetVerticalGrid
