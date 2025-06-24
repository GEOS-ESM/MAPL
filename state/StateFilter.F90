#include "MAPL_ErrLog.h"
module MAPL_StateFilter
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_FieldUtils
   use MAPL_StateArithmeticParserMod
   use MAPL_StateMaskMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public StateFilterItem
   character(len=1), parameter :: var_placeholder = "@"
   character(len=1), parameter :: separator = "."

   interface StateFilterItem
      procedure StateFilter_R4_2D
      procedure StateFilter_R4_3D
   end interface

   contains

   subroutine StateFilter_R4_2D(state, config, itemName, array, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: itemName
      real(REAL32), allocatable, intent(out) :: array(:,:)
      integer, optional, intent(out) :: rc

      integer :: status, rank
      character(len=ESMF_MAXSTR) :: filter_expression, field_name
      character(len=:), allocatable :: processed_expression
      type(ESMF_Field) :: new_field, old_field
      logical :: name_Present, default_Present
      real(REAL32), pointer :: ptr2d_new(:,:), ptr2d_old(:,:)
      type(ESMF_TYPEKIND_FLAG) :: tk
      type(StateMask) :: mask

      call ESMF_StateGet(state, itemName, old_field, __RC)
      call ESMF_FieldGet(old_field, typeKind=tk, rank=rank, __RC) 
      __ASSERT(tk==ESMF_TYPEKIND_R4,"wrong typekind when call MAPL_StateFilter")
      __ASSERT(rank==2,"wrong rank when call MAPL_StateFilter")

      call ESMF_FieldGet(old_field, 0, farrayPtr=ptr2d_old, __RC)
      allocate(array( lbound(ptr2d_old,1):ubound(ptr2d_old,1) , lbound(ptr2d_old,2):ubound(ptr2d_old,2) ),  __STAT) 
      array = ptr2d_old
 
      call ESMF_ConfigFindLabel(config, "FILTER"//separator//trim(itemName)//":", isPresent=name_Present, __RC)
      if (name_Present) then
         call ESMF_ConfigGetAttribute(config, filter_expression, label="FILTER"//separator//trim(itemName)//":", __RC)
      else
         call ESMF_ConfigFindLabel(config, "FILTER"//separator//var_placeholder//":", isPresent=default_Present, __RC)
         __RETURN_UNLESS(default_present)
         call ESMF_ConfigGetAttribute(config, filter_expression, label="FILTER"//separator//var_placeholder//":", __RC)
      end if

      call FieldClone(old_field, new_field, __RC)
      call ESMF_FieldGet(old_field, name=field_name, __RC)
      call ESMF_FieldGet(new_field, 0, farrayPtr=ptr2d_new, __RC)
      ptr2d_new = ptr2d_old

      processed_expression = substitute_name(filter_expression, field_name) 
      if (index(processed_expression,"mask") > 0) then
         mask = StateMask(processed_expression)
         call mask%evaluate_mask(state, new_field, __RC)
      else
         call MAPL_StateEval(state, processed_expression, new_field, __RC)
      end if
      array = ptr2d_new

      call ESMF_FieldDestroy(new_field, noGarbage=.true., __RC)
      __RETURN(__SUCCESS)

   end subroutine StateFilter_R4_2D

   subroutine StateFilter_R4_3D(state, config, itemName, array, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: itemName
      real(REAL32), allocatable, intent(out) :: array(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status, rank
      character(len=ESMF_MAXSTR) :: filter_expression, field_name
      character(len=:), allocatable :: processed_expression
      type(ESMF_Field) :: new_field, old_field
      logical :: name_Present, default_Present
      real(REAL32), pointer :: ptr3d_new(:,:,:), ptr3d_old(:,:,:)
      type(ESMF_TYPEKIND_FLAG) :: tk
      type(StateMask) :: mask

      call ESMF_StateGet(state, itemName, old_field, __RC)
      call ESMF_FieldGet(old_field, typeKind=tk, rank=rank, __RC) 
      __ASSERT(tk==ESMF_TYPEKIND_R4,"wrong typekind when call MAPL_StateFilter")
      __ASSERT(rank==3,"wrong rank when call MAPL_StateFilter")

      call ESMF_FieldGet(old_field, 0, farrayPtr=ptr3d_old, __RC)
      allocate(array( lbound(ptr3d_old,1):ubound(ptr3d_old,1) , lbound(ptr3d_old,2):ubound(ptr3d_old,2), lbound(ptr3d_old,3):ubound(ptr3d_old,3) ),  __STAT) 
      array = ptr3d_old
 
      call ESMF_ConfigFindLabel(config, "FILTER"//separator//trim(itemName)//":", isPresent=name_Present, __RC)
      if (name_Present) then
         call ESMF_ConfigGetAttribute(config, filter_expression, label="FILTER"//separator//trim(itemName)//":", __RC)
      else
         call ESMF_ConfigFindLabel(config, "FILTER"//separator//var_placeholder//":", isPresent=default_Present, __RC)
         __RETURN_UNLESS(default_present)
         call ESMF_ConfigGetAttribute(config, filter_expression, label="FILTER"//separator//var_placeholder//":", __RC)
      end if

      call FieldClone(old_field, new_field, __RC)
      call ESMF_FieldGet(old_field, name=field_name, __RC)
      call ESMF_FieldGet(new_field, 0, farrayPtr=ptr3d_new, __RC)
      ptr3d_new = ptr3d_old

      processed_expression = substitute_name(filter_expression, field_name) 
      if (index(processed_expression,"mask") > 0) then
         mask = StateMask(processed_expression)
         call mask%evaluate_mask(state, new_field, __RC)
      else
         call MAPL_StateEval(state, processed_expression, new_field, __RC)
      end if
      array = ptr3d_new

      call ESMF_FieldDestroy(new_field, noGarbage=.true., __RC)
      __RETURN(__SUCCESS)

   end subroutine StateFilter_R4_3d


   function substitute_name(filter_expression, field_name, rc) result(processed_expression)
      character(len=:), allocatable :: processed_expression
      character(len=*), intent(in) :: filter_expression
      character(len=*), intent(in) :: field_name
      integer, optional, intent(out) :: rc

      integer :: placeholder_loc
      character(len=:), allocatable :: temp_before, temp_after
      placeholder_loc = index(filter_expression, var_placeholder)
      __ASSERT(placeholder_loc > 0, "expression for filter does not have a @ in it")
      temp_before = filter_expression(1:placeholder_loc-1)
      temp_after = filter_expression(placeholder_loc+1:)
      processed_expression = temp_before//trim(field_name)//temp_after
      __RETURN(__SUCCESS)
   end function 
      
end module MAPL_StateFilter
