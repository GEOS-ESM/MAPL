#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_out,left_field,right_field,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: left_field
      type(ESMF_Field), intent(inout) :: right_field
      integer, optional, intent(out) :: rc

      integer :: status
      integer, parameter :: LEFT = 1
      integer, parameter :: RIGHT = 2
      integer, parameter :: OUT_ = 3
      type(ESMF_Field) :: fields(3)
      type(ESMF_TypeKind_Flag) :: tk_in
      type(ESMF_TypeKind_Flag) :: tk_out
      logical :: has_undef, conformable

      real(kind = ESMF_Kind_R4), allocatable :: undef_r4(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr_r4_left(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: ptr_r4_right(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: ptr_r4_out(:) => null()

      real(kind = ESMF_Kind_R8), allocatable :: undef_r8(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8_left(:) => null()
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8_right(:) => null()
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8_out(:) => null()
      
      conformable = FieldsAreConformable(left_field,field_out,_RC)
      _ASSERT(conformable,"Left field passed to binary function is not conformable with result field.")
      conformable = FieldsAreConformable(right_field,field_out,_RC)
      _ASSERT(conformable,"Right field passed to binary function is not conformable with result field.")

      fields(LEFT) = left_field
      fields(RIGHT) = right_field
      fields(OUT_) = field_out
      has_undef = FieldsHaveUndef(fields,_RC)
      call ESMF_FieldGet(field_out,typekind=tk_out,_RC)
      call ESMF_FieldGet(left_field,typekind=tk_in,_RC)
      _ASSERT(tk_in == tk_out, "Left field type does not match result field type.")
      call ESMF_FieldGet(right_field,typekind=tk_in,_RC)
      _ASSERT(tk_in == tk_out, "Right field type does not match result field type.")
      if (tk_out ==ESMF_TypeKind_R4) then
         call assign_fptr(left_field,ptr_r4_left,_RC)
         call assign_fptr(right_field,ptr_r4_right,_RC)
         call assign_fptr(field_out,ptr_r4_out,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r4,_RC)
            where(ptr_r4_left /= undef_r4(LEFT) .or. ptr_r4_right /= undef_r4(RIGHT))
               ptr_r4_out = _FUNC(ptr_r4_left, ptr_r4_right)
            elsewhere
               ptr_r4_out = undef_r4(OUT_)
            end where
         else
            ptr_r4_out = _FUNC(ptr_r4_left, ptr_r4_right)
         end if
      else if (tk_out ==ESMF_TypeKind_R8) then
         call assign_fptr(left_field,ptr_r8_left,_RC)
         call assign_fptr(right_field,ptr_r8_right,_RC)
         call assign_fptr(field_out,ptr_r8_out,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r8,_RC)
            where(ptr_r8_left /= undef_r8(LEFT) .or. ptr_r8_right /= undef_r8(RIGHT))
               ptr_r8_out = _FUNC(ptr_r8_left, ptr_r8_right)
            elsewhere
               ptr_r8_out = undef_r8(OUT_)
            end where
         else
            ptr_r8_out = _FUNC(ptr_r8_left, ptr_r8_right)
         end if
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)
   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
