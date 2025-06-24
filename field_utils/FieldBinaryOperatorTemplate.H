#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_out,field_a,field_b,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: field_a
      type(ESMF_Field), intent(inout) :: field_b
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_a,tk_b,tk_out
      logical :: has_undef,conformable
      type(ESMF_Field) :: fields(3)
      real(kind = ESMF_Kind_R4), allocatable :: undef_r4(:)
      real(kind = ESMF_Kind_R8), allocatable :: undef_r8(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr1_r4(:), ptr2_r4(:), ptr_out_r4(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr1_r8(:), ptr2_r8(:), ptr_out_r8(:)

      fields(1)=field_a
      fields(2)=field_b
      fields(3)=field_out
      conformable = FieldsAreConformable(field_a,field_out,__RC)
      __ASSERT(conformable,"Fields passed binary operator are not conformable")
      conformable = FieldsAreConformable(field_b,field_out,__RC)
      __ASSERT(conformable,"Fields passed binary operator are not conformable")

      has_undef = FieldsHaveUndef(fields,__RC)
      call ESMF_FieldGet(field_a,typekind=tk_a,__RC)
      call ESMF_FieldGet(field_b,typekind=tk_b,__RC)
      call ESMF_FieldGet(field_out,typekind=tk_out,__RC)

      __ASSERT(tk_a==tk_out, "For now we will only allow operations on same type fields")
      __ASSERT(tk_b==tk_out, "For now we will only allow operations on same type fields")

      if (tk_a ==ESMF_TypeKind_R4) then
         call assign_fptr(field_a,ptr1_r4,__RC)
         call assign_fptr(field_b,ptr2_r4,__RC)
         call assign_fptr(field_out,ptr_out_r4,__RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r4,__RC)
            where( (ptr1_r4 /= undef_r4(1)) .and. (ptr2_r4 /= undef_r4(2)) )
               ptr_out_r4 = ptr1_r4  _OP ptr2_r4
            elsewhere
               ptr_out_r4 = undef_r4(3)
            end where 
         else
            ptr_out_r4 = ptr1_r4  _OP ptr2_r4
         end if
      else if (tk_A == ESMF_TypeKind_R8) then
         call assign_fptr(field_a,ptr1_r8,__RC)
         call assign_fptr(field_b,ptr2_r8,__RC)
         call assign_fptr(field_out,ptr_out_r8,__RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r8,__RC)
            where( (ptr1_r8 /= undef_r8(1)) .and. (ptr2_r8 /= undef_r8(2)) )
               ptr_out_r8 = ptr1_r8  _OP ptr2_r8
            else where
               ptr_out_r8 = undef_r8(3)
            endwhere 
         else
            ptr_out_r8 = ptr1_r8  _OP ptr2_r8
         end if
      else
          __FAIL("unsupported type")
      end if
      __RETURN(__SUCCESS)
   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
