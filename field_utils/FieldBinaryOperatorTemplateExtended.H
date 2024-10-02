#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_out,field_left,field_right,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: field_left
      type(ESMF_Field), intent(inout) :: field_right
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_left,tk_right,tk_out
      logical :: has_undef,conformable
      type(ESMF_Field) :: fields(3)
      LEFT_TYPE_(kind=LEFT_KIND_) :: left_undef
      RIGHT_TYPE_(kind=RIGHT_KIND_) :: right_undef
      OUT_TYPE_(kind=OUT_KIND_) :: out_undef

      fields(1)=field_left
      fields(2)=field_right
      fields(3)=field_out
      conformable = FieldsAreConformable(field_left,field_out,_RC)
      _ASSERT(conformable,"Fields passed binary operator are not conformable")
      conformable = FieldsAreConformable(field_right,field_out,_RC)
      _ASSERT(conformable,"Fields passed binary operator are not conformable")

      has_undef = FieldsHaveUndef(fields,_RC)
      call ESMF_FieldGet(field_left,typekind=tk_left,_RC)
      call ESMF_FieldGet(field_right,typekind=tk_right,_RC)
      call ESMF_FieldGet(field_out,typekind=tk_out,_RC)

      select case(intTK(tk_left))
      case(intTK(ESMF_TypeKind_R4))
         call assign_fptr(field_left, ptr1_r4, _RC)
      case(intTK(ESMF_TypeKind_R8))
         call assign_fptr(field_left, ptr1_r8, _RC)
      end select
      if (tk_left ==ESMF_TypeKind_R4) then
         call assign_fptr(field_left,ptr1_r4,_RC)
         call assign_fptr(field_right,ptr2_r4,_RC)
         call assign_fptr(field_out,ptr_out_r4,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r4,_RC)
            where( (ptr1_r4 /= undef_r4(1)) .and. (ptr2_r4 /= undef_r4(2)) )
               ptr_out_r4 = ptr1_r4  _OP ptr2_r4
            elsewhere
               ptr_out_r4 = undef_r4(3)
            end where 
         else
            ptr_out_r4 = ptr1_r4  _OP ptr2_r4
         end if
      else if (tk_A == ESMF_TypeKind_R8) then
         call assign_fptr(field_left,ptr1_r8,_RC)
         call assign_fptr(field_right,ptr2_r8,_RC)
         call assign_fptr(field_out,ptr_out_r8,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r8,_RC)
            where( (ptr1_r8 /= undef_r8(1)) .and. (ptr2_r8 /= undef_r8(2)) )
               ptr_out_r8 = ptr1_r8  _OP ptr2_r8
            else where
               ptr_out_r8 = undef_r8(3)
            endwhere 
         else
            ptr_out_r8 = ptr1_r8  _OP ptr2_r8
         end if
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

   contains
      function intTK(tk) result(n)
         integer :: n
         type(ESMF_TypeKind_Flag), parameter :: type_kinds = [ESMF_TypeKind_I4, ESMF_TypeKind_R4, ESMF_TypeKind_I8, ESMF_TypeKind_R8]

         do n=0, size(tks)-1
            if(tk == tks(n+1)) return
         end do
         n = -1

      end function intTK
   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
