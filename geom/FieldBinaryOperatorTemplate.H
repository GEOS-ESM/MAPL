#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_a,field_b,undef,rc)
      type(ESMF_Field), intent(inout) :: field_a
      type(ESMF_Field), intent(inout) :: field_b
      real, optional, intent(in) :: undef
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_a,tk_b

      real(kind=ESMF_KIND_R4), pointer :: ptr1_r4(:), ptr2_r4(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr1_r8(:),  ptr2_r8(:)

      call ESMF_FieldGet(field_a,typekind=tk_a,_RC)
      call ESMF_FieldGet(field_b,typekind=tk_b,_RC)

      _ASSERT(tk_a==tk_b, "For now we will only allow operations on same type fields")
      if (tk_a ==ESMF_TypeKind_R4) then
         call assign_fptr(field_a,ptr1_r4,_RC)
         call assign_fptr(field_b,ptr2_r4,_RC)
         if (present(undef)) then
            where( (ptr1_r4 /= undef) .and. (ptr2_r4 /= undef) )
               ptr1_r4 = ptr1_r4  _OP ptr2_r4
            else where
               ptr1_r4 = undef
            end where 
         else
            ptr1_r4 = ptr1_r4  _OP ptr2_r4
         end if
      else if (tk_A == ESMF_TypeKind_R8) then
         call assign_fptr(field_a,ptr1_r8,_RC)
         call assign_fptr(field_b,ptr2_r8,_RC)
         if (present(undef)) then
            where( (ptr1_r8 /= undef) .and. (ptr2_r8 /= undef) )
               ptr1_r8 = ptr1_r8  _OP ptr2_r8
            else where
               ptr1_r8 = undef
            end where 
         else
            ptr1_r8 = ptr1_r8  _OP ptr2_r8
         end if
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)
   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
