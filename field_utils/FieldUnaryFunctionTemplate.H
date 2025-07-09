#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_out,field_in,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: field_in
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: fields(2)
      type(ESMF_TypeKind_Flag) :: tk_in, tk_out
      real(kind = ESMF_Kind_R4), allocatable :: undef_r4(:)
      real(kind = ESMF_Kind_R8), allocatable :: undef_r8(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr_r4_in(:),ptr_r4_out(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8_in(:),ptr_r8_out(:)
      logical :: has_undef, conformable
      
      conformable = FieldsAreConformable(field_in,field_out,_rc) 
      _assert(conformable,"Fields passed unary function are not conformable")

      fields(1) = field_in
      fields(2) = field_out
      has_undef = FieldsHaveUndef(fields,_rc)
      call ESMF_FieldGet(field_in,typekind=tk_in,_rc)
      call ESMF_FieldGet(field_out,typekind=tk_out,_rc)
      _assert(tk_in == tk_out, "For now input and output field must be of same type for a field function")
      if (tk_in ==ESMF_TypeKind_R4) then
         call assign_fptr(field_in,ptr_r4_in,_rc)
         call assign_fptr(field_out,ptr_r4_out,_rc)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r4,_rc)
            where(ptr_r4_in /= undef_r4(1))
               ptr_r4_out = _FUNC(ptr_r4_in)
            elsewhere
               ptr_r4_out = undef_r4(2)
            end where
         else
            ptr_r4_out = _FUNC(ptr_r4_in)
         end if
      else if (tk_in == ESMF_TypeKind_R8) then
         call assign_fptr(field_in,ptr_r8_in,_rc)
         call assign_fptr(field_out,ptr_r8_out,_rc)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r8,_rc)
            where(ptr_r8_in /= undef_r8(1))
               ptr_r8_out = _FUNC(ptr_r8_in)
            elsewhere
               ptr_r4_out = undef_r8(2)
            end where
         else
            ptr_r8_out = _FUNC(ptr_r8_in)
         end if
      else
          _fail("unsupported type")
      end if
      _return(_success)
   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
