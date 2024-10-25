! THIS WILL PROBABLY GO AWAY

#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_out,left_field,right_field,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: left_field
      type(ESMF_Field), intent(inout) :: right_field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_in
      type(ESMF_TypeKind_Flag) :: tk_out
      logical :: conformable

      conformable = FieldsAreConformable(left_field,field_out,_RC)
      _ASSERT(conformable,"Left field passed to binary function is not conformable with result field.")
      conformable = FieldsAreConformable(right_field,field_out,_RC)
      _ASSERT(conformable,"Right field passed to binary function is not conformable with result field.")

      call ESMF_FieldGet(field_out,typekind=tk_out,_RC)
      call ESMF_FieldGet(left_field,typekind=tk_in,_RC)
      _ASSERT(tk_in == tk_out, "Left field type does not match result field type.")
      call ESMF_FieldGet(right_field,typekind=tk_in,_RC)
      _ASSERT(tk_in == tk_out, "Right field type does not match result field type.")
      if (tk_out == ESMF_TypeKind_R4) then
         call set_r4(_RC)
      else if (tk_out == ESMF_TypeKind_R8) then
         call set_r8(_RC)
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

   contains

#if defined(KIND_)
#   undef KIND_
#endif

#define KIND_ ESMF_KIND_R4
   subroutine set_r4(rc)
#include "SetFieldTemplate.H"
   end subroutine set_r4
#   undef KIND_

#define KIND_ ESMF_KIND_R8
   subroutine set_r8(rc)
#include "SetFieldTemplate.H"
   end subroutine set_r8
#   undef KIND_

   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
!vim: ft=fortran
