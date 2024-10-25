! THIS WILL PROBABLY GO AWAY
#include "function_overload.macro"

   subroutine _SUB(field_out,_LEFT_ARG,_RIGHT_ARG,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: _LEFT_ARG
      type(ESMF_Field), intent(inout) :: _RIGHT_ARG
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_left
      type(ESMF_TypeKind_Flag) :: tk_right
      type(ESMF_TypeKind_Flag) :: tk_out
      logical :: conformable

      conformable = FieldsAreConformable(_LEFT_ARG,field_out,_RC)
      _ASSERT(conformable,"Left field passed to subroutine is not conformable with result field.")
      call ESMF_FieldGet(field_out,typekind=tk_out,_RC)
      call ESMF_FieldGet(_LEFT_ARG,typekind=tk_left,_RC)
      _ASSERT(tk_left == tk_out, "Left field typekind does not match result field typekind.")

      call ESMF_FieldGet(_RIGHT_ARG,typekind=tk_right,_RC)
      conformable = FieldsAreConformable(_RIGHT_ARG,field_out,_RC)
      _ASSERT(conformable,"Right field passed to subroutine is not conformable with result field.")

      if(tk_left == ESMF_TypeKind_R4 .and. tk_right == ESMF_TypeKind_R4) then
         call set_r4r4(_RC)
      else if(tk_left == ESMF_TypeKind_R4 .and. tk_right == ESMF_TypeKind_R8) then
         call set_r4r8(_RC)
      else if(tk_left == ESMF_TypeKind_R8 .and. tk_right == ESMF_TypeKind_R8) then
         call set_r4r8(_RC)
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

      else if (tk_left == ESMF_TypeKind_R4 .and. tk_right == ESMF_TypeKind_Logical) then
         call set_r4(_RC)
      else if (tk_left == ESMF_TypeKind_R8 .and. tk_right == ESMF_TypeKind_R8) then
        call set_r8r8(_RC)
     else if (tk_left == 
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
