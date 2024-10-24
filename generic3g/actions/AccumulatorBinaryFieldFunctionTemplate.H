!_LEFT_ARG, _FUNC_BASE
!_RIGHT_ARG
!_CHECK_ZEROES, _SETUNDEF
#include "function_overload.macro"
#define _FUNC 
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
         call set_r8r8(_RC)
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

   contains

#if defined(_LEFTK)
#   undef _LEFTK
#endif
#if defined(_RIGHTK)
#   undef _RIGHTK
#endif

#define _LEFTK R4
#define _RIGHTK R4
#include "function_overload.macro"
#undef _LEFTK
#undef _RIGHTK
   subroutine set_r4r4(rc)
#include "SetFieldTemplate.H"
   end subroutine set_r4r4
#include "undo_function_overload.macro"

#define _LEFTK R8
#define _RIGHTK R8
#include "function_overload.macro"
#undef _LEFTK
#undef _RIGHTK
   subroutine set_r8r8(rc)
#include "SetFieldTemplate.H"
   end subroutine set_r8r8
#include "undo_function_overload.macro"
#define _LEFTK R4
#define _RIGHTK R8
#include "function_overload.macro"
#undef _LEFTK
#undef _RIGHTK
   subroutine set_r4r8(rc)
#include "SetFieldTemplate.H"
   end subroutine set_r4r8
#include "undo_function_overload.macro"
!vim: ft=fortran
