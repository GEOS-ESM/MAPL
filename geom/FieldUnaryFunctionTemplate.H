#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field,undef,rc)
      type(ESMF_Field), intent(inout) :: field
      real, optional, intent(in) :: undef
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk

      real(kind=ESMF_KIND_R4), pointer :: ptr_r4(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8(:)

      call ESMF_FieldGet(field,typekind=tk,_RC)
      if (tk ==ESMF_TypeKind_R4) then
         call assign_fptr(field,ptr_r4,_RC)
         if (present(undef)) then
            where(ptr_r4 /= undef)
               ptr_r4 = _FUNC(ptr_r4)
            end where
         else
            ptr_r4 = _FUNC(ptr_r4)
         end if
      else if (tk == ESMF_TypeKind_R8) then
         call assign_fptr(field,ptr_r8,_RC)
         if (present(undef)) then
            where(ptr_r8 /= undef)
               ptr_r8 = _FUNC(ptr_r4)
            end where
         else
            ptr_r8 = _FUNC(ptr_r8)
         end if
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)
   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
