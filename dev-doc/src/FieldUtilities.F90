#include "MAPL_Generic.h"

module MAPL_FieldUtilities
use ESMF
use MAPL_BaseMod, only: MAPL_Undef
use MAPL_ErrorHandlingMod

implicit none
private

public is_field_undef

contains

function is_field_undef(field,rc) result(field_is_undef)
   logical :: field_is_undef
   type(ESMF_Field), intent(in) :: field
   integer, optional, intent(out) :: rc

   integer :: status
 
   real(ESMF_KIND_R4), pointer :: ptr_1d_r4(:), ptr_2d_r4(:,:), ptr_3d_r4(:,:,:), ptr_4d_r4(:,:,:,:)

   integer :: rank
   type(ESMF_TypeKind_Flag) :: typekind
   
   call ESMF_FieldGet(field,rank=rank,typekind=typekind,_RC)

   if (typekind == ESMF_TYPEKIND_R4) then
      select case(rank)
      case(1)
         call ESMF_FieldGet(field,0,farrayPtr=ptr_1d_r4,_RC)
         field_is_undef = all(ptr_1d_r4 == MAPL_UNDEF)
      case(2)
         call ESMF_FieldGet(field,0,farrayPtr=ptr_2d_r4,_RC)
         field_is_undef = all(ptr_2d_r4 == MAPL_UNDEF)
      case(3)
         call ESMF_FieldGet(field,0,farrayPtr=ptr_3d_r4,_RC)
         field_is_undef = all(ptr_3d_r4 == MAPL_UNDEF)
      case(4)
         call ESMF_FieldGet(field,0,farrayPtr=ptr_4d_r4,_RC)
         field_is_undef = all(ptr_4d_r4 == MAPL_UNDEF)
      end select
   else
      _FAIL("MAPL_UNDEF is single precision so you can not check if it is all undef for an R8")
   end if

   _RETURN(_SUCCESS)

end function

end module
    
