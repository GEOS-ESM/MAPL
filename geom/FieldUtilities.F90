#include "MAPL_Generic.h"

module MAPL_FieldUtilities
use ESMF
use MAPL_ErrorHandlingMod
use MAPL_FieldPointerUtilities

implicit none
private

public FieldIsConstant
public FieldSet

interface FieldSet
   module procedure FieldSet_R4
   module procedure FieldSet_R8
end interface


contains

function FieldIsConstant(field,constant_val,rc) result(field_is_constant)
   logical :: field_is_constant
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_R4) :: constant_val
   integer, optional, intent(out) :: rc

   integer :: status
 
   real(ESMF_KIND_R4), pointer :: f_ptr_r4(:),ptr2d(:,:)

   type(ESMF_TypeKind_Flag) :: type_kind
   
   call ESMF_FieldGet(field,typekind=type_kind,_RC)

   field_is_constant = .false.
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,_RC)
      field_is_constant = all(f_ptr_r4 == constant_val)
   else
      _FAIL("constant_val is single precision so you can not check if it is all undef for an R8")
   end if

   _RETURN(_SUCCESS)

end function

subroutine FieldSet_r8(field,constant_val,rc)
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_r8), intent(in) :: constant_val
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: f_ptr_r4(:)
   real(kind=ESMF_KIND_R8), pointer :: f_ptr_r8(:)
   integer :: status, rank

   call ESMF_FieldGet(field,rank=rank,typekind=type_kind,_RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,_RC)
      f_ptr_r4 = constant_val
   else if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r8,_RC)
      f_ptr_r8 = constant_val
   else
      _FAIL('unsupported typekind')
   end if
   _RETURN(ESMF_SUCCESS)
end subroutine FieldSet_r8

subroutine FieldSet_r4(field,constant_val,rc)
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_r4), intent(in) :: constant_val
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: f_ptr_r4(:)
   real(kind=ESMF_KIND_R8), pointer :: f_ptr_r8(:)
   integer :: status, rank

   call ESMF_FieldGet(field,rank=rank,typekind=type_kind,_RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,_RC)
      f_ptr_r4 = constant_val
   else if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r8,_RC)
      f_ptr_r8 = constant_val
   else
      _FAIL('unsupported typekind')
   end if
   _RETURN(ESMF_SUCCESS)
end subroutine FieldSet_r4

end module
    
