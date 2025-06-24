#include "MAPL_Generic.h"

module MAPL_FieldUtilities
use ESMF
use MAPL_ErrorHandlingMod
use MAPL_FieldPointerUtilities

implicit none
private

public FieldIsConstant
public FieldSet
public FieldNegate
public FieldPow

interface FieldIsConstant
   module procedure FieldIsConstantR4
end interface

interface FieldSet
   module procedure FieldSet_R4
   module procedure FieldSet_R8
end interface

contains

function FieldIsConstantR4(field,constant_val,rc) result(field_is_constant)
   logical :: field_is_constant
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_R4) :: constant_val
   integer, optional, intent(out) :: rc

   integer :: status
 
   real(ESMF_KIND_R4), pointer :: f_ptr_r4(:)

   type(ESMF_TypeKind_Flag) :: type_kind
   
   call ESMF_FieldGet(field,typekind=type_kind,__RC)

   field_is_constant = .false.
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,__RC)
      field_is_constant = all(f_ptr_r4 == constant_val)
   else
      __FAIL("constant_val is single precision so you can not check if it is all undef for an R8")
   end if

   __RETURN(__SUCCESS)

end function FieldIsConstantR4

subroutine FieldSet_r8(field,constant_val,rc)
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_r8), intent(in) :: constant_val
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: f_ptr_r4(:)
   real(kind=ESMF_KIND_R8), pointer :: f_ptr_r8(:)
   integer :: status

   call ESMF_FieldGet(field,typekind=type_kind,__RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,__RC)
      f_ptr_r4 = constant_val
   else if (type_kind == ESMF_TYPEKIND_R8) then
      call assign_fptr(field,f_ptr_r8,__RC)
      f_ptr_r8 = constant_val
   else
      __FAIL('unsupported typekind')
   end if
   __RETURN(ESMF_SUCCESS)
end subroutine FieldSet_r8

subroutine FieldSet_r4(field,constant_val,rc)
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_r4), intent(in) :: constant_val
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: f_ptr_r4(:)
   real(kind=ESMF_KIND_R8), pointer :: f_ptr_r8(:)
   integer :: status

   call ESMF_FieldGet(field,typekind=type_kind,__RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,__RC)
      f_ptr_r4 = constant_val
   else if (type_kind == ESMF_TYPEKIND_R8) then
      call assign_fptr(field,f_ptr_r8,__RC)
      f_ptr_r8 = constant_val
   else
      __FAIL('unsupported typekind')
   end if
   __RETURN(ESMF_SUCCESS)
end subroutine FieldSet_r4

subroutine FieldNegate(field,rc)
   type(ESMF_Field), intent(inout) :: field
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: f_ptr_r4(:)
   real(kind=ESMF_KIND_R8), pointer :: f_ptr_r8(:)
   logical :: has_undef
   real(kind = ESMF_Kind_R4), allocatable :: undef_r4(:)
   real(kind = ESMF_Kind_R8), allocatable :: undef_r8(:) 
   integer :: status
   type(ESMF_Field) :: fields(1)


   fields(1) = field 
   has_undef = FieldsHaveUndef(fields,__RC)  
   call ESMF_FieldGet(field,typekind=type_kind,__RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r4,__RC)
      if (has_undef) then
         call GetFieldsUndef(fields,undef_r4,__RC)
         where(f_ptr_r4 /= undef_r4(1))
            f_ptr_r4 = -f_ptr_r4
         end where
      else
         f_ptr_r4 = -f_ptr_r4
      end if
   else if (type_kind == ESMF_TYPEKIND_R4) then
      call assign_fptr(field,f_ptr_r8,__RC)
      if (has_undef) then
         call GetFieldsUndef(fields,undef_r8,__RC)
         where(f_ptr_r8 /= undef_r8(1))
            f_ptr_r8 = -f_ptr_r8
         end where
      else
         f_ptr_r8 = -f_ptr_r8
      end if
   else
      __FAIL('unsupported typekind')
   end if
   __RETURN(ESMF_SUCCESS)
end subroutine FieldNegate

subroutine FieldPow(field_out,field_in,expo,rc)
   type(ESMF_Field), intent(inout) :: field_out
   type(ESMF_Field), intent(inout) :: field_in
   real, intent(in) :: expo
   integer, intent(out), optional :: rc

   real(kind = ESMF_Kind_R4), allocatable :: undef_r4(:)
   real(kind = ESMF_Kind_R8), allocatable :: undef_r8(:)
   type(ESMF_TypeKind_Flag) :: tk_in, tk_out
   real(kind=ESMF_KIND_R4), pointer :: ptr_r4_in(:),ptr_r4_out(:)
   real(kind=ESMF_KIND_R8), pointer :: ptr_r8_in(:),ptr_r8_out(:)
   integer :: status
   logical :: has_undef,conformable
   type(ESMF_Field) :: fields(2)

   conformable = FieldsAreConformable(field_in,field_out,__RC)
   __ASSERT(conformable,"Fields passed power function are not conformable")

   fields(1) = field_in
   fields(2) = field_out
   has_undef = FieldsHaveUndef(fields,__RC)
   call ESMF_FieldGet(field_in,typekind=tk_in,__RC)
   call ESMF_FieldGet(field_out,typekind=tk_out,__RC)
   __ASSERT(tk_in == tk_out, "For now input and output field must be of same type for a field function")
   if (tk_in == ESMF_TYPEKIND_R4) then
      call assign_fptr(field_in,ptr_r4_in,__RC)
      call assign_fptr(field_out,ptr_r4_out,__RC)
      if (has_undef) then
         call GetFieldsUndef(fields,undef_r4,__RC)
         where(ptr_r4_in /= undef_r4(1))
            ptr_r4_out = ptr_r4_in**expo
         elsewhere
            ptr_r4_out = undef_r4(2)
         end where
      else
         ptr_r4_out = ptr_r4_in**expo
      end if
   else if (tk_in == ESMF_TYPEKIND_R8) then
      call assign_fptr(field_in,ptr_r8_in,__RC)
      call assign_fptr(field_out,ptr_r8_out,__RC)
      if (has_undef) then
         call GetFieldsUndef(fields,undef_r8,__RC)
         where(ptr_r8_in /= undef_r8(1))
            ptr_r8_out = ptr_r8_in**expo
         elsewhere
            ptr_r8_out = undef_r8(2)
         end where
      else
         ptr_r8_out = ptr_r8_in**expo
      end if
   else
      __FAIL('unsupported typekind')
   end if
   __RETURN(ESMF_SUCCESS)
end subroutine FieldPow

end module
    
