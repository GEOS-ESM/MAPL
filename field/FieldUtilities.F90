#include "MAPL.h"

module MAPL_FieldUtilities
   use mapl3g_FieldInfo
   use MAPL_ErrorHandlingMod
   use MAPL_FieldPointerUtilities
   use mapl3g_InfoUtilities
   use mapl3g_UngriddedDims
   use mapl3g_LU_Bound
   use mapl_KeywordEnforcer
   use esmf

   implicit none (type, external)
   private

   public :: FieldIsConstant
   public :: FieldSet
   public :: FieldNegate
   public :: FieldPow

   interface FieldIsConstant
      procedure FieldIsConstantR4
      procedure FieldIsConstantR8
   end interface FieldIsConstant

   interface FieldSet
      procedure FieldSet_R4
      procedure FieldSet_R8
   end interface FieldSet

contains

   function FieldIsConstantR4(field,constant_val,rc) result(field_is_constant)
      logical :: field_is_constant
      type(ESMF_Field), intent(inout) :: field
      real(kind=ESMF_KIND_R4) :: constant_val
      integer, optional, intent(out) :: rc

      integer :: status

      real(ESMF_KIND_R4), pointer :: f_ptr_r4(:)

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

   end function FieldIsConstantR4

   function FieldIsConstantR8(field,constant_val,rc) result(field_is_constant)
      logical :: field_is_constant
      type(ESMF_Field), intent(inout) :: field
      real(kind=ESMF_KIND_R8) :: constant_val
      integer, optional, intent(out) :: rc

      integer :: status

      real(ESMF_KIND_R8), pointer :: f_ptr_r8(:)

      type(ESMF_TypeKind_Flag) :: type_kind

      call ESMF_FieldGet(field,typekind=type_kind,_RC)

      field_is_constant = .false.
      if (type_kind == ESMF_TYPEKIND_R8) then
         call assign_fptr(field,f_ptr_r8,_RC)
         field_is_constant = all(f_ptr_r8 == constant_val)
      else
         _FAIL("constant_val is double precision so you can not check if it is all undef for an R4")
      end if

      _RETURN(_SUCCESS)

   end function FieldIsConstantR8


   subroutine FieldSet_r8(field,constant_val,rc)
      type(ESMF_Field), intent(inout) :: field
      real(kind=ESMF_KIND_r8), intent(in) :: constant_val
      integer, intent(out), optional :: rc

      type(ESMF_TYPEKIND_FLAG) :: type_kind
      real(kind=ESMF_KIND_R4), pointer :: f_ptr_r4(:)
      real(kind=ESMF_KIND_R8), pointer :: f_ptr_r8(:)
      integer :: status

      call ESMF_FieldGet(field,typekind=type_kind,_RC)
      if (type_kind == ESMF_TYPEKIND_R4) then
         call assign_fptr(field,f_ptr_r4,_RC)
         f_ptr_r4 = constant_val
      else if (type_kind == ESMF_TYPEKIND_R8) then
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
      integer :: status

      call ESMF_FieldGet(field,typekind=type_kind,_RC)
      if (type_kind == ESMF_TYPEKIND_R4) then
         call assign_fptr(field,f_ptr_r4,_RC)
         f_ptr_r4 = constant_val
      else if (type_kind == ESMF_TYPEKIND_R8) then
         call assign_fptr(field,f_ptr_r8,_RC)
         f_ptr_r8 = constant_val
      else
         _FAIL('unsupported typekind')
      end if
      _RETURN(ESMF_SUCCESS)
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
      has_undef = FieldsHaveUndef(fields,_RC)  
      call ESMF_FieldGet(field,typekind=type_kind,_RC)
      if (type_kind == ESMF_TYPEKIND_R4) then
         call assign_fptr(field,f_ptr_r4,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r4,_RC)
            where(f_ptr_r4 /= undef_r4(1))
               f_ptr_r4 = -f_ptr_r4
            end where
         else
            f_ptr_r4 = -f_ptr_r4
         end if
      else if (type_kind == ESMF_TYPEKIND_R4) then
         call assign_fptr(field,f_ptr_r8,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r8,_RC)
            where(f_ptr_r8 /= undef_r8(1))
               f_ptr_r8 = -f_ptr_r8
            end where
         else
            f_ptr_r8 = -f_ptr_r8
         end if
      else
         _FAIL('unsupported typekind')
      end if
      _RETURN(ESMF_SUCCESS)
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

      conformable = FieldsAreConformable(field_in,field_out,_RC)
      _ASSERT(conformable,"Fields passed power function are not conformable")

      fields(1) = field_in
      fields(2) = field_out
      has_undef = FieldsHaveUndef(fields,_RC)
      call ESMF_FieldGet(field_in,typekind=tk_in,_RC)
      call ESMF_FieldGet(field_out,typekind=tk_out,_RC)
      _ASSERT(tk_in == tk_out, "For now input and output field must be of same type for a field function")
      if (tk_in == ESMF_TYPEKIND_R4) then
         call assign_fptr(field_in,ptr_r4_in,_RC)
         call assign_fptr(field_out,ptr_r4_out,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r4,_RC)
            where(ptr_r4_in /= undef_r4(1))
               ptr_r4_out = ptr_r4_in**expo
            elsewhere
               ptr_r4_out = undef_r4(2)
            end where
         else
            ptr_r4_out = ptr_r4_in**expo
         end if
      else if (tk_in == ESMF_TYPEKIND_R8) then
         call assign_fptr(field_in,ptr_r8_in,_RC)
         call assign_fptr(field_out,ptr_r8_out,_RC)
         if (has_undef) then
            call GetFieldsUndef(fields,undef_r8,_RC)
            where(ptr_r8_in /= undef_r8(1))
               ptr_r8_out = ptr_r8_in**expo
            elsewhere
               ptr_r8_out = undef_r8(2)
            end where
         else
            ptr_r8_out = ptr_r8_in**expo
         end if
      else
         _FAIL('unsupported typekind')
      end if
      _RETURN(ESMF_SUCCESS)
   end subroutine FieldPow


end module MAPL_FieldUtilities


