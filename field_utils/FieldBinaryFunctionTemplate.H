#define _NAME field
#include "function_overload.macro"

   subroutine _SUB(field_out,left_field,right_field,set_undef,rc)
      type(ESMF_Field), intent(inout) :: field_out
      type(ESMF_Field), intent(inout) :: left_field
      type(ESMF_Field), intent(inout) :: right_field
      logical, optional, intent(in) :: set_undef
      integer, optional, intent(out) :: rc

      integer :: status
      integer, parameter :: LEFT = 1
      integer, parameter :: RIGHT = 2
      integer, parameter :: OUT_ = 3
      type(ESMF_Field) :: fields(3)
      type(ESMF_TypeKind_Flag) :: tk_in
      type(ESMF_TypeKind_Flag) :: tk_out
      logical :: has_undef, conformable, setundef

      setundef = .TRUE.
      if(present(set_undef)) setundef = set_undef

      conformable = FieldsAreConformable(left_field,field_out,_RC)
      _ASSERT(conformable,"Left field passed to binary function is not conformable with result field.")
      conformable = FieldsAreConformable(right_field,field_out,_RC)
      _ASSERT(conformable,"Right field passed to binary function is not conformable with result field.")

      fields(LEFT) = left_field
      fields(RIGHT) = right_field
      fields(OUT_) = field_out
      has_undef = FieldsHaveUndef(fields,_RC)
      call ESMF_FieldGet(field_out,typekind=tk_out,_RC)
      call ESMF_FieldGet(left_field,typekind=tk_in,_RC)
      _ASSERT(tk_in == tk_out, "Left field type does not match result field type.")
      call ESMF_FieldGet(right_field,typekind=tk_in,_RC)
      _ASSERT(tk_in == tk_out, "Right field type does not match result field type.")
      if (tk_out ==ESMF_TypeKind_R4) then
         call set_r4(_RC)
      else if (tk_out ==ESMF_TypeKind_R8) then
         call set_r8(_RC)
      else
          _FAIL("unsupported type")
      end if
      _RETURN(_SUCCESS)

   contains

      subroutine set_r4(rc)
         integer, optional, intent(out) :: rc
         integer :: status

         real(ESMF_KIND_R4), allocatable :: undef
         real(kind=ESMF_KIND_R4), pointer :: ptr_left(:) => null()
         real(kind=ESMF_KIND_R4), pointer :: ptr_right(:) => null()
         real(kind=ESMF_KIND_R4), pointer :: ptr_out(:) => null()
         
         call assign_fptr(left_field,ptr_left,_RC)
         call assign_fptr(right_field,ptr_right,_RC)
         call assign_fptr(field_out,ptr_out,_RC)
         if(has_undef) then
            call GetFieldsUndef(fields,undef,_RC)
            where(ptr_left /= undef(LEFT) .and. ptr_right /= undef(RIGHT))
               ptr_out = _FUNC(ptr_left, ptr_right)
            elsewhere
               if(setundef) ptr_out = undef(OUT_)
           end where
         else
            ptr_out = _FUNC(ptr_left, ptr_right)
         end if
      end subroutine set_r4

      subroutine set_r8(rc)
         integer, optional, intent(out) :: rc
         integer :: status

         real(ESMF_KIND_R8), allocatable :: undef
         real(kind=ESMF_KIND_R8), pointer :: ptr_left(:) => null()
         real(kind=ESMF_KIND_R8), pointer :: ptr_right(:) => null()
         real(kind=ESMF_KIND_R8), pointer :: ptr_out(:) => null()
         
         call assign_fptr(left_field,ptr_left,_RC)
         call assign_fptr(right_field,ptr_right,_RC)
         call assign_fptr(field_out,ptr_out,_RC)
         if(has_undef) then
            call GetFieldsUndef(fields,undef,_RC)
            where(ptr_left /= undef(LEFT) .and. ptr_right /= undef(RIGHT))
               ptr_out = _FUNC(ptr_left, ptr_right)
            elsewhere
               if(setundef) ptr_out = undef(OUT_)
           end where
         else
            ptr_out = _FUNC(ptr_left, ptr_right)
         end if
      end subroutine set_r8

   end subroutine _SUB

#include "undo_function_overload.macro"
#undef _NAME
