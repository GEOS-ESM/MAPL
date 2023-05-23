#include "MAPL_Generic.h"

module MAPL_FieldUtilities
use ESMF
use MAPL_BaseMod, only: MAPL_Undef
use MAPL_ErrorHandlingMod

implicit none
private

public is_field_undef
public set_field_to_constant
public set_state_field_to_constant

interface set_state_field_to_constant
   module procedure set_state_field_to_constant_r4
   module procedure set_state_field_to_constant_r8
end interface

interface set_field_to_constant
   module procedure set_field_to_constant_r4
   module procedure set_field_to_constant_r8
end interface

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

subroutine set_state_field_to_constant_r8(state,field_name,constant_val,rc)
   type(ESMF_State), intent(inout) :: state
   character(len=*), intent(in) :: field_name
   real(kind=ESMF_KIND_R8), intent(in) :: constant_val
   integer, optional, intent(out) :: rc

   type(ESMF_Field) :: field
   integer :: status

   call ESMF_StateGet(state,field_name,field,_RC)
   call set_field_to_constant(field,constant_val,_RC)

   _RETURN(_SUCCESS)
end subroutine set_state_field_to_constant_r8

subroutine set_state_field_to_constant_r4(state,field_name,constant_val,rc)
   type(ESMF_State), intent(inout) :: state
   character(len=*), intent(in) :: field_name
   real(kind=ESMF_KIND_R4), intent(in) :: constant_val
   integer, optional, intent(out) :: rc

   type(ESMF_Field) :: field
   integer :: status

   call ESMF_StateGet(state,field_name,field,_RC)
   call set_field_to_constant(field,constant_val,_RC)

   _RETURN(_SUCCESS)
end subroutine set_state_field_to_constant_r4

subroutine set_field_to_constant_r4(field,constant_val,rc)
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_r4), intent(in) :: constant_val
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: x_r4_1d(:),x_r4_2d(:,:),x_r4_3d(:,:,:),x_r4_4d(:,:,:,:),x_r4_5d(:,:,:,:,:)
   real(kind=ESMF_KIND_R8), pointer :: x_r8_1d(:),x_r8_2d(:,:),x_r8_3d(:,:,:),x_r8_4d(:,:,:,:),x_r8_5d(:,:,:,:,:)
   integer :: status, rank

   call ESMF_FieldGet(field,rank=rank,typekind=type_kind,_RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      if (rank == 1) then
         call ESMF_FieldGet(field,farrayptr=x_r4_1d,_RC)
         x_r4_1d = constant_val
      else if (rank == 2) then
         call ESMF_FieldGet(field,farrayptr=x_r4_2d,_RC)
         x_r4_2d = constant_val
      else if (rank == 3) then
         call ESMF_FieldGet(field,farrayptr=x_r4_3d,_RC)
         x_r4_3d = constant_val
      else if (rank == 4) then
         call ESMF_FieldGet(field,farrayptr=x_r4_4d,_RC)
         x_r4_4d = constant_val
      else if (rank == 5) then
         call ESMF_FieldGet(field,farrayptr=x_r4_4d,_RC)
         x_r4_5d = constant_val
      else
         _FAIL('unsupported rank')
      end if
   else if (type_kind == ESMF_TYPEKIND_R8) then
      if (rank == 1) then
         call ESMF_FieldGet(field,farrayptr=x_r8_1d,_RC)
         x_r8_1d = constant_val
      else if (rank == 2) then
         call ESMF_FieldGet(field,farrayptr=x_r8_2d,_RC)
         x_r8_2d = constant_val
      else if (rank == 3) then
         call ESMF_FieldGet(field,farrayptr=x_r8_3d,_RC)
         x_r8_3d = constant_val
      else if (rank == 4) then
         call ESMF_FieldGet(field,farrayptr=x_r8_4d,_RC)
         x_r8_4d = constant_val
      else if (rank == 4) then
         call ESMF_FieldGet(field,farrayptr=x_r8_5d,_RC)
         x_r8_5d = constant_val
      else
         _FAIL('unsupported rank')
      end if
   else
      _FAIL('unsupported typekind')
   end if
   _RETURN(ESMF_SUCCESS)
end subroutine set_field_to_constant_r4

subroutine set_field_to_constant_r8(field,constant_val,rc)
   type(ESMF_Field), intent(inout) :: field
   real(kind=ESMF_KIND_R8), intent(in) :: constant_val
   integer, intent(out), optional :: rc

   type(ESMF_TYPEKIND_FLAG) :: type_kind
   real(kind=ESMF_KIND_R4), pointer :: x_r4_1d(:),x_r4_2d(:,:),x_r4_3d(:,:,:),x_r4_4d(:,:,:,:),x_r4_5d(:,:,:,:,:)
   real(kind=ESMF_KIND_R8), pointer :: x_r8_1d(:),x_r8_2d(:,:),x_r8_3d(:,:,:),x_r8_4d(:,:,:,:),x_r8_5d(:,:,:,:,:)
   integer :: status, rank

   call ESMF_FieldGet(field,rank=rank,typekind=type_kind,_RC)
   if (type_kind == ESMF_TYPEKIND_R4) then
      if (rank == 1) then
         call ESMF_FieldGet(field,farrayptr=x_r4_1d,_RC)
         x_r4_1d = constant_val
      else if (rank == 2) then
         call ESMF_FieldGet(field,farrayptr=x_r4_2d,_RC)
         x_r4_2d = constant_val
      else if (rank == 3) then
         call ESMF_FieldGet(field,farrayptr=x_r4_3d,_RC)
         x_r4_3d = constant_val
      else if (rank == 4) then
         call ESMF_FieldGet(field,farrayptr=x_r4_4d,_RC)
         x_r4_4d = constant_val
      else if (rank == 5) then
         call ESMF_FieldGet(field,farrayptr=x_r4_4d,_RC)
         x_r4_5d = constant_val
      else
         _FAIL('unsupported rank')
      end if
   else if (type_kind == ESMF_TYPEKIND_R8) then
      if (rank == 1) then
         call ESMF_FieldGet(field,farrayptr=x_r8_1d,_RC)
         x_r8_1d = constant_val
      else if (rank == 2) then
         call ESMF_FieldGet(field,farrayptr=x_r8_2d,_RC)
         x_r8_2d = constant_val
      else if (rank == 3) then
         call ESMF_FieldGet(field,farrayptr=x_r8_3d,_RC)
         x_r8_3d = constant_val
      else if (rank == 4) then
         call ESMF_FieldGet(field,farrayptr=x_r8_4d,_RC)
         x_r8_4d = constant_val
      else if (rank == 4) then
         call ESMF_FieldGet(field,farrayptr=x_r8_5d,_RC)
         x_r8_5d = constant_val
      else
         _FAIL('unsupported rank')
      end if
   else
      _FAIL('unsupported typekind')
   end if
   _RETURN(ESMF_SUCCESS)
   end subroutine set_field_to_constant_r8

end module
    
