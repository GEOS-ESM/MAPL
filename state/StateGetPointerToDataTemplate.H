#ifdef DIMENSIONS_
#undef DIMENSIONS_
#endif

#include "overload.macro"

subroutine SUB_(state, ptr, name, is_present, rc)
   type(ESMF_State), intent(inout) :: state
   real(kind=EKIND_), pointer :: ptr DIMENSIONS_
   character(len=*), intent(in) :: name
   logical, optional,intent(out) :: is_present
   integer, optional,intent(out) :: rc

   type (ESMF_FieldBundle) :: bundle
   type (ESMF_Field) :: field
   integer :: loc
   type(ESMF_FieldStatus_Flag) :: field_status
   type(ESMF_StateItem_Flag) :: item_type

   integer :: status

   nullify(ptr)
   if (present(is_present)) is_present = .true.

   ! Get field from state
   loc = index(name,';;')
   if(loc/=0) then
      call ESMF_StateGet(state, name(:loc-1), itemType=item_type, _RC)
      if (item_type /= ESMF_STATEITEM_FIELDBUNDLE) then
         if (present(is_present)) is_present = .false.
         _RETURN(ESMF_SUCCESS)
      else
         call ESMF_StateGet(state, name(:loc-1), bundle, _RC)
      end if
      call ESMF_StateGet(state, name(loc+2:), itemType=item_type, _RC)
      if (item_type /= ESMF_STATEITEM_FIELD) then
         if (present(is_present)) is_present = .false.
         _RETURN(ESMF_SUCCESS)
      else
         call ESMF_StateGet(state, name(loc+2:), field, _RC)
      end if
   else
      call ESMF_StateGet(state, name, itemType=item_type, _RC)
      if (item_type /= ESMF_STATEITEM_FIELD) then
         if (present(is_present)) is_present = .false.
         _RETURN(ESMF_SUCCESS)
      else
         call ESMF_StateGet(state, name, field, _RC)
      end if
   end if

   ! Get pointer to data from field
   call ESMF_FieldGet(field, status=field_status, _RC)
   if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_FieldGet(field, 0, ptr, _RC)
   end if

   _RETURN(ESMF_SUCCESS)
end subroutine SUB_
