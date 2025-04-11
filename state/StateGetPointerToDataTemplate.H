#ifdef DIMENSIONS_
#undef DIMENSIONS_
#endif

#include "overload.macro"

subroutine SUB_(state, ptr, name, not_found_ok, rc)
   type(ESMF_State), intent(INOUT) :: state
   real(kind=EKIND_), pointer :: ptr DIMENSIONS_
   character(len=*), intent(IN) :: name
   logical, optional,intent(IN) :: not_found_ok
   integer, optional,intent(OUT) :: rc

   type (ESMF_FieldBundle) :: bundle
   type (ESMF_Field) :: field
   logical :: is_ok
   integer :: loc
   type(ESMF_FieldStatus_Flag) :: field_status
   type(ESMF_StateItem_Flag) :: item_type

   integer :: status

   nullify(ptr)

   if (present(not_found_ok)) then
      is_ok = not_found_ok
   else
      is_ok = .false.
   end if

   ! Get field from state
   loc = index(name,';;')
   if(loc/=0) then
      call ESMF_StateGet(state, name(:loc-1), itemType=item_type, _RC)
      if (item_type /= ESMF_STATEITEM_FIELDBUNDLE .and. is_ok) then
         _RETURN(ESMF_SUCCESS)
      else
         call ESMF_StateGet(state, name(:loc-1), bundle, _RC)
      end if
      call ESMF_StateGet(state, name(loc+2:), itemType=item_type, _RC)
      if (item_type /= ESMF_STATEITEM_FIELD .and. is_ok) then
         _RETURN(ESMF_SUCCESS)
      else
         call ESMF_StateGet(state, name(loc+2:), field, _RC)
      end if
   else
      call ESMF_StateGet(state, name, itemType=item_type, _RC)
      if (item_type /= ESMF_STATEITEM_FIELD .and. is_ok) then
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
