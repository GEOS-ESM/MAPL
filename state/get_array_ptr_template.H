#ifdef DIMENSIONS_
#  undef DIMENSIONS_
#endif

#include "overload.macro"

subroutine SUB_ (state, farrayPtr, itemName, unusable, isPresent, rc)
   type(ESMF_State), intent(inout) :: state
   real(kind=EKIND_), pointer :: farrayPtr DIMENSIONS_
   character(len=*), intent(in) :: itemName
   class(KeywordEnforcer), optional, intent(in) :: unusable
   logical, optional,intent(out) :: isPresent
   integer, optional,intent(out) :: rc

   type (ESMF_FieldBundle) :: bundle
   type (ESMF_Field) :: field
   type(ESMF_FieldStatus_Flag) :: field_status
   type(ESMF_StateItem_Flag) :: item_type

   integer :: status
   logical :: isPresent_

   nullify(farrayPtr)
   if (present(isPresent)) isPresent = .false.

   call ESMF_StateGet(state, itemName, itemType=item_type, _RC)
   if (item_type /= ESMF_STATEITEM_FIELD) then
      _RETURN(ESMF_SUCCESS)
   end if

   if (present(isPresent)) isPresent = .true.
   call ESMF_StateGet(state, itemName, field, _RC)

   ! Get pointer to data from field
   call ESMF_FieldGet(field, status=field_status, _RC)
   if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_FieldGet(field, 0, farrayPtr, _RC)
   end if

   _RETURN(ESMF_SUCCESS)
end subroutine SUB_


#include "undef.macro"
