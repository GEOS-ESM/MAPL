#include "MAPL_Generic.h"

module mapl3g_FieldBundleCreate

   use mapl_ErrorHandling
   use esmf

   implicit none

   private

   public :: MAPL_FieldBundleCreate

   interface MAPL_FieldBundleCreate
      procedure create_bundle_from_state
      procedure create_bundle_from_field_list
   end interface MAPL_FieldBundleCreate

contains

   function create_bundle_from_state(state, rc) result(bundle)
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: bundle ! result

      character(len=ESMF_MAXSTR), allocatable :: item_name(:)
      type (ESMF_StateItem_Flag), allocatable  :: item_type(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: item_count, idx, status

      ! bundle to pack fields in
      bundle = ESMF_FieldBundleCreate(_RC)
      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(item_name(item_count), _STAT)
      allocate(item_type(item_count), _STAT)
      call ESMF_StateGet(state, itemNameList=item_name, itemTypeList=item_type, _RC)
      do idx = 1, item_count
         if (item_type(idx) /= ESMF_STATEITEM_FIELD) then
            _FAIL("FieldBundle has not been implemented yet")
         end if
         call ESMF_StateGet(state, item_name(idx), field, _RC)
         call ESMF_FieldGet(field, status=field_status, _RC)
         if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
            call ESMF_FieldBundleAdd(bundle, [field], _RC)
         end if
      end do
      deallocate(item_name, item_type, _STAT)

      _RETURN(_SUCCESS)
   end function create_bundle_from_state

   function create_bundle_from_field_list(field_list, rc) result(bundle)
      type(ESMF_Field), intent(in) :: field_list(:)
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: bundle ! result

      _FAIL("not implemented yet")
   end function create_bundle_from_field_list

end module mapl3g_FieldBundleCreate
