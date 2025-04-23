#include "MAPL_Generic.h"

module mapl3g_FieldBundleCreate
   use mapl3g_FieldBundleType_Flag
   use mapl3g_FieldBundleSet
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf

   implicit none(type,external)

   private

   public :: FieldBundleCreate

   interface FieldBundleCreate
      procedure create_bundle_empty
      procedure create_bundle_from_state
      procedure create_bundle_from_field_list
   end interface FieldBundleCreate

contains

   function create_bundle_empty(unusable, name, fieldBundleType, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle ! result
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      integer, optional, intent(out) :: rc

      type(FieldBundleType_Flag) :: fieldbundletype_
      integer :: status

      bundle = ESMF_FieldBundleCreate(name=name, _RC)

      fieldBundleType_ = FIELDBUNDLETYPE_BASIC
      if (present(fieldBundleType)) fieldBundleType_ = fieldBundleType
      call FieldBundleSet(bundle, fieldBundleType=fieldBundleType_)

      _RETURN(_SUCCESS)
   end function create_bundle_empty


   function create_bundle_from_state(state, unusable, name, fieldBundleType, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle ! result
      type(ESMF_State), intent(in) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR), allocatable :: item_name(:)
      type (ESMF_StateItem_Flag), allocatable  :: item_type(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      type(FieldBundleType_Flag) :: fieldbundletype_
      integer :: item_count, idx, status

      ! bundle to pack fields in
      bundle = FieldBundleCreate(name=name, fieldBundleType=fieldBundleType, _RC)

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

   function create_bundle_from_field_list(fieldList, unusable, name, fieldBundleType, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle ! result
      type(ESMF_Field), intent(in) :: fieldList(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      integer, optional, intent(out) :: rc

      integer :: status
      bundle = FieldBundleCreate(name=name, fieldBundleType=fieldBundleType, _RC)
      call ESMF_FieldBundleAdd(bundle, fieldList=fieldList, _RC)

      _RETURN(_SUCCESS)
   end function create_bundle_from_field_list

end module mapl3g_FieldBundleCreate
