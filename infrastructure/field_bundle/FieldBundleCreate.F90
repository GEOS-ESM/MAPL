#include "MAPL.h"

module mapl_FieldBundleCreate_mod

   use mapl_Enums_internal, only: MAPL_FieldBundleType_Flag, &
        MAPL_FIELDBUNDLETYPE_BASIC, MAPL_FIELDBUNDLETYPE_VECTOR, MAPL_FIELDBUNDLETYPE_VECTORBRACKET, &
        MAPL_VECTOR_BASIS_KIND_NS, operator(==)
   use mapl_FieldBundleSet_mod, only: FieldBundleSet
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use esmf

   implicit none(type,external)
   private

   public :: FieldBundleCreate
   public :: FieldBundlesAreAliased

   interface FieldBundleCreate
      procedure create_bundle_empty
      procedure create_bundle_from_state
      procedure create_bundle_from_field_list
   end interface FieldBundleCreate

   interface FieldBundlesAreAliased
      procedure :: bundles_are_aliased
   end interface FieldBundlesAreAliased
contains

   function create_bundle_empty(unusable, name, fieldBundleType, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle ! result
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      type(MAPL_FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      integer, optional, intent(out) :: rc

      type(MAPL_FieldBundleType_Flag) :: fieldbundletype_
      integer :: status

      bundle = ESMF_FieldBundleCreate(name=name, _RC)

      fieldBundleType_ = MAPL_FIELDBUNDLETYPE_BASIC
      if (present(fieldBundleType)) fieldBundleType_ = fieldBundleType
      call FieldBundleSet(bundle, fieldBundleType=fieldBundleType_, _RC)
      
      ! Set default vector basis kind for vector bundles
      if (fieldBundleType_ == MAPL_FIELDBUNDLETYPE_VECTOR .or. &
          fieldBundleType_ == MAPL_FIELDBUNDLETYPE_VECTORBRACKET) then
         call FieldBundleSet(bundle, vector_basis_kind=MAPL_VECTOR_BASIS_KIND_NS, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_bundle_empty

   function create_bundle_from_state(state, unusable, name, fieldBundleType, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle ! result
      type(ESMF_State), intent(in) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      type(MAPL_FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR), allocatable :: item_name(:)
      type (ESMF_StateItem_Flag), allocatable  :: item_type(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldStatus_Flag) :: field_status
      integer :: item_count, idx, status

      ! bundle to pack fields in
      bundle = FieldBundleCreate(name=name, fieldBundleType=fieldBundleType, _RC)

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(item_name(item_count), _STAT)
      allocate(item_type(item_count), _STAT)
      call ESMF_StateGet(state, itemNameList=item_name, itemTypeList=item_type, _RC)
      do idx = 1, item_count
         if (item_type(idx) /= ESMF_STATEITEM_FIELD) cycle
         call ESMF_StateGet(state, item_name(idx), field, _RC)
         call ESMF_FieldGet(field, status=field_status, _RC)
         if (field_status == ESMF_FIELDSTATUS_COMPLETE) then
            call ESMF_FieldBundleAdd(bundle, [field], _RC)
         end if
      end do

      deallocate(item_name, item_type, _STAT)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_bundle_from_state

   function create_bundle_from_field_list(fieldList, unusable, name, fieldBundleType, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle ! result
      type(ESMF_Field), intent(in) :: fieldList(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      type(MAPL_FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      integer, optional, intent(out) :: rc

      integer :: status
      bundle = FieldBundleCreate(name=name, fieldBundleType=fieldBundleType, _RC)
      call ESMF_FieldBundleAdd(bundle, fieldList=fieldList, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_bundle_from_field_list

   logical function bundles_are_aliased(bundle1, bundle2, rc) result(are_aliased)
      type(esmf_FieldBundle), intent(in) :: bundle1
      type(esmf_FieldBundle), intent(in) :: bundle2
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_created

      is_created = esmf_FieldBundleIsCreated(bundle1, _RC)
      _ASSERT(is_created, 'invalid field bundle detected')
      is_created = esmf_FieldBundleIsCreated(bundle2, _RC)
      _ASSERT(is_created, 'invalid field bundle detected')

      are_aliased = associated(bundle1%this, bundle2%this)

      _RETURN(_SUCCESS)
   end function bundles_are_aliased

end module mapl_FieldBundleCreate_mod
