#include "MAPL.h"


module mapl_StateGetPointer_mod
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_enums_api, only: MAPL_FieldBundleType_Flag, MAPL_FIELDBUNDLETYPE_VECTOR, operator(==)
   use mapl_field_bundle_api, only: MAPL_FieldBundleGet, MAPL_FieldBundleGetPointer
   use esmf
   implicit none(type,external)
   private

   public :: StateGetPointer

   interface StateGetPointer
      module procedure state_get_array_ptr_r4_1d
      module procedure state_get_array_ptr_r4_2d
      module procedure state_get_array_ptr_r4_3d
      module procedure state_get_array_ptr_r4_4d
      module procedure state_get_array_ptr_r8_1d
      module procedure state_get_array_ptr_r8_2d
      module procedure state_get_array_ptr_r8_3d
      module procedure state_get_array_ptr_r8_4d
      module procedure state_get_vector_ptr_r4_2d
      module procedure state_get_vector_ptr_r4_3d
   end interface StateGetPointer

contains

#ifdef NAME_
#  undef NAME_
#endif

#define NAME_ state_get_array_ptr

#ifdef TYPEKIND_
#  undef TYPEKIND_
#endif

#define TYPEKIND_ R4


! StateGetPointerToDataR4_1
#define RANK_ 1
#include "get_array_ptr_template.H"
#undef RANK_

! StateGetPointerToDataR4_2
#define RANK_ 2
#include "get_array_ptr_template.H"
#undef RANK_

! StateGetPointerToDataR4_3
#define RANK_ 3
#include "get_array_ptr_template.H"
#undef RANK_

! StateGetPointerToDataR4_4
#define RANK_ 4
#include "get_array_ptr_template.H"
#undef RANK_

#undef TYPEKIND_

#define TYPEKIND_ R8

! StateGetPointerToDataR8_1
#define RANK_ 1
#include "get_array_ptr_template.H"
#undef RANK_

! StateGetPointerToDataR8_2
#define RANK_ 2
#include "get_array_ptr_template.H"
#undef RANK_

! StateGetPointerToDataR8_3
#define RANK_ 3
#include "get_array_ptr_template.H"
#undef RANK_

! StateGetPointerToDataR8_4
#define RANK_ 4
#include "get_array_ptr_template.H"
#undef RANK_

#undef TYPEKIND_

#undef NAME_

   subroutine state_get_vector_ptr_r4_3d(state, itemName, unusable, farrayPtr_1, farrayPtr_2, isPresent, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(in) :: itemName
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: farrayPtr_1(:,:,:)
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: farrayPtr_2(:,:,:)
      logical, optional, intent(out) :: isPresent
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: bundle
      type(ESMF_StateItem_Flag) :: item_type
      type(MAPL_FieldBundleType_Flag) :: fieldBundleType
      integer :: field_count

      integer :: status

      nullify(farrayPtr_1, farrayPtr_2)
      if (present(isPresent)) isPresent = .false.

      call ESMF_StateGet(state, itemName, itemType=item_type, _RC)
      _ASSERT(item_type == ESMF_STATEITEM_FIELDBUNDLE, 'expected vector (fieldBundle) for shortname: <'//itemName//'>')

      call ESMF_StateGet(state, itemName, bundle, _RC)
      call MAPL_FieldBundleGet(bundle, fieldBundleType=fieldBundleType, _RC)
      _ASSERT(fieldBundleType == MAPL_FIELDBUNDLETYPE_VECTOR, 'expected vector fieldBundleType for shortname: <'//itemName//'>')

      ! An optional vector export may be declared but not connected (0 fields)
      call MAPL_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      if (field_count == 2) then
         if (present(isPresent)) isPresent = .true.
         call MAPL_FieldBundleGetPointer(bundle, 1, farrayPtr_1, _RC)
         call MAPL_FieldBundleGetPointer(bundle, 2, farrayPtr_2, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine state_get_vector_ptr_r4_3d

   subroutine state_get_vector_ptr_r4_2d(state, itemName, unusable, farrayPtr_1, farrayPtr_2, isPresent, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(in) :: itemName
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: farrayPtr_1(:,:)
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: farrayPtr_2(:,:)
      logical, optional, intent(out) :: isPresent
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: bundle
      type(ESMF_StateItem_Flag) :: item_type
      type(MAPL_FieldBundleType_Flag) :: fieldBundleType
      integer :: field_count

      integer :: status

      nullify(farrayPtr_1, farrayPtr_2)
      if (present(isPresent)) isPresent = .false.

      call ESMF_StateGet(state, itemName, itemType=item_type, _RC)
      _ASSERT(item_type == ESMF_STATEITEM_FIELDBUNDLE, 'expected vector (fieldBundle) for shortname: <'//itemName//'>')

      call ESMF_StateGet(state, itemName, bundle, _RC)
      call MAPL_FieldBundleGet(bundle, fieldBundleType=fieldBundleType, _RC)
      _ASSERT(fieldBundleType == MAPL_FIELDBUNDLETYPE_VECTOR, 'expected vector fieldBundleType for shortname: <'//itemName//'>')

      ! An optional vector export may be declared but not connected (0 fields)
      call MAPL_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      if (field_count == 2) then
         if (present(isPresent)) isPresent = .true.
         call MAPL_FieldBundleGetPointer(bundle, 1, farrayPtr_1, _RC)
         call MAPL_FieldBundleGetPointer(bundle, 2, farrayPtr_2, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine state_get_vector_ptr_r4_2d

end module mapl_StateGetPointer_mod
