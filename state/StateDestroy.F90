#include "MAPL.h"
#include "unused_dummy.H"

module mapl3g_StateDestroy
   use esmf
   use MAPL_FieldUtils, only: FieldsDestroy
   use mapl3g_FieldBundleDestroy
   use MAPL_ExceptionHandling
   use mapl_KeywordEnforcer
   implicit none(type, external)

   !private
   public :: MAPL_StateDestroy

   interface MAPL_StateDestroy
      procedure :: destroy_state
   end interface MAPL_StateDestroy
   
   logical, parameter :: NESTED = .TRUE.

contains
      
!================================= ESMF_STATE ==================================

   subroutine destroy_state(state, unusable, destroy_contents, rc)
      type(ESMF_State), intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: destroy_contents
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: destroying_contents

      destroying_contents = .FALSE.
      if(present(destroy_contents)) destroying_contents = destroy_contents
      if(destroying_contents) then
         call destroy_state_contents(state, _RC)
      end if
      call ESMF_StateDestroy(state, _RC)
      call ESMF_StateValidate(state, rc=status)
      _ASSERT(status /= ESMF_SUCCESS, 'The state was not destroyed successfully.')
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine destroy_state

   subroutine destroy_state_contents(state, rc)
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_StateItem_Flag), allocatable :: types(:)
      character(len=ESMF_MAXSTR), allocatable :: names(:)
      integer :: itemCount
      type(ESMF_Field), allocatable :: fields(:)
      type(ESMF_FieldBundle), allocatable :: bundles(:)
      type(ESMF_State), allocatable :: nested_states(:)

      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCount, _RC)
      allocate(types(itemCount))
      allocate(names(itemCount))
      call ESMF_StateGet(state, nestedFlag=NESTED, itemTypeList=types, itemNameList=names, _RC)

      call remove_state_fields(state, pack(names, types == ESMF_STATEITEM_FIELD), fields, _RC)
      call FieldsDestroy(fields, _RC)

      call remove_bundles(state, pack(names, types == ESMF_STATEITEM_FIELDBUNDLE), bundles, _RC)
      call destroy_bundles(bundles, _RC)

      call remove_nested_states(state, pack(names, types == ESMF_STATEITEM_STATE), nested_states, _RC)
      call destroy_states(nested_states, _RC)

      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCount, _RC)
      _ASSERT(itemCount == 0, 'Some MAPL_StateItems remain in state.')
      _RETURN(_SUCCESS)

   end subroutine destroy_state_contents

   subroutine remove_state_fields(state, names, fields, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=ESMF_MAXSTR), intent(in) :: names(:)
      type(ESMF_Field), allocatable, intent(inout) :: fields(:)
      integer, optional, intent(out) :: rc
      integer :: status, i, itemCount, itemCountAfter

      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCount, _RC)
      allocate(fields(size(names)))
      do i=1, size(fields)
         call ESMF_StateGet(state, names(i), fields(i), _RC)
      end do
      call ESMF_StateRemove(state, itemNameList=names, _RC)
      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCountAfter, _RC)
      _ASSERT(itemCountAfter == itemCount - size(fields), 'Some fields were not removed.')
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)

   end subroutine remove_state_fields

   subroutine remove_bundles(state, names, bundles, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=ESMF_MAXSTR), intent(in) :: names(:)
      type(ESMF_FieldBundle), allocatable, intent(inout) :: bundles(:)
      integer, optional, intent(out) :: rc
      integer :: status, i, itemCount, itemCountAfter

      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCount, _RC)
      allocate(bundles(size(names)))
      do i=1, size(bundles)
         call ESMF_StateGet(state, names(i), bundles(i), _RC)
      end do
      call ESMF_StateRemove(state, itemNameList=names, _RC)
      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCountAfter, _RC)
      _ASSERT(itemCountAfter == itemCount - size(bundles), 'Some bundles were not removed.')
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)

   end subroutine remove_bundles

   subroutine remove_nested_states(state, names, states, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=ESMF_MAXSTR), intent(in) :: names(:)
      type(ESMF_State), allocatable, intent(inout) :: states(:)
      integer, optional, intent(out) :: rc
      integer :: status, i, itemCount, itemCountAfter

      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCount, _RC)
      allocate(states(size(names)))
      do i=1, size(states)
         call ESMF_StateGet(state, names(i), states(i), _RC)
      end do
      call ESMF_StateRemove(state, itemNameList=names, _RC)
      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCountAfter, _RC)
      _ASSERT(itemCountAfter == itemCount - size(states), 'Some states were not removed.')
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)

   end subroutine remove_nested_states
     
   subroutine destroy_states(states, rc)
      type(ESMF_State), intent(inout) :: states(:)
      integer, optional, intent(out) :: rc
      integer :: status, i
      character(len=ESMF_MAXSTR) :: name

      do i=1, size(states)
         call ESMF_StateGet(states(i), name=name, _RC)
         call ESMF_StateDestroy(states(i), _RC)
         call ESMF_StateValidate(states(i), rc=status)
         _ASSERT(status /= ESMF_SUCCESS, 'State "' // trim(name) // '" was not destroyed.')
      end do
      _RETURN(_SUCCESS)
         
   end subroutine destroy_states

!============================== ESMF_FieldBundle ===============================

   subroutine destroy_bundles(bundles, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundles(:)
      integer, optional, intent(out) :: rc
      integer :: status, i
      
      do i=1, size(bundles)
         call FieldBundleDestroy(bundles(i), _RC)
      end do
      _RETURN(_SUCCESS)
     
   end subroutine destroy_bundles

!   subroutine destroy_bundle(bundle, rc)
!      type(ESMF_FieldBundle), intent(inout) :: bundle
!      integer, optional, intent(out) :: rc
!      integer :: status
!      type(ESMF_Field), allocatable :: fieldList(:)
!      character(len=ESMF_MAXSTR) :: name

!      call remove_bundle_fields(bundle, fieldList, _RC)
!      call destroy_fields(fieldList, _RC)
!      call ESMF_FieldBundleGet(bundle, name=name, _RC)
!      call ESMF_FieldBundleDestroy(bundle, _RC)
!      call ESMF_FieldBundleValidate(bundle, rc=status)
!      _ASSERT(status /= ESMF_SUCCESS, 'Bundle "' // trim(name) // '" was not destroyed.')
!      _RETURN(_SUCCESS)

!   end subroutine destroy_bundle

!   subroutine remove_bundle_fields(bundle, fields, rc)
!      type(ESMF_FieldBundle), intent(inout) :: bundle
!      type(ESMF_Field), allocatable, intent(inout) :: fields(:)
!      integer, optional, intent(out) :: rc
!      integer :: status, fieldCount
!      character(len=ESMF_MAXSTR), allocatable :: fieldNameList(:)

!      call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
!      allocate(fields(fieldCount))
!      allocate(fieldNameList(fieldCount))
!      call ESMF_FieldBundleGet(bundle, fieldList=fields, fieldNameList=fieldNameList, _RC)
!      call ESMF_FieldBundleRemove(bundle, fieldNameList=fieldNameList, _RC)
!      call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
!      _ASSERT(fieldCount == 0, 'Some fields were not removed.')
!      _RETURN(_SUCCESS)

!   end subroutine remove_bundle_fields

!================================= ESMF_Field ==================================

!   subroutine destroy_fields(fields, rc)
!      type(ESMF_Field), intent(inout) :: fields(:)
!      integer, optional, intent(out) :: rc
!      integer :: status, i
!      character(len=ESMF_MAXSTR) :: name

!      do i=1, size(fields)
!         call ESMF_FieldGet(fields(i), name=name, _RC)
!         call ESMF_FieldDestroy(fields(i), _RC)
!         call ESMF_FieldValidate(fields(i), rc=status)
!         _ASSERT(status /= ESMF_SUCCESS, 'Field "' // trim(name) // '" was not destroyed.')
!      end do
         
!   end subroutine destroy_fields

end module mapl3g_StateDestroy
