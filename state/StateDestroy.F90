#include "MAPL.h"
#include "unused_dummy.H"

module mapl3g_StateDestroy
   use mapl3g_StateItem
   use esmf
   use MAPL_ExceptionHandling
   use mapl_KeywordEnforcer
   implicit none(type, external)

   private
   public :: MAPL_StateDestroy

   interface MAPL_StateDestroy
      procedure :: destroy_state
   end interface MAPL_StateDestroy

   interface destroy
      procedure :: destroy_fields
      procedure :: destroy_bundle
      procedure :: destroy_bundles
      procedure :: destroy_states
      procedure :: destroy_items
   end interface destroy
   
   interface remove
      procedure :: remove_state_fields
      procedure :: remove_bundle_fields
      procedure :: remove_bundles
      procedure :: remove_states
   end interface remove

contains
      
   subroutine destroy_state(state, unusable, recurse, rc)
      type(ESMF_State), intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: recurse
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: doing_recursion

      doing_recursion = .FALSE.
      if(present(recurse)) doing_recursion = recurse
      if(doing_recursion) then
         call destroy(state, _RC)
      end if
      call ESMF_StateDestroy(state, _RC)
      _RETURN(_SUCCESS)

   end subroutine destroy_state

   subroutine destroy_items(state, rc)
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_StateItem_Flag), allocatable :: types(:)
      character(len=ESMF_MAXSTR), allocatable :: names(:)
      integer :: itemCount, i
      type(ESMF_Field), allocatable :: fields(:)
      type(ESMF_FieldBundle), allocatable :: bundles(:)
      type(ESMF_State), allocatable :: states(:)
      logical, parameter :: NESTED = .TRUE.
      integer :: number_removed

      call ESMF_StateGet(state, nestedFlag=NESTED, itemCount=itemCount, _RC)
      allocate(types(itemCount))
      allocate(names(itemCount))
      call ESMF_StateGet(state, nestedFlag=NESTED, itemTypeList=types, itemNameList=names, _RC)
      number_removed = 0

      call remove(state, pack(names, types == MAPL_STATEITEM_FIELD), fields, _RC)
      number_removed = number_removed + size(fields)
      call destroy(fields, _RC)

      call remove(state, pack(names, types == MAPL_STATEITEM_FIELDBUNDLE), bundles, _RC)
      number_removed = number_removed + size(bundles)
      call destroy(bundles, _RC)

      call remove(state, pack(names, types == MAPL_STATEITEM_STATE), states, _RC)
      number_removed = number_removed + size(states)
      call destroy(states, _RC)

      _ASSERT(number_removed == itemCount, 'Some MAPL_StateItems remain in state.')
      call ESMF_StateDestroy(state, _RC)
      _RETURN(_SUCCESS)

   end subroutine destroy_items

   subroutine remove_bundle_fields(bundle, fields, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_Field), allocatable, intent(inout) :: fields(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: fieldCount
      character(len=ESMF_MAXSTR), allocatable :: fieldNameList(:)

      call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, _RC)
      allocate(fields(fieldCount))
      allocate(fieldNameList(fieldCount))
      call ESMF_FieldBundleGet(bundle, fieldList=fields, _RC)
      call ESMF_FieldBundleRemove(bundle, fieldNameList=fieldNameList, _RC)
      _RETURN(_SUCCESS)

   end subroutine remove_bundle_fields

   subroutine remove_state_fields(state, names, fields, rc)
      type(ESMF_State), intent(in) :: state
      character(len=ESMF_MAXSTR), intent(in) :: names(:)
      type(ESMF_Field), allocatable, intent(inout) :: fields(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i

      allocate(fields(size(names)))
      do i=1, size(fields)
         call ESMF_StateGet(state, names(i), fields(i), _RC)
      end do
      call ESMF_StateRemove(state, itemNameList=names, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)

   end subroutine remove_state_fields

   subroutine remove_bundles(state, names, bundles, rc)
      type(ESMF_State), intent(in) :: state
      character(len=ESMF_MAXSTR), intent(in) :: names(:)
      type(ESMF_FieldBundle), allocatable, intent(inout) :: bundles(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i

      allocate(bundles(size(names)))
      do i=1, size(bundles)
         call ESMF_StateGet(state, names(i), bundles(i), _RC)
      end do
      call ESMF_StateRemove(state, itemNameList=names, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)

   end subroutine remove_bundles

   subroutine remove_states(state, names, states, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=ESMF_MAXSTR), intent(in) :: names(:)
      type(ESMF_State), allocatable, intent(inout) :: states(:)
      integer, optional, intent(out) :: rc
      integer :: status

      allocate(states(size(names)))
      do i=1, size(states)
         call ESMF_StateGet(state, names(i), states(i), _RC)
      end do
      call ESMF_StateRemove(state, itemNameList=names, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)

   end subroutine remove_states
     
   subroutine destroy_bundles(bundles, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundles(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      
      do i=1, size(bundles)
         call destroy(bundles(i), _RC)
      end do
      _RETURN(_SUCCESS)
     
   end subroutine destroy_bundles

   subroutine destroy_bundle(bundle, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: fieldCount
      type(ESMF_Field), allocatable :: fieldList(:)

      call remove(bundle, fieldList, _RC)
      call destroy(fieldList, _RC)
      call ESMF_FieldBundleDestroy(bundle, _RC)
      _RETURN(_SUCCESS)

   end subroutine destroy_bundle

   subroutine destroy_fields(fields, rc)
      type(ESMF_Field), intent(inout) :: fields(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i

      do i=1, size(fields)
         call ESMF_FieldDestroy(fields(i), _RC)
      end do
         
   end subroutine destroy_fields

   subroutine destroy_states(states, rc)
      type(ESMF_State), intent(inout) :: states(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i

      do i=1, size(states)
         call ESMF_State(states(i), _RC)
      end do
         
   end subroutine destroy_states

end module mapl3g_StateDestroy
