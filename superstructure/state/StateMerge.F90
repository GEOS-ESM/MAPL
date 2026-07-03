#include "MAPL.h"

! Merge two ESMF_State objects into one ESMF_State without allocating new field memory.
!
! All items (fields, field bundles, and nested states) from state_a and state_b are
! added by reference to the returned merged state.  The underlying field data is never
! copied; the merged state holds handles to the same objects that live in the two
! source states.
!
! Duplicate names:  when the same item name appears in both state_a and state_b the
! item from state_b replaces the one from state_a (ESMF_StateAddReplace semantics).

module mapl_StateMerge_mod
   use esmf
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   implicit none(type, external)
   private

   public :: StateMerge

   interface StateMerge
      procedure :: merge_states
   end interface StateMerge

contains

   ! Combine state_a and state_b into a single flat ESMF_State.
   !
   ! The result is a newly created ESMF_State that holds references to all items
   ! from both source states.  No field data is allocated or copied.  Items from
   ! state_b take precedence: if a name already exists in the merged state (because
   ! it was added from state_a) ESMF_StateAddReplace is used so that state_b's item
   ! wins.
   !
   ! The caller is responsible for eventually calling ESMF_StateDestroy on the
   ! returned merged state.  The source states and their contents are unaffected.
   !
   ! Arguments:
   !   state_a  -- first source state  (intent inout to satisfy ESMF_StateGet)
   !   state_b  -- second source state (intent inout to satisfy ESMF_StateGet)
   !   name     -- optional name for the merged state (default: 'merged')
   !   unusable -- keyword enforcer (forces keyword usage for subsequent args)
   !   rc       -- return code; ESMF_SUCCESS on success
   function merge_states(state_a, state_b, unusable, name, rc) result(merged)
      type(ESMF_State), intent(in) :: state_a
      type(ESMF_State), intent(in) :: state_b
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc
      type(ESMF_State) :: merged

      integer :: status
      character(len=ESMF_MAXSTR) :: name_

      name_ = 'merged'
      if (present(name)) name_ = name

      merged = ESMF_StateCreate(name=trim(name_), _RC)

      ! Add state_a items first (no replace needed — merged is empty)
      call add_items_to_state(merged, state_a, replace=.false., _RC)
      ! Add state_b items second; replace=.true. so state_b wins on conflicts
      call add_items_to_state(merged, state_b, replace=.true., _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end function merge_states


   ! Internal helper: iterate every item in src and add it to dst by reference.
   !
   ! When replace is .true., ESMF_StateAddReplace is used so that items in src
   ! silently overwrite any same-named items already present in dst.  When replace
   ! is .false., ESMF_StateAdd is used instead and ESMF will raise an error if a
   ! duplicate name is encountered.
   subroutine add_items_to_state(dst, src, replace, rc)
      type(ESMF_State), intent(inout) :: dst
      type(ESMF_State), intent(in)    :: src
      logical, intent(in) :: replace
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: item_count, i
      character(len=ESMF_MAXSTR), allocatable :: item_names(:)
      type(ESMF_StateItem_Flag), allocatable :: item_types(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_State) :: nested

      call ESMF_StateGet(src, itemCount=item_count, _RC)

      if (item_count == 0) then
         _RETURN(_SUCCESS)
      end if

      allocate(item_names(item_count), _STAT)
      allocate(item_types(item_count), _STAT)
      call ESMF_StateGet(src, &
           itemNameList=item_names, &
           itemTypeList=item_types, &
           itemOrderFlag=ESMF_ITEMORDER_ADDORDER, _RC)

      do i = 1, item_count

         if (item_types(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(src, trim(item_names(i)), field, _RC)
            if (replace) then
               call ESMF_StateAddReplace(dst, [field], _RC)
            else
               call ESMF_StateAdd(dst, [field], _RC)
            end if

         else if (item_types(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(src, trim(item_names(i)), bundle, _RC)
            if (replace) then
               call ESMF_StateAddReplace(dst, [bundle], _RC)
            else
               call ESMF_StateAdd(dst, [bundle], _RC)
            end if

         else if (item_types(i) == ESMF_STATEITEM_STATE) then
            call ESMF_StateGet(src, trim(item_names(i)), nested, _RC)
            if (replace) then
               call ESMF_StateAddReplace(dst, [nested], _RC)
            else
               call ESMF_StateAdd(dst, [nested], _RC)
            end if

         else
            _FAIL('Unsupported ESMF_StateItem type for item: ' // trim(item_names(i)))

         end if

      end do

      _RETURN(_SUCCESS)

   end subroutine add_items_to_state

end module mapl_StateMerge_mod
