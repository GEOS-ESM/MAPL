#include "MAPL_Generic.h"

module mapl3g_ESMF_Utilities
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: write(formatted)
   public :: get_substate
   public :: to_esmf_state_intent
   public :: esmf_state_intent_to_string
   public :: MAPL_TYPEKIND_MIRROR

   type(ESMF_TypeKind_Flag), parameter :: MAPL_TYPEKIND_MIRROR = ESMF_TypeKind_Flag(200)

   interface write(formatted)
      procedure write_state
   end interface write(formatted)

contains


   subroutine write_state(in_state, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_State), intent(in) :: in_state
      integer, intent(in)         :: unit
      character(*), intent(in)    :: iotype
      integer, intent(in)         :: v_list (:)
      integer, intent(out)        :: iostat
      character(*), intent(inout) :: iomsg

      type(ESMF_State) :: state
      integer :: status
      character(ESMF_MAXSTR) :: name
      integer :: itemCount

      state = in_state

      call ESMF_StateGet(state, name=name, itemCount=itemCount, rc=status)
      if (status /= 0) then
         iostat = status
         iomsg = 'invalid state'
         return
      end if

      write(unit,'(a,a,a,i0,a,a)',iostat=iostat, iomsg=iomsg) 'State: ', trim(name),  ' has ', itemCount, ' items.', new_line('a')
      if (iostat /=0) return

      call write_state_(state, unit, iotype, v_list, iostat, iomsg, depth=0)

   end subroutine write_state

   recursive subroutine write_state_(in_state, unit, iotype, v_list, iostat, iomsg, depth)
      type(ESMF_State), intent(in) :: in_state
      integer, intent(in)         :: unit
      character(*), intent(in)    :: iotype
      integer, intent(in)         :: v_list (:)
      integer, intent(out)        :: iostat
      character(*), intent(inout) :: iomsg
      integer, intent(in) :: depth


      type(ESMF_State) :: state
      integer :: itemCount
      character(len=ESMF_MAXSTR) :: name
      character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
      type(ESMF_StateItem_Flag) :: itemType
      integer :: status
      integer :: i
      character(:), allocatable :: type_str
      type(ESMF_State) :: substate

      iostat = 0 ! unless
      state = in_state

      call ESMF_StateGet(state, name=name, itemCount=itemCount, rc=status)
      if (status /= 0) then
         iostat = status
         iomsg = 'invalid state'
         return
      end if

      allocate(itemNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=itemNameList, rc=status)
      if (status /= 0) then
         iostat = status
         iomsg = 'invalid state'
         return
      end if
      do i = 1, itemCount
         call ESMF_StateGet(state, itemName=trim(itemNameList(i)), itemType=itemType, rc=status)
         if (status /= 0) then
            iostat = status
            iomsg = 'invalid state'
            return
         end if
         if (itemType == ESMF_STATEITEM_FIELD) then
            type_str = 'Field'
         elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            type_str = 'Bundle'
         elseif (itemType == ESMF_STATEITEM_STATE) then
            type_str = 'State'
         else
            iostat = -1
            iomsg = 'unknown type of state item'
            return
         end if

         write(unit,'(a,a8,4x,a,a1)', iostat=iostat, iomsg=iomsg) indent(depth+1),  type_str, trim(itemNameList(i)), new_line('a')
         if (iostat /= 0) return

         if (itemType == ESMF_STATEITEM_STATE) then
            call ESMF_StateGet(state, trim(itemNameList(i)), substate, rc=status)
            if (status /= 0) then
               iostat = status
               iomsg = 'could not retrieve substate'
               return
            end if

            call write_state_(substate, unit, iotype, v_list, iostat, iomsg, depth=depth+1)
            if (iostat /= 0) return
         end if
      end do

   contains

      function indent(depth)
         character(:), allocatable :: indent
         integer, intent(in) :: depth
         indent = repeat('......', depth)
      end function indent

   end subroutine write_state_

   ! Traverse nested states to return the innermost substate specified by path.
   ! Intermediate states are created if they do not exist.
   subroutine get_substate(state, path, substate, rc)
      use mapl_ErrorHandling
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: path
      type(ESMF_State), intent(out) :: substate
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      character(:), allocatable :: substate_name, current_path
      type(ESMF_State) :: tmp_state
      integer :: idx

      substate = state
      if (path == '') then ! no substate
         _RETURN(_SUCCESS)
      end if

      current_path = path
      do while (path /= '')
         idx = index(current_path, '/')
         substate_name = current_path
         if (idx > 0) then
            substate_name = current_path(:idx-1)
         end if

         call ESMF_StateGet(substate, substate_name, itemType, _RC)
         
         if (itemType == ESMF_STATEITEM_NOTFOUND) then ! New tmp_state
            tmp_state = ESMF_StateCreate(name=substate_name, _RC)
            call ESMF_StateAdd(substate, [tmp_state], _RC)
         else
            _ASSERT(itemType == ESMF_STATEITEM_STATE, 'expected ' // substate_name // ' to be an ESMF_State.')
            call ESMF_StateGet(substate, substate_name, tmp_state, _RC)
         end if
         substate = tmp_state
         if (idx == 0) exit
         current_path = current_path(idx+1:)
      end do


      _RETURN(_SUCCESS)
   end subroutine get_substate


   function to_esmf_state_intent(str_state_intent, rc) result(state_intent)
      type(ESMF_StateIntent_Flag) :: state_intent
      character(*), intent(in) :: str_state_intent
      integer, optional, intent(out) :: rc

      select case (str_state_intent)
      case ('import')
         state_intent = ESMF_STATEINTENT_IMPORT
      case ('export')
         state_intent = ESMF_STATEINTENT_EXPORT
      case ('internal')
         state_intent = ESMF_STATEINTENT_INTERNAL
      case default
         state_intent = ESMF_STATEINTENT_INVALID
         _FAIL('invalid state intent: ' // str_state_intent)
      end select

      _RETURN(_SUCCESS)
   end function to_esmf_state_intent

   function esmf_state_intent_to_string(state_intent, rc) result(state_intent_str)
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      integer, optional, intent(out) :: rc
      character(:), allocatable :: state_intent_str ! result

      if (state_intent==ESMF_STATEINTENT_IMPORT) then
         state_intent_str = "import"
      else if (state_intent==ESMF_STATEINTENT_EXPORT) then
         state_intent_str = "export"
      else if (state_intent==ESMF_STATEINTENT_INTERNAL) then
         state_intent_str = "internal"
      else
         _FAIL("invalid state intent")
      end if

      _RETURN(_SUCCESS)
   end function esmf_state_intent_to_string

end module mapl3g_ESMF_Utilities
