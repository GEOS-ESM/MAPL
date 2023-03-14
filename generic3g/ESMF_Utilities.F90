module mapl3g_ESMF_Utilities
   use esmf
   implicit none
   private

   public :: write(formatted)

   interface write(formatted)
      procedure write_state
   end interface write(formatted)

contains


   subroutine write_state(state, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_State), intent(in) :: state
      integer, intent(in)         :: unit
      character(*), intent(in)    :: iotype
      integer, intent(in)         :: v_list (:)
      integer, intent(out)        :: iostat
      character(*), intent(inout) :: iomsg


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

      state = in_state

      call ESMF_StateGet(state, name=name, itemCount=itemCount, rc=status)
      if (status /= 0) then
         iostat = status
         iomsg = 'invalid state'
         return
      end if

      write(unit,*, iostat=iostat, iomsg=iomsg) indent(depth), 'State: ', trim(name),  ' has ', itemCount, 'items.', new_line('a')
      if (iostat /= 0) return

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
            type_str = 'ESMF_Field'
         elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            type_str = 'ESMF_FieldBundle'
         elseif (itemType == ESMF_STATEITEM_STATE) then
            type_str = 'ESMF_NestedState'
         else
            iostat = -1
            iomsg = 'unknown type of state item'
            return
         end if
            
         write(unit,*, iostat=iostat, iomsg=iomsg)indent(depth), i, ' ', trim(itemNameList(i)), ' ', type_str, new_line('a')
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
         indent = repeat('..', depth)
      end function indent
      
   end subroutine write_state_
   
end module mapl3g_ESMF_Utilities
