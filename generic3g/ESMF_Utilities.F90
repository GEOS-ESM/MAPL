#include "MAPL_Generic.h"

module mapl3g_ESMF_Utilities
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: write(formatted)
   public :: get_substate
   public :: to_esmf_state_intent
   public :: MAPL_TYPEKIND_MIRROR
   public :: MAPL_HConfigMatch

   type(ESMF_TypeKind_Flag), parameter :: MAPL_TYPEKIND_MIRROR = ESMF_TypeKind_Flag(200)

   interface write(formatted)
      procedure write_state
      procedure write_hconfig
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

   ! If name is empty string then return the existing state.
   ! Otherwise, return the named substate; creating it if it does
   ! not already exist.
   subroutine get_substate(state, name, substate, rc)
      use mapl_ErrorHandling
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: name
      type(ESMF_State), intent(out) :: substate
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      character(:), allocatable :: substate_name

      if (name == '') then ! no substate
         substate = state
         _RETURN(_SUCCESS)
      end if

!!$      substate_name = '[' // name // ']'
      substate_name = name
      call ESMF_StateGet(state, substate_name, itemType, _RC)

      if (itemType == ESMF_STATEITEM_NOTFOUND) then ! New substate
         substate = ESMF_StateCreate(name=substate_name, _RC)
         call ESMF_StateAdd(state, [substate], _RC)
         _RETURN(_SUCCESS)
      end if

      _ASSERT(itemType == ESMF_STATEITEM_STATE, 'incorrect object in state')

      ! Substate exists so ...
      call ESMF_StateGet(state, substate_name, substate, _RC)

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

   subroutine write_hconfig(hconfig, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg


      call write_hconfig_recursive(hconfig, unit, iotype, v_list, iostat, iomsg)
      
   end subroutine write_hconfig

   recursive subroutine write_hconfig_recursive(hconfig, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      logical :: is_mapping, is_sequence, is_scalar

      iostat = 0 ! unless

      is_mapping = ESMF_HConfigIsMap(hconfig, rc=iostat)
      if (iostat /= 0) return

      if (is_mapping) then
         call write_mapping(hconfig, unit, iotype, v_list, iostat, iomsg)
         return
      end if

      is_sequence = ESMF_HConfigIsSequence(hconfig, rc=iostat)
      if (iostat /= 0) return

      if (is_sequence) then
         call write_sequence(hconfig, unit, iotype, v_list, iostat, iomsg)
         return
      end if

      is_scalar = ESMF_HConfigIsScalar(hconfig, rc=iostat)
      if (iostat /= 0) return

      if (is_scalar) then
         call write_scalar(hconfig, unit, iotype, v_list, iostat, iomsg)
         return
      end if

      iostat = 0 ! Illegal node type
   end subroutine write_hconfig_recursive

   recursive subroutine write_mapping(hconfig, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: val_hconfig
      character(:), allocatable :: key
      logical :: first

      iostat = 0 ! unless

      write(unit, '("{")', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return
      iter_begin = ESMF_HConfigIterBegin(hconfig, rc=iostat)
      if (iostat /= 0) return
      iter_end = ESMF_HConfigIterEnd(hconfig, rc=iostat)
      if (iostat /= 0) return
      iter = iter_begin
      
      first = .true.
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=iostat))
         if (iostat /= 0) return
         
         key = ESMF_HConfigAsStringMapKey(iter, rc=iostat)
         if (iostat /= 0) return
         
         if (.not. first) then
            write(unit, '(", ")', iostat=iostat, iomsg=iomsg)
            if (iostat /= 0) return
         end if
         first =.false.
         write(unit, '(a,a)', iostat=iostat, iomsg=iomsg) key, ': '
         if (iostat /= 0) return
         
         val_hconfig = ESMF_HConfigCreateAtMapVal(iter, rc=iostat)
         if (iostat /= 0) return
         
         call write_hconfig_recursive(val_hconfig, unit, iotype, v_list, iostat, iomsg)
         if (iostat /= 0) return
         
         call ESMF_HConfigDestroy(val_hconfig, rc=iostat)
         if (iostat /= 0) return
         
      end do
      write(unit, '("}")', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return
      
      end subroutine write_mapping

   recursive subroutine write_sequence(hconfig, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: val_hconfig
      logical :: first

      iostat = 0 ! unless
      write(unit, '("[")', iostat=iostat, iomsg=iomsg)

      iter_begin = ESMF_HConfigIterBegin(hconfig, rc=iostat)
      if (iostat /= 0) return
      iter_end = ESMF_HConfigIterEnd(hconfig, rc=iostat)
      if (iostat /= 0) return
      iter = iter_begin
      first = .true.
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=iostat))
         if (iostat /= 0) return
         
         if (.not. first) then
            write(unit, '(", ")', iostat=iostat, iomsg=iomsg)
            if (iostat /= 0) return
         end if
         first =.false.
         
         val_hconfig = ESMF_HConfigCreateAt(iter, rc=iostat)
         if (iostat /= 0) return
         call write_hconfig_recursive(val_hconfig, unit, iotype, v_list, iostat, iomsg)
         if (iostat /= 0) return
         call ESMF_HConfigDestroy(val_hconfig, rc=iostat)
         if (iostat /= 0) return
         
      end do

      write(unit, '("]")', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return
      
   end subroutine write_sequence
   
   recursive subroutine write_scalar(hconfig, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      character(:), allocatable :: str
   
      iostat = 0 ! unless

      str = ESMF_HConfigAsString(hconfig, rc=iostat)
      if (iostat /= 0) return
      write(unit, '(a)', iostat=iostat, iomsg=iomsg)  str
      if (iostat /= 0) return
            
   end subroutine write_scalar


   recursive logical function MAPL_HConfigMatch(a, b, rc) result(match)
      type(ESMF_HConfig), intent(in) :: a, b
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: a_type, b_type

      match = .false. ! unless

      a_type = get_hconfig_type(a, _RC)
      b_type = get_hconfig_type(b, _RC)

      if (a_type /= b_type) then
         _RETURN(_SUCCESS)
      end if

      if (a_type == 'MAPPING') then
         match = MAPL_HConfigMatchMapping(a, b, rc)
      else if (a_type == 'SEQUENCE') then
         match = MAPL_HConfigMatchSequence(a, b, rc)
      else if (a_type == 'SCALAR') then
         match = MAPL_HConfigMatchScalar(a, b, rc)
      else
         _FAIL('unsupported HConfig type.')
      end if
      
      match = .true. 

      _RETURN(_SUCCESS)
   end function MAPL_HConfigMatch

   function get_hconfig_type(hconfig, rc) result(hconfig_type)
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: hconfig_type
      logical :: is_scalar
      logical :: is_sequence
      logical :: is_mapping

      is_scalar = ESMF_HConfigIsScalar(hconfig, _RC)
      if (is_scalar) then
         hconfig_type = 'SCALAR'
         _RETURN(_SUCCESS)
      end if
      
      is_sequence = ESMF_HConfigIsSequence(hconfig, _RC)
      if (is_scalar) then
         hconfig_type = 'SEQUENCE'
         _RETURN(_SUCCESS)
      end if

      is_mapping = ESMF_HConfigIsMap(hconfig, _RC)
      if (is_scalar) then
         hconfig_type = 'MAPPING'
         _RETURN(_SUCCESS)
      end if

      hconfig_type = 'UNKNOWN'
      _FAIL('unsupported HConfig type.')

      _RETURN(_SUCCESS)
   end function get_hconfig_type

   recursive logical function MAPL_HConfigMatchScalar(a, b, rc) result(match)
      type(ESMF_HConfig), intent(in) :: a, b
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: a_str, b_str

      match = .false. ! unless

      a_str = ESMF_HConfigAsString(a, _RC)
      b_str = ESMF_HConfigAsString(b, _RC)

      match = (a_str == b_str)

      _RETURN(_SUCCESS)
   end function MAPL_HConfigMatchScalar


   recursive logical function MAPL_HConfigMatchSequence(a, b, rc) result(match)
      type(ESMF_HConfig), intent(in) :: a, b
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: a_val_hconfig, b_val_hconfig
      integer :: i
      integer :: a_size, b_size

      match = .false. ! unless

      a_size = ESMF_HConfigGetSize(a, _RC)
      b_size = ESMF_HConfigGetSize(b, _RC)

      if (a_size /= b_size) then
         _RETURN(_SUCCESS)
      end if

      do i = 1, a_size

         a_val_hconfig = ESMF_HConfigCreateAt(a, index=i, _RC)
         b_val_hconfig = ESMF_HConfigCreateAt(b, index=i, _RC)

         match = MAPL_HConfigMatch(a_val_hconfig, b_val_hconfig, _RC)

         call ESMF_HConfigDestroy(a_val_hconfig, _RC)
         call ESMF_HConfigDestroy(b_val_hconfig, _RC)

         if (.not. match) then
            _RETURN(_SUCCESS)
         end if
      end do

      match = .true.

      _RETURN(_SUCCESS)
   end function MAPL_HConfigMatchSequence

   recursive logical function MAPL_HConfigMatchMapping(a, b, rc) result(match)
      type(ESMF_HConfig), intent(in) :: a, b
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: a_val_hconfig, b_val_hconfig
      character(:), allocatable :: key
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      integer :: a_size, b_size

      match = .false. ! unless

      a_size = ESMF_HConfigGetSize(a, _RC)
      b_size = ESMF_HConfigGetSize(b, _RC)

      if (a_size /= b_size) then
         _RETURN(_SUCCESS)
      end if

      iter_begin = ESMF_HConfigIterBegin(a, _RC)
      iter_end = ESMF_HConfigIterEnd(a, _RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)

         key = ESMF_HConfigAsStringMapKey(iter, _RC)
         
         a_val_hconfig = ESMF_HConfigCreateAt(a, keyString=key, _RC)
         b_val_hconfig = ESMF_HConfigCreateAt(b, keyString=key, _RC)

         match = MAPL_HConfigMatch(a_val_hconfig, b_val_hconfig, _RC)

         call ESMF_HConfigDestroy(a_val_hconfig, _RC)
         call ESMF_HConfigDestroy(b_val_hconfig, _RC)

         if (.not. match) then
            _RETURN(_SUCCESS)
         end if
      end do

      match = .true.

      _RETURN(_SUCCESS)
   end function MAPL_HConfigMatchMapping


end module mapl3g_ESMF_Utilities
