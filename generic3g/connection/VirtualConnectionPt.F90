#include "MAPL.h"

module mapl3g_VirtualConnectionPt
   use mapl_KeywordEnforcer
   use esmf
   use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
   implicit none
   private

   public :: VirtualConnectionPt
   public :: operator(<)
   public :: operator(==)
   public :: operator(/=)

   type :: VirtualConnectionPt
      private
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name
      character(:), allocatable :: comp_name
   contains
      procedure :: get_state_intent
      procedure :: get_esmf_name
      procedure :: get_full_name
      procedure :: get_comp_name

      procedure :: add_comp_name

      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal

      procedure :: matches

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VirtualConnectionPt

   ! Constructors
   interface VirtualConnectionPt
      procedure new_VirtualPt_basic
      procedure new_VirtualPt_string_intent
      procedure new_VirtualPt_substate
   end interface VirtualConnectionPt

   interface operator(<)
      module procedure less_than
      module procedure less_than_esmf_stateintent
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)

contains

   function new_VirtualPt_basic(state_intent, short_name, unusable, comp_name) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: comp_name

      v_pt%state_intent = state_intent
      v_pt%short_name = short_name
      if (present(comp_name)) then
         if (comp_name /= '') v_pt%comp_name = comp_name
      end if

      _UNUSED_DUMMY(unusable)
    end function new_VirtualPt_basic

   ! Must use keyword association for this form due to ambiguity of argument ordering
   function new_VirtualPt_string_intent(unusable, state_intent, short_name) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name

      type(ESMF_StateIntent_flag) :: stateintent

      select case (state_intent)
      case ('import')
         stateintent = ESMF_STATEINTENT_IMPORT
      case ('export')
         stateintent = ESMF_STATEINTENT_EXPORT
      case ('internal')
         stateintent = ESMF_STATEINTENT_INTERNAL
      case default
         stateintent = ESMF_STATEINTENT_INVALID
      end select

      v_pt = VirtualConnectionPt(stateintent, short_name)

      _UNUSED_DUMMY(unusable)
   end function new_VirtualPt_string_intent

   function new_VirtualPt_substate(v_pt, comp_name) result(new_v_pt)
      type(VirtualConnectionPt) :: new_v_pt
      type(VirtualConnectionPt), intent(in) :: v_pt
      character(*), intent(in) :: comp_name

      new_v_pt = VirtualConnectionPt(v_pt%state_intent, v_pt%short_name, comp_name=comp_name)

   end function new_VirtualPt_substate

   function add_comp_name(this, comp_name) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VirtualConnectionPt), intent(in) :: this
      character(*), intent(in) :: comp_name

      v_pt = this
      if (.not. allocated(v_pt%comp_name)) v_pt%comp_name = comp_name

   end function add_comp_name

   function get_state_intent(this) result(state_intent)
      character(:), allocatable :: state_intent
      class(VirtualConnectionPt), intent(in) :: this

      select case (this%state_intent%state)
      case (ESMF_STATEINTENT_IMPORT%state)
         state_intent = 'import'
      case (ESMF_STATEINTENT_EXPORT%state)
         state_intent = 'export'
      case (ESMF_STATEINTENT_INTERNAL%state)
         state_intent = 'internal'
      case default
         state_intent = '<invalid>'
      end select
   end function get_state_intent


   ! Important that name is different if either comp_name or short_name differ
   function get_esmf_name(this) result(name)
      character(:), allocatable :: name
      class(VirtualConnectionPt), intent(in) :: this

      name = this%short_name

   end function get_esmf_name

   ! Important that name is different if either comp_name or short_name differ
   function get_full_name(this) result(name)
      character(:), allocatable :: name
      class(VirtualConnectionPt), intent(in) :: this

      name = this%short_name
      if (allocated(this%comp_name)) name = this%comp_name // '/' // name

   end function get_full_name

   function get_comp_name(this) result(name)
      character(:), allocatable :: name
      class(VirtualConnectionPt), intent(in) :: this
      name = ''
      if (allocated(this%comp_name)) name = this%comp_name
   end function get_comp_name


   logical function less_than(lhs, rhs)
      type(VirtualConnectionPt), intent(in) :: lhs
      type(VirtualConnectionPt), intent(in) :: rhs

      less_than = lhs%state_intent < rhs%state_intent
      if (less_than) return

      ! If greater:
      if (rhs%state_intent < lhs%state_intent) return

      ! If intents are tied:
      less_than = lhs%get_full_name() < rhs%get_full_name()

   end function less_than

   logical function less_than_esmf_stateintent(lhs, rhs) result(less_than)
      type(Esmf_StateIntent_Flag), intent(in) :: lhs
      type(Esmf_StateIntent_Flag), intent(in) :: rhs

      less_than = lhs%state < rhs%state
   end function less_than_esmf_stateintent

   logical function equal_to(lhs, rhs)
      type(VirtualConnectionPt), intent(in) :: lhs
      type(VirtualConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))

   end function equal_to

   logical function not_equal_to(lhs, rhs)
      type(VirtualConnectionPt), intent(in) :: lhs
      type(VirtualConnectionPt), intent(in) :: rhs

      not_equal_to = .not. (lhs == rhs)

   end function not_equal_to

   logical function is_import(this)
      class(VirtualConnectionPt), intent(in) :: this
      is_import = (this%get_state_intent() == 'import')
   end function is_import

   logical function is_export(this)
      class(VirtualConnectionPt), intent(in) :: this
      is_export = (this%get_state_intent() == 'export')
   end function is_export

   logical function is_internal(this)
      class(VirtualConnectionPt), intent(in) :: this
      is_internal = (this%get_state_intent() == 'internal')
   end function is_internal

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(VirtualConnectionPt), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg


      write(unit, '("Virtual{intent: <",a,">, name: <",a,">}")', iostat=iostat, iomsg=iomsg) &
           this%get_state_intent(), this%get_full_name()

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   logical function matches(this, item)
      use regex_module
      class(VirtualConnectionPt), intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: item

      type(regex_type) :: regex

      matches = (this%get_state_intent() == item%get_state_intent())
      if (.not. matches) return

      call regcomp(regex,'^'//this%get_full_name()//'$' // C_NULL_CHAR,flags='xmi')
      matches = regexec(regex,item%get_full_name() // C_NULL_CHAR)

   end function matches

end module mapl3g_VirtualConnectionPt
