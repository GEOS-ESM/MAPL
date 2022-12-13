#include "MAPL_Generic.h"

module mapl3g_VirtualConnectionPt
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private
  
   public :: VirtualConnectionPt
   public :: ESMF_STATEINTENT_INTERNAL
   public :: operator(<)
   public :: operator(==)

   type(ESMF_StateIntent_Flag), parameter :: ESMF_STATEINTENT_INTERNAL = ESMF_StateIntent_Flag(100)
  
   type :: VirtualConnectionPt
      private
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name
      character(:), allocatable :: comp_name
   contains
      procedure :: get_state_intent
      procedure :: get_esmf_name
      procedure :: add_comp_name

      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal
      procedure :: to_string
   end type VirtualConnectionPt

   ! Constructors
   interface VirtualConnectionPt
      module procedure new_VirtualPt_basic
      module procedure new_VirtualPt_string_intent
   end interface VirtualConnectionPt

   interface operator(<)
      module procedure less_than
      module procedure less_than_esmf_stateintent
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

contains

   function new_VirtualPt_basic(state_intent, short_name) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      
      v_pt%state_intent = state_intent
      v_pt%short_name = short_name
      
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

   function add_comp_name(this, comp_name) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VirtualConnectionPt), intent(in) :: this
      character(*), intent(in) :: comp_name

      v_pt = this
      v_pt%comp_name = comp_name
      
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

      name = ''
      if (allocated(this%comp_name)) name = this%comp_name // '::'
      name = name // this%short_name
      
   end function get_esmf_name
      

   logical function less_than(lhs, rhs)
      type(VirtualConnectionPt), intent(in) :: lhs
      type(VirtualConnectionPt), intent(in) :: rhs

      less_than = lhs%state_intent < rhs%state_intent
      if (less_than) return

      ! If greater:
      if (rhs%state_intent < lhs%state_intent) return

      ! If intents are tied:
      less_than = lhs%get_esmf_name() < rhs%get_esmf_name()
      
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

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VirtualConnectionPt), intent(in) :: this

      s = "Virtual{intent: <" // this%get_state_intent() // ">, name: <" // this%get_esmf_name() //"> }"
   end function to_string
end module mapl3g_VirtualConnectionPt
