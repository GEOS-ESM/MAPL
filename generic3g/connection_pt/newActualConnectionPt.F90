#include "MAPL_Generic.h"

module mapl3g_newActualConnectionPt
   use mapl3g_newVirtualConnectionPt
   use mapl_KeywordEnforcer
   implicit none
   private
  
   public :: newActualConnectionPt
   public :: operator(<)
   public :: operator(==)

   ! Note: The design intentioally does not have ActualConnectionPt
   ! inherit from VirtualConnectionPt in order to allow for future
   ! subclasses of VirtualConnectionPt in some interfaces while not
   ! permitting ActualConnectionPt objects.  A potential refactoring
   ! would be instead to have both classes inherit from a single
   ! obstract ConnectionPt class.  TBD

   type :: newActualConnectionPt
      private
      type(newVirtualConnectionPt) :: v_pt
      integer, allocatable :: label
   contains
      procedure :: extend

      procedure :: get_state_intent
      procedure :: get_esmf_name
      procedure :: add_comp_name

      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal

      procedure :: is_extension
      procedure :: get_extension_string
      procedure :: to_string
      
   end type newActualConnectionPt

   ! Constructors
   interface newActualConnectionPt
      module procedure new_newActualPt_from_v_pt
      module procedure new_extension
   end interface newActualConnectionPt

   interface operator(<)
      module procedure less_than
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

contains

   function new_newActualPt_from_v_pt(v_pt) result(a_pt)
      type(newActualConnectionPt) :: a_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt

      a_pt%v_pt = v_pt

   end function new_newActualPt_from_v_pt

   function new_extension(v_pt, label) result(a_pt)
      type(newActualConnectionPt) :: a_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt
      integer, intent(in) :: label

      a_pt%v_pt = v_pt
      a_pt%label = label

   end function new_extension

   function extend(this) result(ext_pt)
      type(newActualConnectionPt) :: ext_pt
      class(newActualConnectionPt), intent(in) :: this

      ext_pt%v_pt = this%v_pt

      ext_pt%label = 0
      if (this%is_extension()) ext_pt%label = this%label + 1
         
   end function extend

   function add_comp_name(this, comp_name) result(a_pt)
      type(newActualConnectionPt) :: a_pt
      class(newActualConnectionPt), intent(in) :: this
      character(*), intent(in) :: comp_name

      a_pt%v_pt = this%v_pt%add_comp_name(comp_name)
      
   end function add_comp_name


   function get_state_intent(this) result(state_intent)
      character(:), allocatable :: state_intent
      class(newActualConnectionPt), intent(in) :: this

      state_intent = this%v_pt%get_state_intent()

   end function get_state_intent


   ! Important that name is different if either comp_name or short_name differ
   function get_esmf_name(this) result(name)
      character(:), allocatable :: name
      class(newActualConnectionPt), intent(in) :: this

      name = this%v_pt%get_esmf_name()
         
   end function get_esmf_name

   function get_extension_string(this) result(s)
      class(newActualConnectionPt), intent(in) :: this
      character(:), allocatable :: s
      
      character(16) :: buf

      s = ''
      if (this%is_extension()) then
         write(buf, '(i0)') this%label
         s = trim(buf)
      end if
   end function get_extension_string
      

   logical function less_than(lhs, rhs)
      type(newActualConnectionPt), intent(in) :: lhs
      class(newActualConnectionPt), intent(in) :: rhs

      select type (rhs)
      type is (newActualConnectionPt)
         less_than = lhs%v_pt < rhs%v_pt
      class default
         less_than = .true.
      end select

   end function less_than

   logical function equal_to(lhs, rhs)
      type(newActualConnectionPt), intent(in) :: lhs
      type(newActualConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))
      
   end function equal_to

   logical function is_import(this)
      class(newActualConnectionPt), intent(in) :: this
      is_import = this%v_pt%is_import()
   end function is_import

   logical function is_export(this)
      class(newActualConnectionPt), intent(in) :: this
      is_export = this%v_pt%is_export()
   end function is_export

   logical function is_internal(this)
      class(newActualConnectionPt), intent(in) :: this
      is_internal = this%v_pt%is_internal()
   end function is_internal

   logical function is_extension(this)
      class(newActualConnectionPt), intent(in) :: this
      is_extension = allocated(this%label)
   end function is_extension

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(newActualConnectionPt), intent(in) :: this

      s = "Actual{intent: <" // this%get_state_intent() // ">, name: <" // this%get_esmf_name() // "> }"
      
   end function to_string

end module mapl3g_newActualConnectionPt
