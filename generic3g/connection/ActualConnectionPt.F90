#include "MAPL.h"

module mapl3g_ActualConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl_KeywordEnforcer
   implicit none
   private
  
   public :: ActualConnectionPt
   public :: extend
   public :: operator(<)
   public :: operator(==)

   ! Note: The design intentioally does not have ActualConnectionPt
   ! inherit from VirtualConnectionPt in order to allow for future
   ! subclasses of VirtualConnectionPt in some interfaces while not
   ! permitting ActualConnectionPt objects.  A potential refactoring
   ! would be instead to have both classes inherit from a single
   ! obstract ConnectionPt class.  TBD

   type :: ActualConnectionPt
      private
      type(VirtualConnectionPt) :: v_pt
      integer, allocatable :: label
   contains
      procedure :: extend => extend_

      procedure :: get_state_intent
      procedure :: get_esmf_name
      procedure :: get_label
      procedure :: get_full_name
      procedure :: get_comp_name
      procedure :: add_comp_name

      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal

      procedure :: is_extension
      procedure :: get_extension_string

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: is_represented_in
   end type ActualConnectionPt

   ! Constructors
   interface ActualConnectionPt
      module procedure new_ActualPt_from_v_pt
      module procedure new_extension
   end interface ActualConnectionPt

   interface operator(<)
      module procedure less_than
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface extend
      module procedure extend_
   end interface extend

contains

   function new_ActualPt_from_v_pt(v_pt) result(a_pt)
      type(ActualConnectionPt) :: a_pt
      type(VirtualConnectionPt), intent(in) :: v_pt

      a_pt%v_pt = v_pt

   end function new_ActualPt_from_v_pt

   function new_extension(v_pt, label) result(a_pt)
      type(ActualConnectionPt) :: a_pt
      type(VirtualConnectionPt), intent(in) :: v_pt
      integer, intent(in) :: label

      a_pt%v_pt = v_pt
      a_pt%label = label

   end function new_extension

   function extend_(this) result(ext_pt)
      type(ActualConnectionPt) :: ext_pt
      class(ActualConnectionPt), intent(in) :: this

      ext_pt%v_pt = this%v_pt
      if (this%is_extension()) then
         ext_pt%label = this%label + 1
         return
      endif
      ! default
      ext_pt%label = 1
         
   end function extend_

   function add_comp_name(this, comp_name) result(a_pt)
      type(ActualConnectionPt) :: a_pt
      class(ActualConnectionPt), intent(in) :: this
      character(*), intent(in) :: comp_name

      a_pt%v_pt = this%v_pt%add_comp_name(comp_name)
      if (allocated(this%label)) a_pt%label = this%label
      
   end function add_comp_name


   function get_state_intent(this) result(state_intent)
      character(:), allocatable :: state_intent
      class(ActualConnectionPt), intent(in) :: this

      state_intent = this%v_pt%get_state_intent()

   end function get_state_intent

   function get_label(this) result(label)
      integer :: label
      class(ActualConnectionPt), intent(in) :: this

      label = -1
      if (allocated(this%label)) label = this%label

   end function get_label


   ! Important that name is different if either comp_name or short_name differ
   function get_esmf_name(this) result(name)
      character(:), allocatable :: name
      class(ActualConnectionPt), intent(in) :: this

      name = this%v_pt%get_esmf_name()
      if (this%is_extension()) &
           name = name // this%get_extension_string()
   end function get_esmf_name

   ! Important that name is different if either comp_name or short_name differ
   function get_full_name(this) result(name)
      character(:), allocatable :: name
      class(ActualConnectionPt), intent(in) :: this

      name = this%v_pt%get_full_name()
      if (this%is_extension()) &
           name = name // this%get_extension_string()
   end function get_full_name

   function get_extension_string(this) result(s)
      class(ActualConnectionPt), intent(in) :: this
      character(:), allocatable :: s
      
      character(16) :: buf

      s = ''
      if (this%is_extension()) then
         write(buf, '("(",i0,")")') this%label
         s = trim(buf)
      end if
   end function get_extension_string
      

   logical function less_than(lhs, rhs)
      type(ActualConnectionPt), intent(in) :: lhs
      type(ActualConnectionPt), intent(in) :: rhs

      less_than = (lhs%v_pt < rhs%v_pt)
      if (less_than) return
      if (rhs%v_pt < lhs%v_pt) return

      less_than = get_label(rhs) < get_label(lhs)

   contains

      integer function get_label(a_pt)
         type(ActualConnectionPt), intent(in) :: a_pt

         get_label = -1
         if (allocated(a_pt%label)) get_label = a_pt%label
      end function get_label

   end function less_than

   logical function equal_to(lhs, rhs)
      type(ActualConnectionPt), intent(in) :: lhs
      type(ActualConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))
      
   end function equal_to

   logical function is_import(this)
      class(ActualConnectionPt), intent(in) :: this
      is_import = this%v_pt%is_import()
   end function is_import

   logical function is_export(this)
      class(ActualConnectionPt), intent(in) :: this
      is_export = this%v_pt%is_export()
   end function is_export

   logical function is_internal(this)
      class(ActualConnectionPt), intent(in) :: this
      is_internal = this%v_pt%is_internal()
   end function is_internal

   logical function is_extension(this)
      class(ActualConnectionPt), intent(in) :: this
      is_extension = allocated(this%label)
   end function is_extension

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(ActualConnectionPt), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, '("Actual{intent: <",a,">, comp: <",a,">, full name: <",a,">}")', iostat=iostat, iomsg=iomsg) &
           this%get_state_intent(), this%get_comp_name(), this%get_full_name()

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   function get_comp_name(this) result(name)
      character(:), allocatable :: name
      class(ActualConnectionPt), intent(in) :: this
      name = this%v_pt%get_comp_name()
   end function get_comp_name
      

   logical function is_represented_in(this, mode)
      class(ActualConnectionPt), intent(in) :: this
      character(*), intent(in) :: mode ! user or outer grid comp

      is_represented_in = .false. ! unless

      select case (mode)
      case ('user') ! only add undecorated items
         if (this%is_extension()) return
         if (this%get_comp_name() /= '') return
      case ('outer') ! do not add internal items
         if (this%get_state_intent() == 'internal') return
      case default
         error stop "Illegal mode in ActualConnectionPt.F90 - should be checked by calling procedure."
      end select

      is_represented_in = .true.
      
   end function is_represented_in

end module mapl3g_ActualConnectionPt
