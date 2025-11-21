#include "MAPL.h"

module mapl3g_VerticalRegridMethod

   use esmf, only: ESMF_MAXSTR

   implicit none
   private
   
   public :: VerticalRegridMethod
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type :: VerticalRegridMethod
      private
      integer :: id = -1
   contains
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VerticalRegridMethod


   interface operator(==)
      procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

   type(VerticalRegridMethod), parameter :: VERTICAL_REGRID_UNKNOWN = VerticalRegridMethod(-1)
   type(VerticalRegridMethod), parameter :: VERTICAL_REGRID_LINEAR = VerticalRegridMethod(1)
   type(VerticalRegridMethod), parameter :: VERTICAL_REGRID_CONSERVATIVE = VerticalRegridMethod(2)

contains

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(VerticalRegridMethod), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      integer :: id
      character(len=ESMF_MAXSTR) :: regrid_method_str

      id = this%id
      select case(id)
      case(-1)
         regrid_method_str = "VERTICAL_REGRID_UNKNOWN"
      case(1)
         regrid_method_str = "VERTICAL_REGRID_LINEAR"
      case(2)
         regrid_method_str = "VERTICAL_REGRID_CONSERVATIVE"
      ! case default
      !    _FAIL("Invalid vertical dim spec")
      end select
      write(unit, '("VerticalRegridMethod(",a,")")', iostat=iostat, iomsg=iomsg) trim(regrid_method_str)

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   elemental logical function equal_to(a, b)
      type(VerticalRegridMethod), intent(in) :: a, b
      equal_to = (a%id == b%id)
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(VerticalRegridMethod), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to

end module mapl3g_VerticalRegridMethod
