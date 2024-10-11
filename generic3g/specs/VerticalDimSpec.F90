#include "MAPL_Generic.h"

module mapl3g_VerticalDimSpec

   !use mapl3g_UngriddedDimSpec
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
   use esmf, only: ESMF_MAXSTR
   use mapl_ErrorHandling

   implicit none
   private
  
   public :: VerticalDimSpec
  
   public :: VERTICAL_DIM_UNKNOWN
   public :: VERTICAL_DIM_NONE
   public :: VERTICAL_DIM_CENTER
   public :: VERTICAL_DIM_EDGE
   public :: VERTICAL_DIM_MIRROR

   public :: operator(==)
   public :: operator(/=)

   type :: VerticalDimSpec
      private
      integer :: id = -1
   contains
      procedure :: make_info
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VerticalDimSpec

   type(VerticalDimSpec), parameter :: VERTICAL_DIM_UNKNOWN = VerticalDimSpec(-1)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_NONE = VerticalDimSpec(1)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_CENTER = VerticalDimSpec(2)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_EDGE = VerticalDimSpec(3)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_MIRROR = VerticalDimSpec(4)

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)
      
contains

   elemental logical function equal_to(a, b)
      type(VerticalDimSpec), intent(in) :: a, b
      equal_to = a%id == b%id
   end function equal_to

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(VerticalDimSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      integer :: id
      character(len=ESMF_MAXSTR) :: dim_spec_str

      id = this%id
      select case(id)
      case(-1)
         dim_spec_str = "VERTICAL_DIM_UNKNOWN"
      case(1)
         dim_spec_str = "VERTICAL_DIM_NONE"
      case(2)
         dim_spec_str = "VERTICAL_DIM_CENTER"
      case(3)
         dim_spec_str = "VERTICAL_DIM_EDGE"
      case(4)
         dim_spec_str = "VERTICAL_DIM_MIRROR"
      ! case default
      !    _FAIL("Invalid vertical dim spec")
      end select
      write(unit, '("VerticalDimSpec{",a,"}")', iostat=iostat, iomsg=iomsg) trim(dim_spec_str)

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   elemental logical function not_equal_to(a, b)
      type(VerticalDimSpec), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   function make_info(this, rc) result(info)
      type(ESMF_Info) :: info
      class(VerticalDimSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      info = ESMF_InfoCreate(_RC)
      select case (this%id)
      case (VERTICAL_DIM_NONE%id)
         call ESMF_InfoSet(info, key='vloc', value='VERTICAL_DIM_NONE', _RC)
      case (VERTICAL_DIM_CENTER%id)
         call ESMF_InfoSet(info, key='vloc', value='VERTICAL_DIM_CENTER', _RC)
      case (VERTICAL_DIM_EDGE%id)
         call ESMF_InfoSet(info, key='vloc', value='VERTICAL_DIM_EDGE', _RC)
      case default
         _FAIL('unsupported vertical dim spec')
      end select

      _RETURN(_SUCCESS)
   end function make_info
   
end module mapl3g_VerticalDimSpec
