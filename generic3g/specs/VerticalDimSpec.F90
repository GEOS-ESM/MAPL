#include "MAPL_Generic.h"

module mapl3g_VerticalDimSpec
   !use mapl3g_UngriddedDimSpec
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
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
   end type VerticalDimSpec

   type(VerticalDimSpec), parameter :: VERTICAL_DIM_UNKNOWN = VerticalDimSpec(-1)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_NONE = VerticalDimSpec(0)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_CENTER = VerticalDimSpec(1)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_EDGE = VerticalDimSpec(2)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_MIRROR = VerticalDimSpec(3)

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
