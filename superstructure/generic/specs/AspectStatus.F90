#include "MAPL.h"

module mapl_AspectStatus_mod
   implicit none(type,external)
   private

   public :: AspectStatus
   public :: operator(==)
   public :: operator(/=)
   public :: ASPECT_STATUS_INVALID
   public :: ASPECT_STATUS_FROM_COMP
   public :: ASPECT_STATUS_SPECIFIED
   public :: ASPECT_STATUS_MIRRORED
   public :: ASPECT_STATUS_DEFERRED
   public :: ASPECT_STATUS_UNCHECKED

   type :: AspectStatus
      private
      integer :: value = 0
   contains
      procedure :: to_string
   end type AspectStatus

   type(AspectStatus), parameter :: ASPECT_STATUS_INVALID   = AspectStatus(0)
   type(AspectStatus), parameter :: ASPECT_STATUS_FROM_COMP = AspectStatus(1)
   type(AspectStatus), parameter :: ASPECT_STATUS_SPECIFIED = AspectStatus(2)
   type(AspectStatus), parameter :: ASPECT_STATUS_MIRRORED  = AspectStatus(3)
   type(AspectStatus), parameter :: ASPECT_STATUS_DEFERRED  = AspectStatus(4)
   type(AspectStatus), parameter :: ASPECT_STATUS_UNCHECKED = AspectStatus(5)

   interface operator(==)
      procedure :: equals
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

contains

   elemental logical function equals(a, b)
      type(AspectStatus), intent(in) :: a
      type(AspectStatus), intent(in) :: b
      equals = (a%value == b%value)
   end function equals

   elemental logical function not_equal_to(a, b)
      type(AspectStatus), intent(in) :: a
      type(AspectStatus), intent(in) :: b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   function to_string(this) result(str)
      class(AspectStatus), intent(in) :: this
      character(len=:), allocatable :: str

      select case (this%value)
      case (ASPECT_STATUS_INVALID%value)
         str = 'INVALID'
      case (ASPECT_STATUS_FROM_COMP%value)
         str = 'FROM_COMP'
      case (ASPECT_STATUS_SPECIFIED%value)
         str = 'SPECIFIED'
      case (ASPECT_STATUS_MIRRORED%value)
         str = 'MIRRORED'
      case (ASPECT_STATUS_DEFERRED%value)
         str = 'DEFERRED'
      case (ASPECT_STATUS_UNCHECKED%value)
         str = 'UNCHECKED'
      case default
         str = 'UNKNOWN'
      end select
   end function to_string

end module mapl_AspectStatus_mod
