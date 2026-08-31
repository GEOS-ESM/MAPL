#include "MAPL.h"

module mapl_AspectState_mod
   implicit none(type,external)
   private

   public :: AspectState
   public :: operator(==)
   public :: operator(/=)
   public :: ASPECT_STATE_INVALID
   public :: ASPECT_STATE_FROM_COMP
   public :: ASPECT_STATE_SPECIFIED
   public :: ASPECT_STATE_MIRRORED
   public :: ASPECT_STATE_DEFERRED
   public :: ASPECT_STATE_UNCHECKED

   type :: AspectState
      private
      integer :: value = 0
   contains
      procedure :: to_string
   end type AspectState

   type(AspectState), parameter :: ASPECT_STATE_INVALID   = AspectState(0)
   type(AspectState), parameter :: ASPECT_STATE_FROM_COMP = AspectState(1)
   type(AspectState), parameter :: ASPECT_STATE_SPECIFIED = AspectState(2)
   type(AspectState), parameter :: ASPECT_STATE_MIRRORED  = AspectState(3)
   type(AspectState), parameter :: ASPECT_STATE_DEFERRED  = AspectState(4)
   type(AspectState), parameter :: ASPECT_STATE_UNCHECKED = AspectState(5)

   interface operator(==)
      procedure :: equals
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

contains

   elemental logical function equals(a, b)
      type(AspectState), intent(in) :: a
      type(AspectState), intent(in) :: b
      equals = (a%value == b%value)
   end function equals

   elemental logical function not_equal_to(a, b)
      type(AspectState), intent(in) :: a
      type(AspectState), intent(in) :: b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   function to_string(this) result(str)
      class(AspectState), intent(in) :: this
      character(len=:), allocatable :: str

      select case (this%value)
      case (ASPECT_STATE_INVALID%value)
         str = 'INVALID'
      case (ASPECT_STATE_FROM_COMP%value)
         str = 'FROM_COMP'
      case (ASPECT_STATE_SPECIFIED%value)
         str = 'SPECIFIED'
      case (ASPECT_STATE_MIRRORED%value)
         str = 'MIRRORED'
      case (ASPECT_STATE_DEFERRED%value)
         str = 'DEFERRED'
      case (ASPECT_STATE_UNCHECKED%value)
         str = 'UNCHECKED'
      case default
         str = 'UNKNOWN'
      end select
   end function to_string

end module mapl_AspectState_mod
