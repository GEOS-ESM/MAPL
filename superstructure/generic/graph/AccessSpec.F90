#include "MAPL.h"

!------------------------------------------------------------------------------
! AccessSpec: general-purpose argument access-mode classification
! (spec/15-callbacks.md REQ-CB-005: "a general MAPL concept, not
! callback-specific"), introduced by MethodGraphNode's own named
! argument declarations (spec/12-methods-and-drivers.md REQ-MTH-002,
! which restates spec/15-callbacks.md §15.4's AccessSpec at the graph
! level) ahead of that capability's own CallbackMethodSpec (REQ-CB-004,
! "argument name -> AccessSpec") so the latter can reuse this type
! unchanged rather than duplicating it.
!
! Follows MAPL_StateItem_Flag's exact style (MAPLStateItemFlag.F90): a
! small derived type wrapping a private integer code, compared via
! operator(==)/operator(/=), not a plain integer - so new access modes
! can be added later without disturbing client comparison call sites.
!------------------------------------------------------------------------------
module mapl_AccessSpec_mod
   implicit none(type, external)
   private

   public :: AccessSpec
   public :: operator(==)
   public :: operator(/=)

   public :: MAPL_ACCESS_IN
   public :: MAPL_ACCESS_OUT
   public :: MAPL_ACCESS_INOUT
   public :: MAPL_ACCESS_UNSPECIFIED

   type :: AccessSpec
      private
      integer :: value = -1
   contains
      procedure :: to_string => access_to_string
   end type AccessSpec

   type(AccessSpec), parameter :: MAPL_ACCESS_IN          = AccessSpec(0)
   type(AccessSpec), parameter :: MAPL_ACCESS_OUT         = AccessSpec(1)
   type(AccessSpec), parameter :: MAPL_ACCESS_INOUT       = AccessSpec(2)
   type(AccessSpec), parameter :: MAPL_ACCESS_UNSPECIFIED = AccessSpec(3)

   interface operator(==)
      module procedure access_equal
   end interface operator(==)

   interface operator(/=)
      module procedure access_not_equal
   end interface operator(/=)

contains

   pure logical function access_equal(left, right) result(equal)
      type(AccessSpec), intent(in) :: left, right

      equal = left%value == right%value
   end function access_equal

   pure logical function access_not_equal(left, right) result(not_equal)
      type(AccessSpec), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function access_not_equal

   pure function access_to_string(this) result(name)
      class(AccessSpec), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0); name = 'IN'
      case (1); name = 'OUT'
      case (2); name = 'INOUT'
      case (3); name = 'UNSPECIFIED'
      case default; name = 'UNKNOWN'
      end select
   end function access_to_string

end module mapl_AccessSpec_mod
