!------------------------------------------------------------------------------
! CharacteristicStatus: status value every StateItemCharacteristic
! (StateItemCharacteristic.F90) carries (docs/graph/spec/18-state-item-
! characteristics.md REQ-CHAR-003/004, openspec/changes/
! state-item-characteristics design.md D1). Keeps the spec's own working
! name - no collision, no better alternative was proposed.
!
! Five values only (REQ-CHAR-003): INVALID (no meaningful value yet, the
! default-constructed state), SPECIFIED (fully resolved/authoritative),
! MIRRORED (not yet resolved, will be made to match a source once
! connected), UNCHECKED (connection allowed without reconciliation despite
! a known/possible mismatch - an explicit, dangerous opt-out that MUST
! remain diagnostically distinguishable from SPECIFIED/MIRRORED), and
! DEFERRED (processing deferred to a known, later point - distinct from
! INVALID, which implies "not yet touched" with no known future resolution
! point). Design.md D1: no additional ERROR/CONFLICT value is added here -
! a reconciliation failure this capability's own detection/mutator code
! cannot resolve is reported as an ordinary rc/_FAIL error at the call
! site, not encoded as a sixth status value.
!
! Follows CharacteristicId.F90's own shape (wrapped integer, named
! parameter constants, operator(==)/operator(/=), to_string()) -
! deliberately independent of it (design.md D2's own rationale for
! StateItemCharacteristicKind applies equally here: this is a different
! concept living in the same directory).
!------------------------------------------------------------------------------
module mapl_CharacteristicStatus_mod
   implicit none(type, external)
   private

   public :: CharacteristicStatus
   public :: operator(==)
   public :: operator(/=)
   public :: CHARACTERISTIC_STATUS_INVALID
   public :: CHARACTERISTIC_STATUS_SPECIFIED
   public :: CHARACTERISTIC_STATUS_MIRRORED
   public :: CHARACTERISTIC_STATUS_UNCHECKED
   public :: CHARACTERISTIC_STATUS_DEFERRED

   type :: CharacteristicStatus
      private
      integer :: value = 0
   contains
      procedure :: to_string => status_to_string
   end type CharacteristicStatus

   ! value=0 (the type's own default-initializer) is INVALID, so a
   ! default-constructed CharacteristicStatus() is invalid without any
   ! explicit constructor call - mirrors NodeRevision's own "distinct
   ! invalid default" precedent.
   type(CharacteristicStatus), parameter :: CHARACTERISTIC_STATUS_INVALID    = CharacteristicStatus(0)
   type(CharacteristicStatus), parameter :: CHARACTERISTIC_STATUS_SPECIFIED  = CharacteristicStatus(1)
   type(CharacteristicStatus), parameter :: CHARACTERISTIC_STATUS_MIRRORED   = CharacteristicStatus(2)
   type(CharacteristicStatus), parameter :: CHARACTERISTIC_STATUS_UNCHECKED  = CharacteristicStatus(3)
   type(CharacteristicStatus), parameter :: CHARACTERISTIC_STATUS_DEFERRED   = CharacteristicStatus(4)

   interface operator(==)
      module procedure status_equal
   end interface operator(==)

   interface operator(/=)
      module procedure status_not_equal
   end interface operator(/=)

contains

   pure logical function status_equal(left, right) result(equal)
      type(CharacteristicStatus), intent(in) :: left, right

      equal = left%value == right%value
   end function status_equal

   pure logical function status_not_equal(left, right) result(not_equal)
      type(CharacteristicStatus), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function status_not_equal

   pure function status_to_string(this) result(name)
      class(CharacteristicStatus), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0); name = 'INVALID'
      case (1); name = 'SPECIFIED'
      case (2); name = 'MIRRORED'
      case (3); name = 'UNCHECKED'
      case (4); name = 'DEFERRED'
      case default; name = 'UNKNOWN'
      end select
   end function status_to_string

end module mapl_CharacteristicStatus_mod
