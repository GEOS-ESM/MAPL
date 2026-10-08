!------------------------------------------------------------------------------
! StateItemCharacteristicKind: type-safe, stable identity for a concrete
! StateItemCharacteristic (StateItemCharacteristic.F90) subclass
! (docs/graph/spec/18-state-item-characteristics.md REQ-CHAR-005/006,
! openspec/changes/state-item-characteristics design.md D2).
!
! Named StateItemCharacteristicKind rather than the spec's own working
! name, CharacteristicType, for one concrete reason (design.md D2): this
! directory already has mapl_CharacteristicId_mod -
! "type-safe identity for a Characteristic kind" - for the entirely
! different, deliberately independent graph/extension-reuse Characteristic
! hierarchy (Characteristic.F90). CharacteristicId next to a new,
! unrelated CharacteristicType would be a real, ongoing readability
! hazard - two near-identical names, two unrelated purposes, no textual
! cue which is which - the same class of problem this codebase already
! solved once for UnitsConverterTransform vs. legacy ConvertUnitsTransform
! by picking a visibly different name. Ties the name directly to the type
! it tags (StateItemCharacteristic), matching REQ-CHAR-005's own
! description ("closer to a type-tag enumeration than to NodeId").
!
! Shape mirrors CharacteristicId.F90 exactly (wrapped integer, named
! parameter constants, operator(==)/operator(/=)/operator(<), to_string())
! - same pattern, new, unambiguous name, no shared module dependency with
! CharacteristicId.F90 beyond both existing in the same directory.
!------------------------------------------------------------------------------
module mapl_StateItemCharacteristicKind_mod
   implicit none(type, external)
   private

   ! Type
   public :: StateItemCharacteristicKind
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)
   ! Parameters
   public :: INVALID_CHARACTERISTIC_KIND
   public :: PHYSICAL_UNITS_CHARACTERISTIC_KIND
   public :: TYPE_KIND_CHARACTERISTIC_KIND
   public :: GEOMETRY_CHARACTERISTIC_KIND
   public :: MOCK_CHARACTERISTIC_KIND

   type :: StateItemCharacteristicKind
      private
      integer :: value
   contains
      procedure :: to_string
   end type StateItemCharacteristicKind

   type(StateItemCharacteristicKind), parameter :: INVALID_CHARACTERISTIC_KIND        = StateItemCharacteristicKind(-1)
   type(StateItemCharacteristicKind), parameter :: PHYSICAL_UNITS_CHARACTERISTIC_KIND = StateItemCharacteristicKind(1)
   type(StateItemCharacteristicKind), parameter :: TYPE_KIND_CHARACTERISTIC_KIND      = StateItemCharacteristicKind(2)
   type(StateItemCharacteristicKind), parameter :: GEOMETRY_CHARACTERISTIC_KIND       = StateItemCharacteristicKind(3)

   ! Test-only, mirrors CharacteristicId.F90's own MOCK_CHARACTERISTIC_ID
   ! precedent - lets a test define a fake StateItemCharacteristic
   ! subclass without colliding with a real kind.
   type(StateItemCharacteristicKind), parameter :: MOCK_CHARACTERISTIC_KIND = StateItemCharacteristicKind(99)

   interface operator(==)
      procedure equal
   end interface operator(==)

   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

   interface operator(<)
      procedure less_than
   end interface operator(<)

contains

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(StateItemCharacteristicKind), intent(in) :: this

      select case (this%value)
      case (PHYSICAL_UNITS_CHARACTERISTIC_KIND%value)
         s = "PHYSICAL_UNITS"
      case (TYPE_KIND_CHARACTERISTIC_KIND%value)
         s = "TYPE_KIND"
      case (GEOMETRY_CHARACTERISTIC_KIND%value)
         s = "GEOMETRY"
      case (MOCK_CHARACTERISTIC_KIND%value)
         s = "MOCK"
      case default
         s = "UNKNOWN"
      end select
   end function to_string

   logical elemental function equal(a, b)
      class(StateItemCharacteristicKind), intent(in) :: a, b
      equal = a%value == b%value
   end function equal

   logical elemental function not_equal(a, b)
      class(StateItemCharacteristicKind), intent(in) :: a, b
      not_equal = .not. (a%value == b%value)
   end function not_equal

   logical function less_than(a, b)
      class(StateItemCharacteristicKind), intent(in) :: a, b
      less_than = a%value < b%value
   end function less_than

end module mapl_StateItemCharacteristicKind_mod
