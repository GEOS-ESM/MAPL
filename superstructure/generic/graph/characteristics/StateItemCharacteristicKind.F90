!------------------------------------------------------------------------------
! StateItemCharacteristicKind: type-safe, stable identity for a concrete
! StateItemCharacteristic (StateItemCharacteristic.F90) subclass
! (REQ-CHAR-005/006).
!
! Named StateItemCharacteristicKind to avoid collision with
! mapl_CharacteristicId_mod (unrelated, different purpose).
!
! Shape mirrors CharacteristicId.F90 (wrapped integer, operators, to_string).
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
   ! Values 4-10: Vertical, Attributes, Ungridded, Quantity, Conservation,
   ! Normalization, Standard Name.
   public :: VERTICAL_GRID_CHARACTERISTIC_KIND
   public :: ATTRIBUTES_CHARACTERISTIC_KIND
   public :: UNGRIDDED_DIMS_CHARACTERISTIC_KIND
   public :: QUANTITY_TYPE_CHARACTERISTIC_KIND
   public :: CONSERVATION_CHARACTERISTIC_KIND
   public :: NORMALIZATION_CHARACTERISTIC_KIND
   public :: STANDARD_NAME_CHARACTERISTIC_KIND
   ! Value 11: the item-class/shape axis.
   public :: ITEM_SHAPE_CHARACTERISTIC_KIND
   ! Value 12: vertical stagger location.
   public :: VERTICAL_STAGGER_CHARACTERISTIC_KIND
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

   ! Values 4-10: one per remaining legacy *Aspect axis (REQ-CHAR-006).
   type(StateItemCharacteristicKind), parameter :: VERTICAL_GRID_CHARACTERISTIC_KIND = StateItemCharacteristicKind(4)
   type(StateItemCharacteristicKind), parameter :: ATTRIBUTES_CHARACTERISTIC_KIND          = StateItemCharacteristicKind(5)
   type(StateItemCharacteristicKind), parameter :: UNGRIDDED_DIMS_CHARACTERISTIC_KIND      = StateItemCharacteristicKind(6)
   type(StateItemCharacteristicKind), parameter :: QUANTITY_TYPE_CHARACTERISTIC_KIND       = StateItemCharacteristicKind(7)
   type(StateItemCharacteristicKind), parameter :: CONSERVATION_CHARACTERISTIC_KIND        = StateItemCharacteristicKind(8)
   type(StateItemCharacteristicKind), parameter :: NORMALIZATION_CHARACTERISTIC_KIND       = StateItemCharacteristicKind(9)
   type(StateItemCharacteristicKind), parameter :: STANDARD_NAME_CHARACTERISTIC_KIND       = StateItemCharacteristicKind(10)

   ! Value 11: item-class/shape axis (ItemShapeCharacteristic.F90).
   type(StateItemCharacteristicKind), parameter :: ITEM_SHAPE_CHARACTERISTIC_KIND          = StateItemCharacteristicKind(11)

   ! Value 12: vertical stagger (VerticalStaggerCharacteristic.F90).
   type(StateItemCharacteristicKind), parameter :: VERTICAL_STAGGER_CHARACTERISTIC_KIND    = StateItemCharacteristicKind(12)

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
      case (VERTICAL_GRID_CHARACTERISTIC_KIND%value)
         s = "VERTICAL_GRID"
      case (ATTRIBUTES_CHARACTERISTIC_KIND%value)
         s = "ATTRIBUTES"
      case (UNGRIDDED_DIMS_CHARACTERISTIC_KIND%value)
         s = "UNGRIDDED_DIMS"
      case (QUANTITY_TYPE_CHARACTERISTIC_KIND%value)
         s = "QUANTITY_TYPE"
      case (CONSERVATION_CHARACTERISTIC_KIND%value)
         s = "CONSERVATION"
      case (NORMALIZATION_CHARACTERISTIC_KIND%value)
         s = "NORMALIZATION"
      case (STANDARD_NAME_CHARACTERISTIC_KIND%value)
         s = "STANDARD_NAME"
      case (ITEM_SHAPE_CHARACTERISTIC_KIND%value)
         s = "ITEM_SHAPE"
      case (VERTICAL_STAGGER_CHARACTERISTIC_KIND%value)
         s = "VERTICAL_STAGGER"
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
