!------------------------------------------------------------------------------
! CharacteristicId: type-safe identity for a Characteristic kind
! (mapl_Characteristic_mod) - the graph's own analog of
! superstructure/generic/specs/AspectId.F90, mirroring its pattern
! exactly (wrapped integer, named parameter constants, ==/</=,
! to_string()) but deliberately independent of it, per extension-reuse
! change design.md Decisions ("Characteristic is the graph's own name
! for what legacy calls an Aspect, and is a deliberately independent
! design, not a reskin").
!------------------------------------------------------------------------------
module mapl_CharacteristicId_mod
   implicit none(type, external)
   private

   ! Type
   public :: CharacteristicId
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)
   ! Parameters
   public :: UNITS_CHARACTERISTIC_ID
   public :: VERTICAL_GRID_CHARACTERISTIC_ID
   public :: GEOM_CHARACTERISTIC_ID
   public :: INVALID_CHARACTERISTIC_ID
   public :: MOCK_CHARACTERISTIC_ID

   type :: CharacteristicId
      private
      integer :: id
   contains
      procedure :: to_string
   end type CharacteristicId

   type(CharacteristicId), parameter :: INVALID_CHARACTERISTIC_ID = CharacteristicId(-1)
   type(CharacteristicId), parameter :: UNITS_CHARACTERISTIC_ID = CharacteristicId(1)
   type(CharacteristicId), parameter :: VERTICAL_GRID_CHARACTERISTIC_ID = CharacteristicId(2)
   type(CharacteristicId), parameter :: GEOM_CHARACTERISTIC_ID = CharacteristicId(3)

   ! Test-only, mirrors AspectId.F90's own MOCK_ASPECT_ID precedent -
   ! lets a test define a fake Characteristic subclass without
   ! colliding with a real kind.
   type(CharacteristicId), parameter :: MOCK_CHARACTERISTIC_ID = CharacteristicId(99)

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
      class(CharacteristicId), intent(in) :: this

      select case (this%id)
      case (UNITS_CHARACTERISTIC_ID%id)
         s = "UNITS"
      case (VERTICAL_GRID_CHARACTERISTIC_ID%id)
         s = "VERTICAL_GRID"
      case (GEOM_CHARACTERISTIC_ID%id)
         s = "GEOM"
      case (MOCK_CHARACTERISTIC_ID%id)
         s = "MOCK"
      case default
         s = "UNKNOWN"
      end select
   end function to_string

   logical elemental function equal(a, b)
      class(CharacteristicId), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   logical elemental function not_equal(a, b)
      class(CharacteristicId), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

   logical function less_than(a, b)
      class(CharacteristicId), intent(in) :: a, b
      less_than = a%id < b%id
   end function less_than

end module mapl_CharacteristicId_mod
