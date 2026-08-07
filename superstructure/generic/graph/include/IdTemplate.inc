! MAPL_IdTemplate.inc
!
! Required definitions before inclusion:
!
!   ID_NAME
!   ID_KIND
!
! Example:
!
!   #define ID_NAME NodeId
!   #define ID_KIND INT32
!   module mapl_NodeId_mod
!   #include "IdTemplate.inc"
!   end module mapl_NodeId_mod

#include "MAPL.h"

! Two-step concatenation ensures macro arguments expand before ## is applied.
#define IDENTITY(x) x
#define ID_CAT_IMPL(a,b) IDENTITY(a)IDENTITY(b)
#define ID_CAT(a,b) ID_CAT_IMPL(a,b)

#define ID_GENERATOR ID_CAT(ID_NAME,Generator)
#define INVALID_ID   ID_CAT(INVALID_,ID_NAME)

   use, intrinsic :: iso_fortran_env, only: INT8, INT16, INT32, INT64
   use :: mapl_ErrorHandling_mod
   implicit none(type,external)
   private

   public :: ID_NAME
   public :: ID_GENERATOR
   public :: INVALID_ID

   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)

   ! The underlying integer kind remains an implementation detail.
   integer, parameter :: ID_KIND_ = ID_KIND
   integer, parameter :: MAX_ID_VALUE = huge(1_ID_KIND_)

   ! Zero is reserved for an invalid or absent identifier.
   integer(ID_KIND_), parameter :: INVALID_VALUE = 0

   type :: ID_NAME
      private

      integer(ID_KIND_) :: value = INVALID_VALUE

   contains

      procedure :: is_valid => id_is_valid

   end type ID_NAME

   ! External code can use the invalid value without accessing the
   ! encapsulated integer representation.
   type(ID_NAME), parameter :: INVALID_ID = &
      ID_NAME(INVALID_VALUE)

   type :: ID_GENERATOR
      private

      ! One is the first valid identifier.
      integer(ID_KIND_) :: next_value = 1

   contains

      procedure :: next => generator_next

   end type ID_GENERATOR

   interface operator(==)
      module procedure id_equal
   end interface

   interface operator(/=)
      module procedure id_not_equal
   end interface

   ! Ordering is often needed when the identifier is a gFTL map key.
   interface operator(<)
      module procedure id_less_than
   end interface

contains

   pure logical function id_is_valid(this) result(is_valid)
      class(ID_NAME), intent(in) :: this

      is_valid = this%value /= INVALID_VALUE
   end function id_is_valid


   pure logical function id_equal(left, right) result(is_equal)
      type(ID_NAME), intent(in) :: left
      type(ID_NAME), intent(in) :: right

      is_equal = left%value == right%value
   end function id_equal


   pure logical function id_not_equal(left, right) result(is_not_equal)
      type(ID_NAME), intent(in) :: left
      type(ID_NAME), intent(in) :: right

      is_not_equal = .not. (left == right)
   end function id_not_equal


   pure logical function id_less_than(left, right) result(is_less)
      type(ID_NAME), intent(in) :: left
      type(ID_NAME), intent(in) :: right

      is_less = left%value < right%value
   end function id_less_than


   function generator_next(this, rc) result(id)
      type(ID_NAME) :: id
      class(ID_GENERATOR), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      ! Always return a defined value if generation fails.
      id = INVALID_ID

      ! Reserve the largest representable integer. This guarantees that
      ! advancing next_value can never overflow.
      _ASSERT(this%next_value < MAX_ID_VALUE, 'Identifier generator exhausted.')

      id = ID_NAME(this%next_value)
      this%next_value = this%next_value + 1

      _RETURN(_SUCCESS)
   end function generator_next

#undef IDENTITY
#undef ID_CAT_IMPL
#undef ID_CAT
#undef ID_GENERATOR
#undef INVALID_ID
#undef ID_NAME
#undef ID_KIND
