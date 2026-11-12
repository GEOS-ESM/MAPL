#include "MAPL.h"

#define IDENTITY(x) x
#define ID_CAT_IMPL(a,b) IDENTITY(a)IDENTITY(b)
#define ID_CAT(a,b) ID_CAT_IMPL(a,b)
#define ID_GENERATOR ID_CAT(ID_NAME,Generator)
#define INVALID_ID ID_CAT(INVALID_,ID_NAME)


   use, intrinsic :: iso_fortran_env, only: INT32
   implicit none(type,external)
   private

   public :: ID_NAME
   public :: ID_GENERATOR
   public :: INVALID_ID
   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)

   integer, parameter :: ID_KIND_ = INT32
   integer(ID_KIND_), parameter :: INVALID_VALUE = -1
   integer(ID_KIND_), parameter :: DEFAULT_VALUE = 0

   type :: ID_NAME
      private
      integer(ID_KIND_) :: value = INVALID_VALUE
   contains
      procedure :: is_valid => id_is_valid
      procedure :: to_string => id_to_string
   end type ID_NAME

   type(ID_NAME), parameter :: INVALID_ID = ID_NAME(INVALID_VALUE)
#if defined(DEFAULT_ID)
   type(ID_NAME), parameter, public :: DEFAULT_ID = ID_NAME(DEFAULT_VALUE)
#endif

   type :: ID_GENERATOR
      private
      integer(ID_KIND_) :: next_value = 1
   contains
      procedure :: next => generator_next
   end type ID_GENERATOR

   interface operator(==)
      module procedure id_equal
   end interface operator(==)
   interface operator(/=)
      module procedure id_not_equal
   end interface operator(/=)
   interface operator(<)
      module procedure id_less_than
   end interface operator(<)

contains

   pure logical function id_is_valid(this)
      class(ID_NAME), intent(in) :: this
      id_is_valid = this%value /= INVALID_VALUE
   end function id_is_valid

   function id_to_string(this) result(value)
      class(ID_NAME), intent(in) :: this
      character(:), allocatable :: value
      character(32) :: buffer

      if (.not. this%is_valid()) then
         value = ''
         return
      end if

      write(buffer, '(I0)') this%value
      value = trim(adjustl(buffer))
   end function id_to_string

   pure logical function id_equal(left, right)
      type(ID_NAME), intent(in) :: left, right
      id_equal = left%value == right%value
   end function id_equal

   pure logical function id_not_equal(left, right)
      type(ID_NAME), intent(in) :: left, right
      id_not_equal = .not. (left == right)
   end function id_not_equal

   pure logical function id_less_than(left, right)
      type(ID_NAME), intent(in) :: left, right
      id_less_than = left%value < right%value
   end function id_less_than

   function generator_next(this, status) result(id)
      class(ID_GENERATOR), intent(inout) :: this
      integer, optional, intent(out) :: status
      type(ID_NAME) :: id

      if (this%next_value >= huge(this%next_value)) then
         id = INVALID_ID
         if (present(status)) status = 1
         return
      end if

      id%value = this%next_value
      this%next_value = this%next_value + 1
      if (present(status)) status = 0
   end function generator_next

#undef INVALID_ID
#undef ID_GENERATOR
#undef ID_CAT
#undef ID_CAT_IMPL
#undef IDENTITY
