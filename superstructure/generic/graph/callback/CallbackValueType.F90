!------------------------------------------------------------------------------
! CallbackValueType: the value type of a callback attribute or call scalar
! (integer, real, logical or string). Shared by every callback declaration
! type that carries a value, so each uses one vocabulary. Whether the value
! is a list is declared separately by the user of this type.
!------------------------------------------------------------------------------
module mapl_CallbackValueType_mod
   implicit none(type, external)
   private

   public :: CallbackValueType
   public :: operator(==)
   public :: operator(/=)

   public :: CALLBACK_VALUE_INTEGER
   public :: CALLBACK_VALUE_REAL
   public :: CALLBACK_VALUE_LOGICAL
   public :: CALLBACK_VALUE_STRING

   type :: CallbackValueType
      private
      integer :: value = -1
   contains
      procedure :: is_known => valuetype_is_known
      procedure :: to_string => valuetype_to_string
   end type CallbackValueType

   type(CallbackValueType), parameter :: &
        CALLBACK_VALUE_INTEGER = CallbackValueType(0)
   type(CallbackValueType), parameter :: &
        CALLBACK_VALUE_REAL = CallbackValueType(1)
   type(CallbackValueType), parameter :: &
        CALLBACK_VALUE_LOGICAL = CallbackValueType(2)
   type(CallbackValueType), parameter :: &
        CALLBACK_VALUE_STRING = CallbackValueType(3)

   interface operator(==)
      module procedure valuetype_equal
   end interface operator(==)

   interface operator(/=)
      module procedure valuetype_not_equal
   end interface operator(/=)

contains

   pure logical function valuetype_equal(left, right) result(equal)
      type(CallbackValueType), intent(in) :: left, right

      equal = left%value == right%value
   end function valuetype_equal

   pure logical function valuetype_not_equal(left, right) &
        result(not_equal)
      type(CallbackValueType), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function valuetype_not_equal

   ! False for a default-initialized (never assigned) value.
   pure logical function valuetype_is_known(this) result(is_known)
      class(CallbackValueType), intent(in) :: this

      is_known = this%value >= 0 .and. this%value <= 3
   end function valuetype_is_known

   pure function valuetype_to_string(this) result(name)
      class(CallbackValueType), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0); name = 'integer'
      case (1); name = 'real'
      case (2); name = 'logical'
      case (3); name = 'string'
      case default; name = 'unknown'
      end select
   end function valuetype_to_string

end module mapl_CallbackValueType_mod
