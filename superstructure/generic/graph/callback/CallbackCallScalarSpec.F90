#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackCallScalarSpec: one named call scalar in a CallbackInterface
! (spec/15-callbacks.md): a scalar written into the callback State's own
! Info just before a method call. It declares:
!   - a value type (CallbackValueType), the same vocabulary as attributes.
!     A time interval is a string holding an ISO 8601 duration.
!   - a writer: the invoker (before calling the method) or the handler.
! The slash-free name rule needs the whole interface and is checked there.
!------------------------------------------------------------------------------
module mapl_CallbackCallScalarSpec_mod
   use mapl_CallbackValueType_mod, only: CallbackValueType
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackCallScalarSpec
   public :: CallbackCallScalarWriter
   public :: operator(==)
   public :: operator(/=)

   public :: CALLBACK_WRITER_INVOKER
   public :: CALLBACK_WRITER_HANDLER

   type :: CallbackCallScalarWriter
      private
      integer :: value = -1
   contains
      procedure :: is_known => writer_is_known
      procedure :: to_string => writer_to_string
   end type CallbackCallScalarWriter

   type(CallbackCallScalarWriter), parameter :: &
        CALLBACK_WRITER_INVOKER = CallbackCallScalarWriter(0)
   type(CallbackCallScalarWriter), parameter :: &
        CALLBACK_WRITER_HANDLER = CallbackCallScalarWriter(1)

   interface operator(==)
      module procedure writer_equal
   end interface operator(==)

   interface operator(/=)
      module procedure writer_not_equal
   end interface operator(/=)

   type :: CallbackCallScalarSpec
      private
      character(:), allocatable :: name
      type(CallbackValueType) :: value_type
      type(CallbackCallScalarWriter) :: writer
   contains
      procedure :: get_name => callscalar_get_name
      procedure :: get_value_type => callscalar_get_value_type
      procedure :: get_writer => callscalar_get_writer
   end type CallbackCallScalarSpec

   interface CallbackCallScalarSpec
      module procedure new_CallbackCallScalarSpec
   end interface CallbackCallScalarSpec

contains

   pure logical function writer_equal(left, right) result(equal)
      type(CallbackCallScalarWriter), intent(in) :: left, right

      equal = left%value == right%value
   end function writer_equal

   pure logical function writer_not_equal(left, right) result(not_equal)
      type(CallbackCallScalarWriter), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function writer_not_equal

   ! False for a default-initialized (never assigned) value.
   pure logical function writer_is_known(this) result(is_known)
      class(CallbackCallScalarWriter), intent(in) :: this

      is_known = this%value == 0 .or. this%value == 1
   end function writer_is_known

   pure function writer_to_string(this) result(name)
      class(CallbackCallScalarWriter), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0); name = 'invoker'
      case (1); name = 'handler'
      case default; name = 'unknown'
      end select
   end function writer_to_string

   function failure_message(text, name) result(msg)
      character(*), intent(in) :: text
      character(*), intent(in) :: name
      character(:), allocatable :: msg

      msg = 'CallbackCallScalarSpec: ' // text // ': ' // name
   end function failure_message

   ! The writer has no default: who writes the scalar is part of the
   ! contract and must be stated.
   function new_CallbackCallScalarSpec(name, value_type, writer, rc) &
        result(spec)
      character(*), intent(in) :: name
      type(CallbackValueType), intent(in) :: value_type
      type(CallbackCallScalarWriter), intent(in) :: writer
      integer, optional, intent(out) :: rc
      type(CallbackCallScalarSpec) :: spec

      character(:), allocatable :: msg

      msg = failure_message('call scalar name must not be empty', name)
      _ASSERT(len(name) > 0, msg)
      msg = failure_message('unknown value type', name)
      _ASSERT(value_type%is_known(), msg)
      msg = failure_message('unknown writer', name)
      _ASSERT(writer%is_known(), msg)

      spec%name = name
      spec%value_type = value_type
      spec%writer = writer

      _RETURN(_SUCCESS)
   end function new_CallbackCallScalarSpec

   function callscalar_get_name(this) result(name)
      class(CallbackCallScalarSpec), intent(in) :: this
      character(:), allocatable :: name

      name = this%name
   end function callscalar_get_name

   function callscalar_get_value_type(this) result(value_type)
      class(CallbackCallScalarSpec), intent(in) :: this
      type(CallbackValueType) :: value_type

      value_type = this%value_type
   end function callscalar_get_value_type

   function callscalar_get_writer(this) result(writer)
      class(CallbackCallScalarSpec), intent(in) :: this
      type(CallbackCallScalarWriter) :: writer

      writer = this%writer
   end function callscalar_get_writer

end module mapl_CallbackCallScalarSpec_mod
