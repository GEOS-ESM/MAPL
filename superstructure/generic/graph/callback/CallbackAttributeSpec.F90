#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackAttributeSpec: one named attribute in a CallbackInterface's
! attribute schema (spec/15-callbacks.md). It declares:
!   - a value type (CallbackValueType): integer, real, logical or string,
!     optionally as a list of that type (is_list). Integer and real
!     accept 4-byte or 8-byte values and are held as 8-byte.
!   - a scope: member (hosted on the Info of host_argument's member) or
!     state (hosted on the callback State's own Info). Default member.
!   - host_argument: required for scope member, rejected for scope
!     state. Stored as a plain name only; whether it names a real,
!     non-weight argument needs the whole interface and is checked there,
!     as is the slash-free name rule.
!   - required (default .false.), an optional default value, and, for
!     string and integer types only, an optional set of allowed values.
!
! Defaults and allowed values are unlimited-polymorphic arguments so one
! constructor serves every type. A value of another type family is
! rejected, never converted. String values are stored trimmed. A required
! attribute may not carry a default, and a default must lie in the
! allowed set when there is one; for a list attribute the allowed set
! constrains each element. Duplicate allowed values are kept as given.
!------------------------------------------------------------------------------
module mapl_CallbackAttributeSpec_mod
   use, intrinsic :: iso_fortran_env, only: INT32, INT64, REAL32, REAL64
   use gFTL2_StringVector, only: StringVector
   use mapl_CallbackValueType_mod, only: CallbackValueType, operator(==)
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_INTEGER
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_REAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_LOGICAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_STRING
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackAttributeSpec
   public :: CallbackAttributeScope
   public :: operator(==)
   public :: operator(/=)

   public :: CALLBACK_SCOPE_MEMBER
   public :: CALLBACK_SCOPE_STATE

   type :: CallbackAttributeScope
      private
      integer :: value = -1
   contains
      procedure :: is_known => scope_is_known
      procedure :: to_string => scope_to_string
   end type CallbackAttributeScope

   type(CallbackAttributeScope), parameter :: &
        CALLBACK_SCOPE_MEMBER = CallbackAttributeScope(0)
   type(CallbackAttributeScope), parameter :: &
        CALLBACK_SCOPE_STATE = CallbackAttributeScope(1)

   interface operator(==)
      module procedure scope_equal
   end interface operator(==)

   interface operator(/=)
      module procedure scope_not_equal
   end interface operator(/=)

   ! The values of one default or allowed set; only the component
   ! matching the attribute's value type is populated.
   type :: ValueStore
      integer(INT64), allocatable :: integers(:)
      real(REAL64), allocatable :: reals(:)
      logical, allocatable :: logicals(:)
      type(StringVector) :: strings
   end type ValueStore

   type :: CallbackAttributeSpec
      private
      character(:), allocatable :: name
      type(CallbackValueType) :: value_type = CALLBACK_VALUE_STRING
      logical :: list = .false.
      type(CallbackAttributeScope) :: scope = CALLBACK_SCOPE_MEMBER
      character(:), allocatable :: host_argument
      logical :: required = .false.
      logical :: has_default_value = .false.
      type(ValueStore) :: default_values
      logical :: has_allowed_set = .false.
      type(ValueStore) :: allowed_values
   contains
      procedure :: get_name => attrspec_get_name
      procedure :: get_value_type => attrspec_get_value_type
      procedure :: is_list => attrspec_is_list
      procedure :: get_scope => attrspec_get_scope
      procedure :: has_host_argument => attrspec_has_host_argument
      procedure :: get_host_argument => attrspec_get_host_argument
      procedure :: is_required => attrspec_is_required
      procedure :: has_default => attrspec_has_default
      procedure :: has_allowed_values => attrspec_has_allowed_values
      procedure :: get_default_integer => attrspec_get_default_integer
      procedure :: get_default_real => attrspec_get_default_real
      procedure :: get_default_logical => attrspec_get_default_logical
      procedure :: get_default_string => attrspec_get_default_string
      procedure :: get_default_integers => attrspec_get_default_integers
      procedure :: get_default_reals => attrspec_get_default_reals
      procedure :: get_default_logicals => attrspec_get_default_logicals
      procedure :: get_default_strings => attrspec_get_default_strings
      procedure :: get_allowed_integers => attrspec_get_allowed_integers
      procedure :: get_allowed_strings => attrspec_get_allowed_strings
      procedure, private :: has_scalar_default_of
      procedure, private :: has_list_default_of
      procedure, private :: has_allowed_set_of
   end type CallbackAttributeSpec

   interface CallbackAttributeSpec
      module procedure new_CallbackAttributeSpec
   end interface CallbackAttributeSpec

contains

   pure logical function scope_equal(left, right) result(equal)
      type(CallbackAttributeScope), intent(in) :: left, right

      equal = left%value == right%value
   end function scope_equal

   pure logical function scope_not_equal(left, right) result(not_equal)
      type(CallbackAttributeScope), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function scope_not_equal

   ! False for a default-initialized (never assigned) value.
   pure logical function scope_is_known(this) result(is_known)
      class(CallbackAttributeScope), intent(in) :: this

      is_known = this%value == 0 .or. this%value == 1
   end function scope_is_known

   pure function scope_to_string(this) result(name)
      class(CallbackAttributeScope), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0); name = 'member'
      case (1); name = 'state'
      case default; name = 'unknown'
      end select
   end function scope_to_string

   function failure_message(text, name) result(msg)
      character(*), intent(in) :: text
      character(*), intent(in) :: name
      character(:), allocatable :: msg

      msg = 'CallbackAttributeSpec: ' // text // ': ' // name
   end function failure_message

   ! Only integer and string attributes may carry an allowed set.
   logical function supports_allowed_set(value_type) result(supported)
      type(CallbackValueType), intent(in) :: value_type

      supported = value_type == CALLBACK_VALUE_INTEGER .or. &
           value_type == CALLBACK_VALUE_STRING
   end function supports_allowed_set

   ! default is a scalar default; default_list is a list default and
   ! requires is_list. allowed_values is the allowed set (for lists, the
   ! set each element must lie in). Rejects an inconsistent declaration.
   function new_CallbackAttributeSpec(name, value_type, is_list, scope, &
        host_argument, required, default, default_list, allowed_values, &
        rc) result(spec)
      character(*), intent(in) :: name
      type(CallbackValueType), intent(in) :: value_type
      logical, optional, intent(in) :: is_list
      type(CallbackAttributeScope), optional, intent(in) :: scope
      character(*), optional, intent(in) :: host_argument
      logical, optional, intent(in) :: required
      class(*), optional, intent(in) :: default
      class(*), optional, intent(in) :: default_list(:)
      class(*), optional, intent(in) :: allowed_values(:)
      integer, optional, intent(out) :: rc
      type(CallbackAttributeSpec) :: spec

      character(:), allocatable :: msg
      logical :: ok, two_defaults
      integer :: i

      msg = failure_message('attribute name must not be empty', name)
      _ASSERT(len(name) > 0, msg)
      msg = failure_message('unknown value type', name)
      _ASSERT(value_type%is_known(), msg)

      spec%name = name
      spec%value_type = value_type
      if (present(is_list)) spec%list = is_list
      if (present(scope)) spec%scope = scope
      if (present(required)) spec%required = required

      msg = failure_message('unknown scope', name)
      _ASSERT(spec%scope%is_known(), msg)

      if (spec%scope == CALLBACK_SCOPE_MEMBER) then
         msg = failure_message('member scope needs a host_argument', name)
         _ASSERT(present(host_argument), msg)
         msg = failure_message('host_argument must not be empty', name)
         _ASSERT(len(host_argument) > 0, msg)
         spec%host_argument = host_argument
      else
         msg = failure_message('state scope takes no host_argument', name)
         _ASSERT(.not. present(host_argument), msg)
      end if

      two_defaults = present(default) .and. present(default_list)
      msg = failure_message('give default or default_list, not both', name)
      _ASSERT(.not. two_defaults, msg)

      if (present(allowed_values)) then
         msg = failure_message('allowed values need integer/string', name)
         _ASSERT(supports_allowed_set(value_type), msg)
         msg = failure_message('allowed values must not be empty', name)
         _ASSERT(size(allowed_values) > 0, msg)
         msg = failure_message('allowed value has the wrong type', name)
         do i = 1, size(allowed_values)
            call append_value(value_type, allowed_values(i), &
                 spec%allowed_values, ok)
            _ASSERT(ok, msg)
         end do
         spec%has_allowed_set = .true.
      end if

      if (present(default) .or. present(default_list)) then
         msg = failure_message('a required attribute has no default', name)
         _ASSERT(.not. spec%required, msg)
      end if

      if (present(default)) then
         msg = failure_message('a list attribute needs default_list', name)
         _ASSERT(.not. spec%list, msg)
         msg = failure_message('default has the wrong type', name)
         call append_value(value_type, default, spec%default_values, ok)
         _ASSERT(ok, msg)
         spec%has_default_value = .true.
      end if

      if (present(default_list)) then
         msg = failure_message('default_list needs a list attribute', name)
         _ASSERT(spec%list, msg)
         msg = failure_message('default has the wrong type', name)
         do i = 1, size(default_list)
            call append_value(value_type, default_list(i), &
                 spec%default_values, ok)
            _ASSERT(ok, msg)
         end do
         spec%has_default_value = .true.
      end if

      if (spec%has_default_value .and. spec%has_allowed_set) then
         msg = failure_message('default is not in the allowed values', name)
         ok = all_allowed(value_type, spec%default_values, spec%allowed_values)
         _ASSERT(ok, msg)
      end if

      _RETURN(_SUCCESS)
   end function new_CallbackAttributeSpec

   ! Appends value to store when its dynamic type matches value_type;
   ! ok is .false. otherwise. Integer and real accept 4- and 8-byte
   ! kinds; nothing is converted across type families.
   subroutine append_value(value_type, value, store, ok)
      type(CallbackValueType), intent(in) :: value_type
      class(*), intent(in) :: value
      type(ValueStore), intent(inout) :: store
      logical, intent(out) :: ok

      ok = .true.
      select type (value)
      type is (integer(INT32))
         ok = value_type == CALLBACK_VALUE_INTEGER
         if (ok) call append_integer(store, int(value, INT64))
      type is (integer(INT64))
         ok = value_type == CALLBACK_VALUE_INTEGER
         if (ok) call append_integer(store, value)
      type is (real(REAL32))
         ok = value_type == CALLBACK_VALUE_REAL
         if (ok) call append_real(store, real(value, REAL64))
      type is (real(REAL64))
         ok = value_type == CALLBACK_VALUE_REAL
         if (ok) call append_real(store, value)
      type is (logical)
         ok = value_type == CALLBACK_VALUE_LOGICAL
         if (ok) call append_logical(store, value)
      type is (character(*))
         ok = value_type == CALLBACK_VALUE_STRING
         if (ok) call store%strings%push_back(trim(value))
      class default
         ok = .false.
      end select
   end subroutine append_value

   subroutine append_integer(store, value)
      type(ValueStore), intent(inout) :: store
      integer(INT64), intent(in) :: value

      if (.not. allocated(store%integers)) allocate(store%integers(0))
      store%integers = [store%integers, value]
   end subroutine append_integer

   subroutine append_real(store, value)
      type(ValueStore), intent(inout) :: store
      real(REAL64), intent(in) :: value

      if (.not. allocated(store%reals)) allocate(store%reals(0))
      store%reals = [store%reals, value]
   end subroutine append_real

   subroutine append_logical(store, value)
      type(ValueStore), intent(inout) :: store
      logical, intent(in) :: value

      if (.not. allocated(store%logicals)) allocate(store%logicals(0))
      store%logicals = [store%logicals, value]
   end subroutine append_logical

   ! True when every default element lies in the allowed set.
   logical function all_allowed(value_type, defaults, allowed) result(ok)
      type(CallbackValueType), intent(in) :: value_type
      type(ValueStore), intent(in) :: defaults
      type(ValueStore), intent(in) :: allowed

      integer :: i, j
      logical :: found

      ok = .true.
      if (value_type == CALLBACK_VALUE_INTEGER) then
         if (.not. allocated(defaults%integers)) return
         do i = 1, size(defaults%integers)
            ok = ok .and. any(allowed%integers == defaults%integers(i))
         end do
      else if (value_type == CALLBACK_VALUE_STRING) then
         do i = 1, defaults%strings%size()
            found = .false.
            do j = 1, allowed%strings%size()
               if (defaults%strings%of(i) == allowed%strings%of(j)) then
                  found = .true.
               end if
            end do
            ok = ok .and. found
         end do
      end if
   end function all_allowed

   logical function has_scalar_default_of(this, value_type) result(has)
      class(CallbackAttributeSpec), intent(in) :: this
      type(CallbackValueType), intent(in) :: value_type

      has = this%has_default_value .and. .not. this%list .and. &
           this%value_type == value_type
   end function has_scalar_default_of

   logical function has_list_default_of(this, value_type) result(has)
      class(CallbackAttributeSpec), intent(in) :: this
      type(CallbackValueType), intent(in) :: value_type

      has = this%has_default_value .and. this%list .and. &
           this%value_type == value_type
   end function has_list_default_of

   logical function has_allowed_set_of(this, value_type) result(has)
      class(CallbackAttributeSpec), intent(in) :: this
      type(CallbackValueType), intent(in) :: value_type

      has = this%has_allowed_set .and. this%value_type == value_type
   end function has_allowed_set_of

   function attrspec_get_name(this) result(name)
      class(CallbackAttributeSpec), intent(in) :: this
      character(:), allocatable :: name

      name = this%name
   end function attrspec_get_name

   function attrspec_get_value_type(this) result(value_type)
      class(CallbackAttributeSpec), intent(in) :: this
      type(CallbackValueType) :: value_type

      value_type = this%value_type
   end function attrspec_get_value_type

   logical function attrspec_is_list(this) result(is_list)
      class(CallbackAttributeSpec), intent(in) :: this

      is_list = this%list
   end function attrspec_is_list

   function attrspec_get_scope(this) result(scope)
      class(CallbackAttributeSpec), intent(in) :: this
      type(CallbackAttributeScope) :: scope

      scope = this%scope
   end function attrspec_get_scope

   logical function attrspec_has_host_argument(this) result(has_host)
      class(CallbackAttributeSpec), intent(in) :: this

      has_host = allocated(this%host_argument)
   end function attrspec_has_host_argument

   ! Returns '' for a state-scoped attribute.
   function attrspec_get_host_argument(this) result(host_argument)
      class(CallbackAttributeSpec), intent(in) :: this
      character(:), allocatable :: host_argument

      host_argument = ''
      if (allocated(this%host_argument)) host_argument = this%host_argument
   end function attrspec_get_host_argument

   logical function attrspec_is_required(this) result(is_required)
      class(CallbackAttributeSpec), intent(in) :: this

      is_required = this%required
   end function attrspec_is_required

   logical function attrspec_has_default(this) result(has_default)
      class(CallbackAttributeSpec), intent(in) :: this

      has_default = this%has_default_value
   end function attrspec_has_default

   logical function attrspec_has_allowed_values(this) result(has_allowed)
      class(CallbackAttributeSpec), intent(in) :: this

      has_allowed = this%has_allowed_set
   end function attrspec_has_allowed_values

   function attrspec_get_default_integer(this, rc) result(value)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer(INT64) :: value

      character(:), allocatable :: msg

      value = 0_INT64
      msg = failure_message('no scalar integer default', this%name)
      _ASSERT(this%has_scalar_default_of(CALLBACK_VALUE_INTEGER), msg)
      value = this%default_values%integers(1)

      _RETURN(_SUCCESS)
   end function attrspec_get_default_integer

   function attrspec_get_default_real(this, rc) result(value)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      real(REAL64) :: value

      character(:), allocatable :: msg

      value = 0._REAL64
      msg = failure_message('no scalar real default', this%name)
      _ASSERT(this%has_scalar_default_of(CALLBACK_VALUE_REAL), msg)
      value = this%default_values%reals(1)

      _RETURN(_SUCCESS)
   end function attrspec_get_default_real

   function attrspec_get_default_logical(this, rc) result(value)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      logical :: value

      character(:), allocatable :: msg

      value = .false.
      msg = failure_message('no scalar logical default', this%name)
      _ASSERT(this%has_scalar_default_of(CALLBACK_VALUE_LOGICAL), msg)
      value = this%default_values%logicals(1)

      _RETURN(_SUCCESS)
   end function attrspec_get_default_logical

   function attrspec_get_default_string(this, rc) result(value)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      character(:), allocatable :: value

      character(:), allocatable :: msg

      value = ''
      msg = failure_message('no scalar string default', this%name)
      _ASSERT(this%has_scalar_default_of(CALLBACK_VALUE_STRING), msg)
      value = this%default_values%strings%of(1)

      _RETURN(_SUCCESS)
   end function attrspec_get_default_string

   function attrspec_get_default_integers(this, rc) result(values)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer(INT64), allocatable :: values(:)

      character(:), allocatable :: msg

      allocate(values(0))
      msg = failure_message('no integer list default', this%name)
      _ASSERT(this%has_list_default_of(CALLBACK_VALUE_INTEGER), msg)
      if (allocated(this%default_values%integers)) then
         values = this%default_values%integers
      end if

      _RETURN(_SUCCESS)
   end function attrspec_get_default_integers

   function attrspec_get_default_reals(this, rc) result(values)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      real(REAL64), allocatable :: values(:)

      character(:), allocatable :: msg

      allocate(values(0))
      msg = failure_message('no real list default', this%name)
      _ASSERT(this%has_list_default_of(CALLBACK_VALUE_REAL), msg)
      if (allocated(this%default_values%reals)) then
         values = this%default_values%reals
      end if

      _RETURN(_SUCCESS)
   end function attrspec_get_default_reals

   function attrspec_get_default_logicals(this, rc) result(values)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      logical, allocatable :: values(:)

      character(:), allocatable :: msg

      allocate(values(0))
      msg = failure_message('no logical list default', this%name)
      _ASSERT(this%has_list_default_of(CALLBACK_VALUE_LOGICAL), msg)
      if (allocated(this%default_values%logicals)) then
         values = this%default_values%logicals
      end if

      _RETURN(_SUCCESS)
   end function attrspec_get_default_logicals

   function attrspec_get_default_strings(this, rc) result(values)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(StringVector) :: values

      character(:), allocatable :: msg

      msg = failure_message('no string list default', this%name)
      _ASSERT(this%has_list_default_of(CALLBACK_VALUE_STRING), msg)
      values = this%default_values%strings

      _RETURN(_SUCCESS)
   end function attrspec_get_default_strings

   function attrspec_get_allowed_integers(this, rc) result(values)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer(INT64), allocatable :: values(:)

      character(:), allocatable :: msg

      allocate(values(0))
      msg = failure_message('no integer allowed values', this%name)
      _ASSERT(this%has_allowed_set_of(CALLBACK_VALUE_INTEGER), msg)
      values = this%allowed_values%integers

      _RETURN(_SUCCESS)
   end function attrspec_get_allowed_integers

   function attrspec_get_allowed_strings(this, rc) result(values)
      class(CallbackAttributeSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(StringVector) :: values

      character(:), allocatable :: msg

      msg = failure_message('no string allowed values', this%name)
      _ASSERT(this%has_allowed_set_of(CALLBACK_VALUE_STRING), msg)
      values = this%allowed_values%strings

      _RETURN(_SUCCESS)
   end function attrspec_get_allowed_strings

end module mapl_CallbackAttributeSpec_mod
