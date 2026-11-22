#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackAttributeAccess: user-facing accessors for one callback State's
! declared attributes and call scalars (spec/15-callbacks.md), resolved
! through the CallbackInterface named by the State's own recorded
! interface name (record_callback_interface_name writes that name,
! through MAPL_InfoSetCallbackInterface).
!
! MAPL_CallbackGetAttribute(state, name, value, unusable, member, rc)
! looks up name in that interface's attribute schema, reads from the
! given member's Info (scope member) or the State's own Info (scope
! state), and returns the declared default when the key is absent. The
! default is not logged here: the conformance checker
! (CallbackStateConformance.F90) already logs "default used" once per
! callback State and attribute, and this accessor can run every method
! execution. Only scalar attributes are supported; a list attribute is
! rejected.
!
! MAPL_CallbackSetScalar(state, name, value, rc) checks name and
! declared type, then writes value into the State's own Info under the
! same interface-qualified key as an attribute would use. A call
! scalar's declared writer (Handler or Invoker) is a documented
! convention the caller is trusted to follow; this accessor does not
! check it.
!
! value is unlimited polymorphic in both accessors, dispatched by its
! dynamic type (plain integer, real(ESMF_KIND_R4), real(ESMF_KIND_R8),
! logical or character(*)), the same style CallbackAttributeSpec uses
! for its default and allowed-value arguments. For a string attribute,
! the value actual argument passed to MAPL_CallbackGetAttribute must be
! a fixed-length character variable long enough to hold the result
! (not a deferred-length allocatable): select type cannot reallocate a
! polymorphic actual argument to a new length. A buffer too short to
! hold the result is a reported error, not a silent truncation.
!
! No graph or component dependency, so both accessors are testable on
! hand-built States.
!------------------------------------------------------------------------------
module mapl_CallbackAttributeAccess_mod
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   use esmf, only: ESMF_State, ESMF_StateGet
   use esmf, only: ESMF_MAXSTR
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   use mapl_InfoUtilities_mod, only: MAPL_InfoHasCallbackInterface
   use mapl_InfoUtilities_mod, only: MAPL_InfoGetCallbackInterface
   use mapl_InfoUtilities_mod, only: MAPL_InfoSetCallbackInterface
   use mapl_InfoUtilities_mod, only: MAPL_InfoGetShared, MAPL_InfoSetShared
   use mapl_InfoUtilities_mod, only: MAPL_InfoGetValueKind
   use mapl_InfoUtilities_mod, only: InfoValueKind, operator(==)
   use mapl_InfoUtilities_mod, only: INFO_VALUE_ABSENT, INFO_VALUE_INTEGER
   use mapl_InfoUtilities_mod, only: INFO_VALUE_REAL, INFO_VALUE_LOGICAL, INFO_VALUE_STRING
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceRegistry_mod, only: get_callback_interface_by_name
   use mapl_CallbackAttributeSpec_mod, only: CallbackAttributeSpec, CALLBACK_SCOPE_STATE, &
        operator(==)
   use mapl_CallbackAttributeSpecMap_mod, only: CallbackAttributeSpecMap, &
        CallbackAttributeSpecMapIterator, operator(/=)
   use mapl_CallbackCallScalarSpec_mod, only: CallbackCallScalarSpec
   use mapl_CallbackCallScalarSpecMap_mod, only: CallbackCallScalarSpecMap, &
        CallbackCallScalarSpecMapIterator, operator(/=)
   use mapl_CallbackValueType_mod, only: CallbackValueType, operator(==)
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_INTEGER, CALLBACK_VALUE_REAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_LOGICAL, CALLBACK_VALUE_STRING
   use mapl_CallbackStateConformance_mod, only: integer_text
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: record_callback_interface_name
   public :: MAPL_CallbackGetAttribute
   public :: MAPL_CallbackSetScalar

contains

   ! Writes the interface name under which name/type/scope lookups in
   ! this module resolve, into the callback State's own Info.
   subroutine record_callback_interface_name(state, interface_name, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: interface_name
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_InfoSetCallbackInterface(state, interface_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine record_callback_interface_name

   function get_recorded_interface_name(state, rc) result(interface_name)
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc
      character(:), allocatable :: interface_name

      character(:), allocatable :: msg
      character(ESMF_MAXSTR) :: state_name
      logical :: is_present
      integer :: status

      interface_name = ''
      is_present = MAPL_InfoHasCallbackInterface(state, _RC)

      state_name = ''
      call ESMF_StateGet(state, name=state_name, rc=status)
      msg = "CallbackAttributeAccess: callback State '" // trim(state_name) // &
           "' has no recorded interface name"
      _ASSERT(is_present, msg)

      call MAPL_InfoGetCallbackInterface(state, interface_name, _RC)

      _RETURN(_SUCCESS)
   end function get_recorded_interface_name

   function attribute_names_sorted(iface) result(names)
      type(CallbackInterface), intent(in) :: iface
      character(ESMF_MAXSTR), allocatable :: names(:)

      type(CallbackAttributeSpecMap), target :: attributes
      type(CallbackAttributeSpecMapIterator) :: iter
      integer :: i

      attributes = iface%get_attributes()
      allocate(names(attributes%size()))
      i = 0
      iter = attributes%ftn_begin()
      do while (iter /= attributes%ftn_end())
         call iter%next()
         i = i + 1
         names(i) = iter%first()
      end do
   end function attribute_names_sorted

   function call_scalar_names_sorted(iface) result(names)
      type(CallbackInterface), intent(in) :: iface
      character(ESMF_MAXSTR), allocatable :: names(:)

      type(CallbackCallScalarSpecMap), target :: call_scalars
      type(CallbackCallScalarSpecMapIterator) :: iter
      integer :: i

      call_scalars = iface%get_call_scalars()
      allocate(names(call_scalars%size()))
      i = 0
      iter = call_scalars%ftn_begin()
      do while (iter /= call_scalars%ftn_end())
         call iter%next()
         i = i + 1
         names(i) = iter%first()
      end do
   end function call_scalar_names_sorted

   ! Comma-space joins names(:) for a "legal ... names:" message.
   function join(names) result(joined)
      character(*), intent(in) :: names(:)
      character(:), allocatable :: joined

      integer :: i

      joined = ''
      do i = 1, size(names)
         if (i > 1) joined = joined // ', '
         joined = joined // trim(names(i))
      end do
   end function join

   logical function type_compatible_scalar(value_type, stored_kind) result(ok)
      type(CallbackValueType), intent(in) :: value_type
      type(InfoValueKind), intent(in) :: stored_kind

      ok = (value_type == CALLBACK_VALUE_INTEGER .and. stored_kind == INFO_VALUE_INTEGER) .or. &
           (value_type == CALLBACK_VALUE_REAL .and. stored_kind == INFO_VALUE_REAL) .or. &
           (value_type == CALLBACK_VALUE_LOGICAL .and. stored_kind == INFO_VALUE_LOGICAL) .or. &
           (value_type == CALLBACK_VALUE_STRING .and. stored_kind == INFO_VALUE_STRING)
   end function type_compatible_scalar

   ! Reads a declared scalar attribute, resolving the host from scope
   ! and, for scope member, the member argument (the caller's name for
   ! the host bound to the attribute's declared host_argument).
   subroutine MAPL_CallbackGetAttribute(state, name, value, unusable, member, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: name
      class(*), intent(inout) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: member
      integer, optional, intent(out) :: rc

      character(:), allocatable :: interface_name, host, key, msg
      type(CallbackInterface) :: iface
      type(CallbackAttributeSpec) :: spec
      type(CallbackValueType) :: value_type
      type(InfoValueKind) :: stored_kind
      logical :: use_default
      integer(INT64) :: default_integer
      real(REAL64) :: default_real
      character(:), allocatable :: string_value
      integer :: status

      interface_name = get_recorded_interface_name(state, _RC)
      iface = get_callback_interface_by_name(interface_name, _RC)

      if (.not. iface%is_attribute(name)) then
         msg = "MAPL_CallbackGetAttribute: '" // name // "' is not a declared attribute of " // &
              "interface '" // interface_name // "'; legal attribute names: " // &
              join(attribute_names_sorted(iface))
         _FAIL(msg)
      end if

      spec = iface%get_attribute(name, _RC)
      msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is a list; only scalar " // &
           "attributes are supported"
      _ASSERT(.not. spec%is_list(), msg)

      key = interface_name // '/' // name
      if (spec%get_scope() == CALLBACK_SCOPE_STATE) then
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is State-scoped; " // &
              "the member argument must not be given"
         _ASSERT(.not. present(member), msg)
         host = ''
         call MAPL_InfoGetValueKind(state, key, stored_kind, _RC)
      else
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is member-scoped; " // &
              "the host member name must be given"
         _ASSERT(present(member), msg)
         host = member
         call MAPL_InfoGetValueKind(state, host, key, stored_kind, _RC)
      end if

      value_type = spec%get_value_type()
      use_default = .false.
      if (stored_kind == INFO_VALUE_ABSENT) then
         msg = "MAPL_CallbackGetAttribute: '" // key // "' is not set and has no default"
         _ASSERT(spec%has_default(), msg)
         use_default = .true.
      else
         msg = "MAPL_CallbackGetAttribute: '" // key // "' is stored as '" // &
              stored_kind%to_string() // "', declared as '" // value_type%to_string() // "'"
         _ASSERT(type_compatible_scalar(value_type, stored_kind), msg)
      end if

      select type (value)
      type is (integer)
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is declared '" // &
              value_type%to_string() // "'; the value argument is integer"
         _ASSERT(value_type == CALLBACK_VALUE_INTEGER, msg)
         if (use_default) then
            default_integer = spec%get_default_integer(_RC)
            value = int(default_integer)
         else if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, value, _RC)
         end if
      type is (real(ESMF_KIND_R4))
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is declared '" // &
              value_type%to_string() // "'; the value argument is real"
         _ASSERT(value_type == CALLBACK_VALUE_REAL, msg)
         if (use_default) then
            default_real = spec%get_default_real(_RC)
            value = real(default_real, ESMF_KIND_R4)
         else if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, value, _RC)
         end if
      type is (real(ESMF_KIND_R8))
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is declared '" // &
              value_type%to_string() // "'; the value argument is real"
         _ASSERT(value_type == CALLBACK_VALUE_REAL, msg)
         if (use_default) then
            value = spec%get_default_real(_RC)
         else if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, value, _RC)
         end if
      type is (logical)
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is declared '" // &
              value_type%to_string() // "'; the value argument is logical"
         _ASSERT(value_type == CALLBACK_VALUE_LOGICAL, msg)
         if (use_default) then
            value = spec%get_default_logical(_RC)
         else if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, value, _RC)
         end if
      type is (character(*))
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' is declared '" // &
              value_type%to_string() // "'; the value argument is a string"
         _ASSERT(value_type == CALLBACK_VALUE_STRING, msg)
         if (use_default) then
            string_value = spec%get_default_string(_RC)
         else if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=string_value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, string_value, _RC)
         end if
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // "' value is " // &
              integer_text(int(len(string_value), INT64)) // " characters; the value " // &
              "argument is only " // integer_text(int(len(value), INT64)) // " characters"
         _ASSERT(len(value) >= len(string_value), msg)
         value = string_value
      class default
         msg = "MAPL_CallbackGetAttribute: attribute '" // name // &
              "' - unsupported value argument type"
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine MAPL_CallbackGetAttribute

   ! Writes a declared call scalar into the callback State's own Info,
   ! under the same interface-qualified key an attribute of that name
   ! would use. Checks only the name and the declared type; which side
   ! (Handler or Invoker) may write this call scalar is a documented
   ! convention on the call scalar's own declaration, not checked here.
   subroutine MAPL_CallbackSetScalar(state, name, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: name
      class(*), intent(in) :: value
      integer, optional, intent(out) :: rc

      character(:), allocatable :: interface_name, key, msg
      type(CallbackInterface) :: iface
      type(CallbackCallScalarSpec) :: spec
      type(CallbackValueType) :: value_type
      integer :: status

      interface_name = get_recorded_interface_name(state, _RC)
      iface = get_callback_interface_by_name(interface_name, _RC)

      if (.not. iface%is_call_scalar(name)) then
         msg = "MAPL_CallbackSetScalar: '" // name // "' is not a declared call scalar of " // &
              "interface '" // interface_name // "'; legal call scalar names: " // &
              join(call_scalar_names_sorted(iface))
         _FAIL(msg)
      end if

      spec = iface%get_call_scalar(name, _RC)

      value_type = spec%get_value_type()
      key = interface_name // '/' // name

      select type (value)
      type is (integer)
         msg = "MAPL_CallbackSetScalar: call scalar '" // name // "' is declared '" // &
              value_type%to_string() // "'; given an integer value"
         _ASSERT(value_type == CALLBACK_VALUE_INTEGER, msg)
         call MAPL_InfoSetShared(state, key=key, value=value, _RC)
      type is (real(ESMF_KIND_R4))
         msg = "MAPL_CallbackSetScalar: call scalar '" // name // "' is declared '" // &
              value_type%to_string() // "'; given a real value"
         _ASSERT(value_type == CALLBACK_VALUE_REAL, msg)
         call MAPL_InfoSetShared(state, key=key, value=value, _RC)
      type is (real(ESMF_KIND_R8))
         msg = "MAPL_CallbackSetScalar: call scalar '" // name // "' is declared '" // &
              value_type%to_string() // "'; given a real value"
         _ASSERT(value_type == CALLBACK_VALUE_REAL, msg)
         call MAPL_InfoSetShared(state, key=key, value=value, _RC)
      type is (logical)
         msg = "MAPL_CallbackSetScalar: call scalar '" // name // "' is declared '" // &
              value_type%to_string() // "'; given a logical value"
         _ASSERT(value_type == CALLBACK_VALUE_LOGICAL, msg)
         call MAPL_InfoSetShared(state, key=key, value=value, _RC)
      type is (character(*))
         msg = "MAPL_CallbackSetScalar: call scalar '" // name // "' is declared '" // &
              value_type%to_string() // "'; given a string value"
         _ASSERT(value_type == CALLBACK_VALUE_STRING, msg)
         call MAPL_InfoSetShared(state, key=key, value=value, _RC)
      class default
         msg = "MAPL_CallbackSetScalar: call scalar '" // name // &
              "' - unsupported value argument type"
         _FAIL(msg)
      end select

      _RETURN(_SUCCESS)
   end subroutine MAPL_CallbackSetScalar

end module mapl_CallbackAttributeAccess_mod
