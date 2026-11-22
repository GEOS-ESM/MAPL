#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackInterface: a reusable callback contract (spec/15-callbacks.md).
! It carries a version (at least 1), a one-line summary and an optional
! description, and these declarations, each keyed by name:
!   - arguments (CallbackArgumentSpec), declared by declare_argument
!   - methods (CallbackMethodSpec), declared by declare_method, with
!     per-argument access set by set_method_argument_access
!   - attributes (CallbackAttributeSpec), by declare_attribute
!   - call scalars (CallbackCallScalarSpec), by declare_call_scalar
!   - rules (CallbackRule, in declaration order), by declare_rule
! Each declare_* rejects a duplicate name and leaves the existing
! declaration unchanged (no silent replace).
!
! This is the one place that sees every declaration together, so it owns
! the checks that span them: names contain no '/' (it separates the
! interface namespace from the key in Info); a member-scoped attribute's
! host argument is a declared, required argument that is not a weight; a
! weight argument is extensive, names a declared non-extensive argument,
! and is required whenever that argument is; a rule names a declared
! attribute, a value legal for it, and declared optional arguments.
!
! Declarations are checked against what is already declared, so a name
! must be declared before anything refers to it: arguments before the
! weights, attributes and rules that name them, attributes before rules.
! A declaration cannot change an earlier one, so no later check is needed.
!
! The interface's own name is not stored here: it is the service name
! under which CallbackInterfaceRegistry registers the interface.
!
! A default-initialized value has version 0 and no summary. Build one
! with make_CallbackInterface to have version and summary checked.
!
! set_method_argument_access requires both the method and the argument
! to be declared, then mutates the method's CallbackMethodSpec in place
! through %at() pointer access, the same declare-then-mutate idiom as
! MethodGraphNode.
!------------------------------------------------------------------------------
module mapl_CallbackInterface_mod
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap, &
        CallbackArgumentSpecMapIterator, operator(/=)
   use mapl_CallbackAttributeSpec_mod, only: CallbackAttributeSpec, &
        CallbackAttributeScope
   use mapl_CallbackAttributeSpecMap_mod, only: CallbackAttributeSpecMap, &
        CallbackAttributeSpecMapIterator, operator(/=)
   use mapl_CallbackCallScalarSpec_mod, only: CallbackCallScalarSpec, &
        CallbackCallScalarWriter
   use mapl_CallbackCallScalarSpecMap_mod, only: CallbackCallScalarSpecMap
   use mapl_CallbackRule_mod, only: CallbackRule, make_CallbackRule
   use mapl_CallbackRuleVector_mod, only: CallbackRuleVector
   use mapl_CallbackValueType_mod, only: CallbackValueType, operator(==)
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_INTEGER
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_REAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_LOGICAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_STRING
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec
   use mapl_CallbackMethodSpecMap_mod, only: CallbackMethodSpecMap
   use mapl_CallbackArgumentOptions_mod, only: CallbackArgumentOptions
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use gFTL2_StringVector, only: StringVector
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackInterface
   public :: make_CallbackInterface

   type :: CallbackInterface
      private
      integer :: version = 0
      character(:), allocatable :: summary
      character(:), allocatable :: description
      type(CallbackArgumentSpecMap) :: arguments
      type(CallbackMethodSpecMap) :: methods
      type(CallbackAttributeSpecMap) :: attributes
      type(CallbackCallScalarSpecMap) :: call_scalars
      type(CallbackRuleVector) :: rules
   contains
      procedure :: get_version => interface_get_version
      procedure :: get_summary => interface_get_summary
      procedure :: has_description => interface_has_description
      procedure :: get_description => interface_get_description
      procedure :: declare_argument => interface_declare_argument
      procedure :: is_argument => interface_is_argument
      procedure :: get_argument => interface_get_argument
      procedure :: get_arguments => interface_get_arguments
      procedure :: declare_method => interface_declare_method
      procedure :: is_method => interface_is_method
      procedure :: get_method => interface_get_method
      procedure :: get_methods => interface_get_methods
      procedure :: set_method_argument_access => &
           interface_set_method_argument_access
      procedure :: declare_attribute => interface_declare_attribute
      procedure :: is_attribute => interface_is_attribute
      procedure :: get_attribute => interface_get_attribute
      procedure :: get_attributes => interface_get_attributes
      procedure :: declare_call_scalar => interface_declare_call_scalar
      procedure :: is_call_scalar => interface_is_call_scalar
      procedure :: get_call_scalar => interface_get_call_scalar
      procedure :: get_call_scalars => interface_get_call_scalars
      procedure :: declare_rule => interface_declare_rule
      procedure :: get_rules => interface_get_rules
   end type CallbackInterface

   interface CallbackInterface
      module procedure new_CallbackInterface
   end interface CallbackInterface

contains

   ! Stores what it is given and checks nothing; make_CallbackInterface is
   ! the checking form.
   function new_CallbackInterface(version, summary, description) &
        result(iface)
      integer, intent(in) :: version
      character(*), intent(in) :: summary
      character(*), optional, intent(in) :: description
      type(CallbackInterface) :: iface

      iface%version = version
      iface%summary = summary
      if (present(description)) iface%description = description
   end function new_CallbackInterface

   ! Rejects a version below 1, and a summary that is blank or spans more
   ! than one line. The result is a stored interface even when a check
   ! fails.
   function make_CallbackInterface(version, summary, description, rc) &
        result(iface)
      integer, intent(in) :: version
      character(*), intent(in) :: summary
      character(*), optional, intent(in) :: description
      integer, optional, intent(out) :: rc
      type(CallbackInterface) :: iface

      character(:), allocatable :: msg

      iface = CallbackInterface(version, summary, description)

      msg = 'CallbackInterface: version must be at least 1'
      _ASSERT(version >= 1, msg)
      msg = 'CallbackInterface: summary must not be blank'
      _ASSERT(len_trim(summary) > 0, msg)
      msg = 'CallbackInterface: summary must be a single line'
      _ASSERT(scan(summary, new_line('a') // achar(13)) == 0, msg)

      _RETURN(_SUCCESS)
   end function make_CallbackInterface

   integer function interface_get_version(this) result(version)
      class(CallbackInterface), intent(in) :: this

      version = this%version
   end function interface_get_version

   ! Returns '' for a default-initialized interface.
   function interface_get_summary(this) result(summary)
      class(CallbackInterface), intent(in) :: this
      character(:), allocatable :: summary

      summary = ''
      if (allocated(this%summary)) summary = this%summary
   end function interface_get_summary

   logical function interface_has_description(this) result(has_description)
      class(CallbackInterface), intent(in) :: this

      has_description = allocated(this%description)
   end function interface_has_description

   ! Returns '' when no description was given.
   function interface_get_description(this) result(description)
      class(CallbackInterface), intent(in) :: this
      character(:), allocatable :: description

      description = ''
      if (allocated(this%description)) description = this%description
   end function interface_get_description

   ! '/' separates the interface namespace from the key in Info, so no
   ! declared name may contain one.
   subroutine check_name_has_no_slash(kind_of_name, name, rc)
      character(*), intent(in) :: kind_of_name
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      character(:), allocatable :: msg

      msg = 'CallbackInterface: ' // kind_of_name // &
           ' name must not contain "/": ' // name
      _ASSERT(index(name, '/') == 0, msg)

      _RETURN(_SUCCESS)
   end subroutine check_name_has_no_slash

   ! The declared argument names, comma separated, in sorted order.
   function argument_names_text(this) result(text)
      class(CallbackInterface), target, intent(in) :: this
      character(:), allocatable :: text

      type(CallbackArgumentSpecMapIterator) :: iter

      text = ''
      associate (e => this%arguments%ftn_end())
         iter = this%arguments%ftn_begin()
         do while (iter /= e)
            call iter%next()
            if (len(text) > 0) text = text // ', '
            text = text // iter%first()
         end do
      end associate
      if (len(text) == 0) text = '(none)'
   end function argument_names_text

   ! The declared attribute names, comma separated, in sorted order.
   function attribute_names_text(this) result(text)
      class(CallbackInterface), target, intent(in) :: this
      character(:), allocatable :: text

      type(CallbackAttributeSpecMapIterator) :: iter

      text = ''
      associate (e => this%attributes%ftn_end())
         iter = this%attributes%ftn_begin()
         do while (iter /= e)
            call iter%next()
            if (len(text) > 0) text = text // ', '
            text = text // iter%first()
         end do
      end associate
      if (len(text) == 0) text = '(none)'
   end function attribute_names_text

   ! Declares a named argument. The optional arguments are those of
   ! CallbackArgumentSpec. Rejects a duplicate name, leaving the existing
   ! declaration unchanged, and a weight that is inconsistent with the
   ! argument it weights (see the module header).
   subroutine interface_declare_argument(this, name, expected_kind, dims, &
        optional, extensive, weight_for, units, rc)
      class(CallbackInterface), target, intent(inout) :: this
      character(*), intent(in) :: name
      type(MAPL_StateItem_Flag), intent(in) :: expected_kind
      character(*), optional, intent(in) :: dims
      logical, optional, intent(in) :: optional
      logical, optional, intent(in) :: extensive
      character(*), optional, intent(in) :: weight_for
      character(*), optional, intent(in) :: units
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpec) :: spec
      character(:), allocatable :: msg
      integer :: status

      call check_name_has_no_slash('argument', name, _RC)
      msg = 'CallbackInterface: argument name already declared: ' // name
      _ASSERT(this%arguments%count(name) == 0, msg)

      spec = CallbackArgumentSpec(name, expected_kind, dims, optional, &
           extensive, weight_for, units, rc=status)
      _VERIFY(status)

      if (spec%has_weight_for()) then
         call check_weight(this, spec, _RC)
      end if

      call this%arguments%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine interface_declare_argument

   ! A weight is extensive, names a declared argument that is not
   ! extensive, and is required whenever that argument is.
   subroutine check_weight(this, weight, rc)
      class(CallbackInterface), target, intent(in) :: this
      type(CallbackArgumentSpec), intent(in) :: weight
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpec), pointer :: weighted
      character(:), allocatable :: msg, name, weighted_name

      name = weight%get_name()
      weighted_name = weight%get_weight_for()

      weighted => this%arguments%at(weighted_name)
      msg = 'CallbackInterface: argument ' // name // ' is the weight ' // &
           'for unknown argument ' // weighted_name // &
           '; declared arguments: ' // argument_names_text(this)
      _ASSERT(associated(weighted), msg)

      msg = 'CallbackInterface: weight argument must be extensive: ' // name
      _ASSERT(weight%is_extensive(), msg)
      msg = 'CallbackInterface: argument ' // name // ' is the weight ' // &
           'for ' // weighted_name // ', which is extensive'
      _ASSERT(.not. weighted%is_extensive(), msg)

      if (.not. weighted%is_optional()) then
         msg = 'CallbackInterface: weight argument ' // name // &
              ' must be required because ' // weighted_name // &
              ' is required'
         _ASSERT(.not. weight%is_optional(), msg)
      end if

      _RETURN(_SUCCESS)
   end subroutine check_weight

   logical function interface_is_argument(this, name) result(is_arg)
      class(CallbackInterface), intent(in) :: this
      character(*), intent(in) :: name

      is_arg = this%arguments%count(name) > 0
   end function interface_is_argument

   function interface_get_argument(this, name, rc) result(spec)
      class(CallbackInterface), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(CallbackArgumentSpec) :: spec

      type(CallbackArgumentSpec), pointer :: found
      character(:), allocatable :: msg

      found => this%arguments%at(name)
      msg = 'CallbackInterface: get_argument - argument not declared: ' &
           // name
      _ASSERT(associated(found), msg)
      spec = found

      _RETURN(_SUCCESS)
   end function interface_get_argument

   function interface_get_arguments(this) result(arguments)
      class(CallbackInterface), intent(in) :: this
      type(CallbackArgumentSpecMap) :: arguments

      arguments = this%arguments
   end function interface_get_arguments

   ! Declares a named method with no argument-access entries
   ! yet - set_method_argument_access (below) fills those in. Rejects a
   ! duplicate name, leaving the existing declaration unchanged (spec
   ! scenario "Duplicate method name is rejected").
   subroutine interface_declare_method(this, name, rc)
      class(CallbackInterface), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(CallbackMethodSpec) :: empty
      character(:), allocatable :: msg
      integer :: status

      call check_name_has_no_slash('method', name, _RC)
      msg = 'CallbackInterface: method name already declared: ' // name
      _ASSERT(this%methods%count(name) == 0, msg)

      call this%methods%insert(name, empty)

      _RETURN(_SUCCESS)
   end subroutine interface_declare_method

   logical function interface_is_method(this, name) result(is_meth)
      class(CallbackInterface), intent(in) :: this
      character(*), intent(in) :: name

      is_meth = this%methods%count(name) > 0
   end function interface_is_method

   function interface_get_method(this, name, rc) result(spec)
      class(CallbackInterface), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(CallbackMethodSpec) :: spec

      type(CallbackMethodSpec), pointer :: found
      character(:), allocatable :: msg

      found => this%methods%at(name)
      msg = 'CallbackInterface: get_method - method not declared: ' // name
      _ASSERT(associated(found), msg)
      spec = found

      _RETURN(_SUCCESS)
   end function interface_get_method

   function interface_get_methods(this) result(methods)
      class(CallbackInterface), intent(in) :: this
      type(CallbackMethodSpecMap) :: methods

      methods = this%methods
   end function interface_get_methods

   ! Assigns method_name's access mode for argument_name, and optionally
   ! its per-argument options. Requires both names to already be declared
   ! on this interface before mutating the method's own CallbackMethodSpec
   ! in place via %at() pointer access (see module header).
   subroutine interface_set_method_argument_access(this, method_name, &
        argument_name, access, options, rc)
      class(CallbackInterface), intent(inout) :: this
      character(*), intent(in) :: method_name
      character(*), intent(in) :: argument_name
      type(AccessSpec), intent(in) :: access
      type(CallbackArgumentOptions), optional, intent(in) :: options
      integer, optional, intent(out) :: rc

      type(CallbackMethodSpec), pointer :: spec_ptr
      character(:), allocatable :: msg
      integer :: status

      msg = 'CallbackInterface: set_method_argument_access - ' // &
           'method not declared: ' // method_name
      _ASSERT(this%is_method(method_name), msg)
      msg = 'CallbackInterface: set_method_argument_access - ' // &
           'argument not declared: ' // argument_name
      _ASSERT(this%is_argument(argument_name), msg)

      spec_ptr => this%methods%at(method_name)
      msg = 'CallbackInterface: set_method_argument_access - ' // &
           'method not found: ' // method_name
      _ASSERT(associated(spec_ptr), msg)

      call spec_ptr%declare_argument_access(argument_name, access, &
           options, _RC)

      _RETURN(_SUCCESS)
   end subroutine interface_set_method_argument_access

   ! Declares a named attribute. The arguments are those of
   ! CallbackAttributeSpec, which checks the declaration on its own. Here
   ! a duplicate name is rejected, and a member-scoped attribute's host
   ! argument must be a declared, required argument that is not a weight.
   subroutine interface_declare_attribute(this, name, value_type, &
        is_list, scope, host_argument, required, default, default_list, &
        allowed_values, rc)
      class(CallbackInterface), target, intent(inout) :: this
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
      integer :: status

      call check_name_has_no_slash('attribute', name, _RC)
      msg = 'CallbackInterface: attribute name already declared: ' // name
      _ASSERT(this%attributes%count(name) == 0, msg)

      spec = CallbackAttributeSpec(name, value_type, is_list, scope, &
           host_argument, required, default, default_list, &
           allowed_values, rc=status)
      _VERIFY(status)

      if (spec%has_host_argument()) then
         call check_host_argument(this, spec, _RC)
      end if

      call this%attributes%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine interface_declare_attribute

   subroutine check_host_argument(this, attribute, rc)
      class(CallbackInterface), target, intent(in) :: this
      type(CallbackAttributeSpec), intent(in) :: attribute
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpec), pointer :: host
      character(:), allocatable :: msg, name, host_name

      name = attribute%get_name()
      host_name = attribute%get_host_argument()

      host => this%arguments%at(host_name)
      msg = 'CallbackInterface: attribute ' // name // &
           ' names unknown host argument ' // host_name // &
           '; declared arguments: ' // argument_names_text(this)
      _ASSERT(associated(host), msg)

      msg = 'CallbackInterface: host argument ' // host_name // &
           ' of attribute ' // name // ' must be required'
      _ASSERT(.not. host%is_optional(), msg)
      msg = 'CallbackInterface: host argument ' // host_name // &
           ' of attribute ' // name // ' must not be a weight'
      _ASSERT(.not. host%has_weight_for(), msg)

      _RETURN(_SUCCESS)
   end subroutine check_host_argument

   logical function interface_is_attribute(this, name) result(is_attr)
      class(CallbackInterface), intent(in) :: this
      character(*), intent(in) :: name

      is_attr = this%attributes%count(name) > 0
   end function interface_is_attribute

   function interface_get_attribute(this, name, rc) result(spec)
      class(CallbackInterface), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(CallbackAttributeSpec) :: spec

      type(CallbackAttributeSpec), pointer :: found
      character(:), allocatable :: msg

      found => this%attributes%at(name)
      msg = 'CallbackInterface: get_attribute - attribute not declared: ' &
           // name
      _ASSERT(associated(found), msg)
      spec = found

      _RETURN(_SUCCESS)
   end function interface_get_attribute

   function interface_get_attributes(this) result(attributes)
      class(CallbackInterface), intent(in) :: this
      type(CallbackAttributeSpecMap) :: attributes

      attributes = this%attributes
   end function interface_get_attributes

   ! Declares a named call scalar. The arguments are those of
   ! CallbackCallScalarSpec. Rejects a duplicate name.
   subroutine interface_declare_call_scalar(this, name, value_type, &
        writer, rc)
      class(CallbackInterface), intent(inout) :: this
      character(*), intent(in) :: name
      type(CallbackValueType), intent(in) :: value_type
      type(CallbackCallScalarWriter), intent(in) :: writer
      integer, optional, intent(out) :: rc

      type(CallbackCallScalarSpec) :: spec
      character(:), allocatable :: msg
      integer :: status

      call check_name_has_no_slash('call scalar', name, _RC)
      msg = 'CallbackInterface: call scalar name already declared: ' // name
      _ASSERT(this%call_scalars%count(name) == 0, msg)

      spec = CallbackCallScalarSpec(name, value_type, writer, rc=status)
      _VERIFY(status)

      call this%call_scalars%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine interface_declare_call_scalar

   logical function interface_is_call_scalar(this, name) result(is_scalar)
      class(CallbackInterface), intent(in) :: this
      character(*), intent(in) :: name

      is_scalar = this%call_scalars%count(name) > 0
   end function interface_is_call_scalar

   function interface_get_call_scalar(this, name, rc) result(spec)
      class(CallbackInterface), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(CallbackCallScalarSpec) :: spec

      type(CallbackCallScalarSpec), pointer :: found
      character(:), allocatable :: msg

      found => this%call_scalars%at(name)
      msg = 'CallbackInterface: get_call_scalar - call scalar not ' // &
           'declared: ' // name
      _ASSERT(associated(found), msg)
      spec = found

      _RETURN(_SUCCESS)
   end function interface_get_call_scalar

   function interface_get_call_scalars(this) result(call_scalars)
      class(CallbackInterface), intent(in) :: this
      type(CallbackCallScalarSpecMap) :: call_scalars

      call_scalars = this%call_scalars
   end function interface_get_call_scalars

   ! Declares "when attribute has value, the arguments in requires are
   ! required". The attribute must be declared, value must be legal for
   ! it, and every name in requires must be a declared optional argument
   ! (a required argument needs no rule). Rules keep declaration order.
   subroutine interface_declare_rule(this, attribute, value, requires, rc)
      class(CallbackInterface), target, intent(inout) :: this
      character(*), intent(in) :: attribute
      character(*), intent(in) :: value
      character(*), intent(in) :: requires(:)
      integer, optional, intent(out) :: rc

      type(CallbackRule) :: rule
      type(CallbackAttributeSpec), pointer :: attribute_spec
      type(CallbackArgumentSpec), pointer :: argument_spec
      character(:), allocatable :: msg
      integer :: i, status

      rule = make_CallbackRule(attribute, value, requires, rc=status)
      _VERIFY(status)

      attribute_spec => this%attributes%at(attribute)
      msg = 'CallbackInterface: rule names unknown attribute ' // &
           attribute // '; declared attributes: ' // &
           attribute_names_text(this)
      _ASSERT(associated(attribute_spec), msg)

      call check_rule_value(attribute_spec, trim(value), _RC)

      do i = 1, size(requires)
         argument_spec => this%arguments%at(trim(requires(i)))
         msg = 'CallbackInterface: rule on ' // attribute // &
              ' names unknown argument ' // trim(requires(i)) // &
              '; declared arguments: ' // argument_names_text(this)
         _ASSERT(associated(argument_spec), msg)
         msg = 'CallbackInterface: rule on ' // attribute // &
              ' names argument ' // trim(requires(i)) // &
              ', which is not optional'
         _ASSERT(argument_spec%is_optional(), msg)
      end do

      call this%rules%push_back(rule)

      _RETURN(_SUCCESS)
   end subroutine interface_declare_rule

   function interface_get_rules(this) result(rules)
      class(CallbackInterface), intent(in) :: this
      type(CallbackRuleVector) :: rules

      rules = this%rules
   end function interface_get_rules

   ! A rule's value is text. It is legal for the attribute when it has the
   ! attribute's type (integer: an optional sign and digits; real: a
   ! number; logical: true or false; string: any text) and, when the
   ! attribute has an allowed set, lies in it. A list attribute has no
   ! single-valued form, so a rule on one is rejected.
   subroutine check_rule_value(attribute, value, rc)
      type(CallbackAttributeSpec), intent(in) :: attribute
      character(*), intent(in) :: value
      integer, optional, intent(out) :: rc

      type(CallbackValueType) :: value_type
      type(StringVector) :: allowed_strings
      integer(INT64), allocatable :: allowed_integers(:)
      integer(INT64) :: number
      character(:), allocatable :: msg, name
      logical :: legal
      integer :: i, status

      name = attribute%get_name()
      msg = 'CallbackInterface: a rule cannot test list attribute ' // name
      _ASSERT(.not. attribute%is_list(), msg)

      value_type = attribute%get_value_type()
      msg = 'CallbackInterface: value "' // value // &
           '" is not legal for attribute ' // name // ' of type ' // &
           value_type%to_string()

      if (value_type == CALLBACK_VALUE_INTEGER) then
         call parse_integer(value, number, legal)
         _ASSERT(legal, msg)
         if (attribute%has_allowed_values()) then
            allowed_integers = attribute%get_allowed_integers(rc=status)
            _VERIFY(status)
            msg = 'CallbackInterface: value ' // value // &
                 ' is not among the allowed values of attribute ' // name
            _ASSERT(any(allowed_integers == number), msg)
         end if
      else if (value_type == CALLBACK_VALUE_REAL) then
         _ASSERT(is_real_text(value), msg)
      else if (value_type == CALLBACK_VALUE_LOGICAL) then
         _ASSERT(value == 'true' .or. value == 'false', msg)
      else if (value_type == CALLBACK_VALUE_STRING) then
         if (attribute%has_allowed_values()) then
            allowed_strings = attribute%get_allowed_strings(rc=status)
            _VERIFY(status)
            legal = .false.
            do i = 1, allowed_strings%size()
               if (allowed_strings%of(i) == value) legal = .true.
            end do
            msg = 'CallbackInterface: value ' // value // &
                 ' is not among the allowed values of attribute ' // name
            _ASSERT(legal, msg)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine check_rule_value

   ! An optional sign followed by at least one digit and nothing else.
   subroutine parse_integer(text, number, ok)
      character(*), intent(in) :: text
      integer(INT64), intent(out) :: number
      logical, intent(out) :: ok

      integer :: first, status

      number = 0_INT64
      first = 1
      if (len(text) > 0) then
         if (text(1:1) == '+' .or. text(1:1) == '-') first = 2
      end if
      ! At most 19 digits (the width of the largest INT64); the read
      ! rejects a 19-digit value that overflows.
      ok = len(text) >= first .and. len(text) - first + 1 <= 19
      if (ok) ok = verify(text(first:), '0123456789') == 0
      if (ok) then
         read(text, '(I20)', iostat=status) number
         ok = status == 0
      end if
   end subroutine parse_integer

   ! A decimal number with an optional exponent, without separators.
   logical function is_real_text(text) result(ok)
      character(*), intent(in) :: text

      real(REAL64) :: number
      integer :: status

      ok = len(text) > 0
      if (ok) ok = verify(text, '0123456789+-.eEdD') == 0
      if (ok) ok = scan(text, '0123456789') > 0
      if (ok) then
         read(text, *, iostat=status) number
         ok = status == 0
      end if
   end function is_real_text

end module mapl_CallbackInterface_mod
