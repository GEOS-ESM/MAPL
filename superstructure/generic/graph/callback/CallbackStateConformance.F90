#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackStateConformance: checks one realized callback State (an
! ESMF_State and the Info of its members) against its CallbackInterface
! and returns every violation found, in a fixed order: arguments
! (presence, kind), weights (presence, geometry), attributes (presence,
! type, allowed values), unknown keys under the interface namespace, and
! last the rules, so a rule whose attribute failed its own checks is
! never evaluated.
!
! The interface name is an argument: CallbackInterface does not store
! its own name (the registry keys it), and the caller formats the report
! with format_callback_contract_report from the same name.
!
! An absent optional attribute with a declared default is logged once
! per call (the caller runs the check at most once per callback State),
! and its default is the value rules are evaluated on. A rule-required
! argument is never defaulted.
!
! No graph or component dependency, so it is testable on hand-built
! States.
!------------------------------------------------------------------------------
module mapl_CallbackStateConformance_mod
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   use esmf, only: ESMF_State, ESMF_StateGet, ESMF_StateItem_Flag
   use esmf, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use esmf, only: ESMF_STATEITEM_STATE, ESMF_STATEITEM_ROUTEHANDLE
   use esmf, only: ESMF_STATEITEM_NOTFOUND
   use esmf, only: ESMF_Field, ESMF_FieldGet, ESMF_Geom
   use esmf, only: ESMF_MAXSTR
   use esmf, only: operator(==)
   use gFTL2_StringVector, only: StringVector
   use pflogger, only: logging, Logger
   use mapl_geom_api, only: mapl_SameGeom
   use mapl_InfoUtilities_mod, only: MAPL_InfoGetValueKind, MAPL_InfoGetKeys
   use mapl_InfoUtilities_mod, only: MAPL_InfoGetShared
   use mapl_InfoUtilities_mod, only: InfoValueKind, operator(==)
   use mapl_InfoUtilities_mod, only: INFO_VALUE_ABSENT
   use mapl_InfoUtilities_mod, only: INFO_VALUE_INTEGER, INFO_VALUE_REAL
   use mapl_InfoUtilities_mod, only: INFO_VALUE_LOGICAL, INFO_VALUE_STRING
   use mapl_InfoUtilities_mod, only: INFO_VALUE_INTEGER_LIST, INFO_VALUE_REAL_LIST
   use mapl_InfoUtilities_mod, only: INFO_VALUE_LOGICAL_LIST, INFO_VALUE_STRING_LIST
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, operator(==)
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_BRACKET
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_VECTORBRACKET, MAPL_STATEITEM_STATE
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_ROUTEHANDLE
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap, &
        CallbackArgumentSpecMapIterator, operator(/=)
   use mapl_CallbackAttributeSpec_mod, only: CallbackAttributeSpec, &
        CALLBACK_SCOPE_STATE, operator(==)
   use mapl_CallbackAttributeSpecMap_mod, only: CallbackAttributeSpecMap, &
        CallbackAttributeSpecMapIterator, operator(/=)
   use mapl_CallbackCallScalarSpecMap_mod, only: CallbackCallScalarSpecMap, &
        CallbackCallScalarSpecMapIterator, operator(/=)
   use mapl_CallbackRule_mod, only: CallbackRule
   use mapl_CallbackRuleVector_mod, only: CallbackRuleVector
   use mapl_CallbackValueType_mod, only: CallbackValueType, operator(==)
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_INTEGER, CALLBACK_VALUE_REAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_LOGICAL, CALLBACK_VALUE_STRING
   use mapl_CallbackContractViolation_mod, only: CallbackContractViolation, &
        missing_argument_violation, kind_mismatch_violation, &
        missing_attribute_violation, attribute_type_violation, &
        attribute_value_not_allowed_violation, rule_not_met_violation, &
        unknown_key_violation, weight_missing_violation, weight_geometry_violation
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: check_callback_state_conformance
   public :: integer_text

   ! Outcome of one attribute's own checks, consulted by the rules.
   type :: AttributeOutcome
      logical :: passed = .false.
      logical :: has_value = .false.
      character(:), allocatable :: value_text
   end type AttributeOutcome

contains

   ! violations is empty when the State conforms. defaults_used, if
   ! present, receives the bare names of the attributes whose default was
   ! used (the same ones logged).
   subroutine check_callback_state_conformance(state, iface, interface_name, &
        handler_name, violations, defaults_used, rc)
      type(ESMF_State), intent(in) :: state
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: interface_name
      character(*), intent(in) :: handler_name
      type(CallbackContractViolation), allocatable, intent(out) :: violations(:)
      type(StringVector), optional, intent(out) :: defaults_used
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpecMap) :: arguments
      type(CallbackAttributeSpecMap) :: attributes
      type(StringVector) :: present_arguments, conforming_arguments, used
      type(AttributeOutcome), allocatable :: outcomes(:)
      integer :: status

      allocate(violations(0))
      arguments = iface%get_arguments()
      attributes = iface%get_attributes()

      call check_arguments(state, arguments, present_arguments, conforming_arguments, violations, _RC)
      call check_weights(state, arguments, present_arguments, violations, _RC)
      call check_attributes(state, attributes, interface_name, handler_name, &
           conforming_arguments, outcomes, used, violations, _RC)
      call check_unknown_keys(state, iface, interface_name, violations, _RC)
      call check_rules(iface, attributes, interface_name, outcomes, present_arguments, violations)

      if (present(defaults_used)) defaults_used = used

      _RETURN(_SUCCESS)
   end subroutine check_callback_state_conformance

   subroutine append(violations, violation)
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      type(CallbackContractViolation), intent(in) :: violation

      type(CallbackContractViolation), allocatable :: grown(:)
      integer :: n

      n = size(violations)
      allocate(grown(n + 1))
      grown(1:n) = violations
      grown(n + 1) = violation
      call move_alloc(grown, violations)
   end subroutine append

   logical function contains_name(names, name) result(found)
      type(StringVector), intent(in) :: names
      character(*), intent(in) :: name

      integer :: i

      found = .false.
      do i = 1, names%size()
         if (names%of(i) == name) then
            found = .true.
            return
         end if
      end do
   end function contains_name

   function item_type_of(state, name, rc) result(item_type)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(ESMF_StateItem_Flag) :: item_type

      integer :: status

      call ESMF_StateGet(state, itemName=name, itemType=item_type, _RC)

      _RETURN(_SUCCESS)
   end function item_type_of

   function found_kind_name(item_type) result(name)
      type(ESMF_StateItem_Flag), intent(in) :: item_type
      character(:), allocatable :: name

      type(MAPL_StateItem_Flag) :: flag

      name = 'UNKNOWN'
      if (item_type == ESMF_STATEITEM_FIELD) then
         flag = MAPL_STATEITEM_FIELD
      else if (item_type == ESMF_STATEITEM_FIELDBUNDLE) then
         flag = MAPL_STATEITEM_FIELDBUNDLE
      else if (item_type == ESMF_STATEITEM_STATE) then
         flag = MAPL_STATEITEM_STATE
      else if (item_type == ESMF_STATEITEM_ROUTEHANDLE) then
         flag = MAPL_STATEITEM_ROUTEHANDLE
      else
         return
      end if
      name = flag%to_string()
   end function found_kind_name

   ! Every MAPL bundle-like kind (vector, bracket) is realized as an
   ! ESMF_FieldBundle.
   logical function kind_matches(expected, item_type) result(matches)
      type(MAPL_StateItem_Flag), intent(in) :: expected
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      if (expected == MAPL_STATEITEM_FIELD) then
         matches = item_type == ESMF_STATEITEM_FIELD
      else if (expected == MAPL_STATEITEM_FIELDBUNDLE .or. expected == MAPL_STATEITEM_VECTOR .or. &
           expected == MAPL_STATEITEM_BRACKET .or. expected == MAPL_STATEITEM_VECTORBRACKET) then
         matches = item_type == ESMF_STATEITEM_FIELDBUNDLE
      else if (expected == MAPL_STATEITEM_STATE) then
         matches = item_type == ESMF_STATEITEM_STATE
      else if (expected == MAPL_STATEITEM_ROUTEHANDLE) then
         matches = item_type == ESMF_STATEITEM_ROUTEHANDLE
      else
         matches = .false.
      end if
   end function kind_matches

   ! present_arguments lists every declared argument present as a
   ! member, whatever its kind (rules and weights test presence only);
   ! conforming_arguments those also of the declared kind.
   subroutine check_arguments(state, arguments, present_arguments, conforming_arguments, &
        violations, rc)
      type(ESMF_State), intent(in) :: state
      type(CallbackArgumentSpecMap), target, intent(in) :: arguments
      type(StringVector), intent(out) :: present_arguments
      type(StringVector), intent(out) :: conforming_arguments
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpecMapIterator) :: iter
      type(CallbackArgumentSpec) :: spec
      type(ESMF_StateItem_Flag) :: item_type
      type(MAPL_StateItem_Flag) :: expected
      character(:), allocatable :: name
      integer :: status

      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         spec = iter%second()
         name = spec%get_name()
         item_type = item_type_of(state, name, _RC)
         if (item_type == ESMF_STATEITEM_NOTFOUND) then
            if (spec%is_optional()) cycle
            if (spec%has_weight_for()) then
               call append(violations, missing_argument_violation(name, weight_of=spec%get_weight_for()))
            else
               call append(violations, missing_argument_violation(name))
            end if
            cycle
         end if
         call present_arguments%push_back(name)
         expected = spec%get_expected_kind()
         if (kind_matches(expected, item_type)) then
            call conforming_arguments%push_back(name)
         else
            call append(violations, kind_mismatch_violation(name, found_kind_name(item_type), &
                 expected%to_string()))
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine check_arguments

   ! A required weight that is absent was already reported as a missing
   ! argument; only an optional weight gets the weight-missing violation.
   subroutine check_weights(state, arguments, present_arguments, violations, rc)
      type(ESMF_State), intent(in) :: state
      type(CallbackArgumentSpecMap), target, intent(in) :: arguments
      type(StringVector), intent(in) :: present_arguments
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpecMapIterator) :: iter
      type(CallbackArgumentSpec) :: spec
      character(:), allocatable :: weight_name, weighted_name
      type(ESMF_Field) :: weight_field, weighted_field
      type(ESMF_Geom) :: weight_geom, weighted_geom
      type(ESMF_StateItem_Flag) :: weight_type, weighted_type
      integer :: status

      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         spec = iter%second()
         if (.not. spec%has_weight_for()) cycle
         weight_name = spec%get_name()
         weighted_name = spec%get_weight_for()
         if (.not. contains_name(present_arguments, weighted_name)) cycle

         if (.not. contains_name(present_arguments, weight_name)) then
            if (spec%is_optional()) then
               call append(violations, weight_missing_violation(weight_name, weighted_name))
            end if
            cycle
         end if

         weight_type = item_type_of(state, weight_name, _RC)
         weighted_type = item_type_of(state, weighted_name, _RC)
         if (.not. (weight_type == ESMF_STATEITEM_FIELD .and. weighted_type == ESMF_STATEITEM_FIELD)) cycle
         call ESMF_StateGet(state, itemName=weight_name, field=weight_field, _RC)
         call ESMF_StateGet(state, itemName=weighted_name, field=weighted_field, _RC)
         call ESMF_FieldGet(weight_field, geom=weight_geom, _RC)
         call ESMF_FieldGet(weighted_field, geom=weighted_geom, _RC)
         if (.not. mapl_SameGeom(weight_geom, weighted_geom)) then
            call append(violations, weight_geometry_violation(weight_name, weighted_name))
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine check_weights

   ! outcomes(i) belongs to the i-th attribute in the map's iteration
   ! order (the same order check_rules walks to find it again).
   subroutine check_attributes(state, attributes, interface_name, handler_name, &
        conforming_arguments, outcomes, defaults_used, violations, rc)
      type(ESMF_State), intent(in) :: state
      type(CallbackAttributeSpecMap), target, intent(in) :: attributes
      character(*), intent(in) :: interface_name
      character(*), intent(in) :: handler_name
      type(StringVector), intent(in) :: conforming_arguments
      type(AttributeOutcome), allocatable, intent(out) :: outcomes(:)
      type(StringVector), intent(out) :: defaults_used
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      integer, optional, intent(out) :: rc

      type(CallbackAttributeSpecMapIterator) :: iter
      type(CallbackAttributeSpec) :: spec
      character(:), allocatable :: key, host
      type(InfoValueKind) :: stored_kind
      type(ESMF_StateItem_Flag) :: host_type
      integer :: i, status

      allocate(outcomes(attributes%size()))
      i = 0
      iter = attributes%ftn_begin()
      do while (iter /= attributes%ftn_end())
         call iter%next()
         i = i + 1
         spec = iter%second()
         key = interface_name // '/' // spec%get_name()
         host = spec%get_host_argument()

         if (spec%get_scope() == CALLBACK_SCOPE_STATE) then
            call MAPL_InfoGetValueKind(state, key, stored_kind, _RC)
         else
            ! Skip a host that is absent or of the wrong kind: that is
            ! reported as an argument violation (a host is always a
            ! required argument), not again per attribute.
            if (.not. contains_name(conforming_arguments, host)) cycle
            host_type = item_type_of(state, host, _RC)
            if (.not. hosts_info(host_type)) cycle
            call MAPL_InfoGetValueKind(state, host, key, stored_kind, _RC)
         end if

         if (stored_kind == INFO_VALUE_ABSENT) then
            if (spec%is_required()) then
               call append(violations, missing_attribute_violation(key, host))
            else if (spec%has_default()) then
               outcomes(i)%passed = .true.
               outcomes(i)%has_value = .not. spec%is_list()
               outcomes(i)%value_text = default_text(spec)
               call defaults_used%push_back(spec%get_name())
               call log_default(state, handler_name, key, outcomes(i)%value_text)
            else
               outcomes(i)%passed = .true.
            end if
            cycle
         end if

         if (.not. type_compatible(spec, stored_kind)) then
            call append(violations, attribute_type_violation(key, stored_kind%to_string(), &
                 declared_type_name(spec)))
            cycle
         end if

         call check_value(state, spec, key, host, outcomes(i), violations, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine check_attributes

   logical function hosts_info(item_type) result(hosts)
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      hosts = item_type == ESMF_STATEITEM_FIELD .or. item_type == ESMF_STATEITEM_FIELDBUNDLE
   end function hosts_info

   ! Read without conversion: integer and real are distinct, R4 and R8
   ! are both real, and a list needs an array of the element type.
   logical function type_compatible(spec, stored_kind) result(compatible)
      type(CallbackAttributeSpec), intent(in) :: spec
      type(InfoValueKind), intent(in) :: stored_kind

      type(CallbackValueType) :: value_type

      value_type = spec%get_value_type()
      if (spec%is_list()) then
         compatible = (value_type == CALLBACK_VALUE_INTEGER .and. stored_kind == INFO_VALUE_INTEGER_LIST) .or. &
              (value_type == CALLBACK_VALUE_REAL .and. stored_kind == INFO_VALUE_REAL_LIST) .or. &
              (value_type == CALLBACK_VALUE_LOGICAL .and. stored_kind == INFO_VALUE_LOGICAL_LIST) .or. &
              (value_type == CALLBACK_VALUE_STRING .and. stored_kind == INFO_VALUE_STRING_LIST)
      else
         compatible = (value_type == CALLBACK_VALUE_INTEGER .and. stored_kind == INFO_VALUE_INTEGER) .or. &
              (value_type == CALLBACK_VALUE_REAL .and. stored_kind == INFO_VALUE_REAL) .or. &
              (value_type == CALLBACK_VALUE_LOGICAL .and. stored_kind == INFO_VALUE_LOGICAL) .or. &
              (value_type == CALLBACK_VALUE_STRING .and. stored_kind == INFO_VALUE_STRING)
      end if
   end function type_compatible

   function declared_type_name(spec) result(name)
      type(CallbackAttributeSpec), intent(in) :: spec
      character(:), allocatable :: name

      type(CallbackValueType) :: value_type

      value_type = spec%get_value_type()
      name = value_type%to_string()
      if (spec%is_list()) name = name // ' list'
   end function declared_type_name

   ! Reads a type-checked present value, checks it against the allowed
   ! set, and records its text for the rules (scalars only: a rule
   ! cannot test a list attribute).
   subroutine check_value(state, spec, key, host, outcome, violations, rc)
      type(ESMF_State), intent(in) :: state
      type(CallbackAttributeSpec), intent(in) :: spec
      character(*), intent(in) :: key
      character(*), intent(in) :: host
      type(AttributeOutcome), intent(inout) :: outcome
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      integer, optional, intent(out) :: rc

      type(CallbackValueType) :: value_type
      type(StringVector) :: allowed, values
      integer(INT64), allocatable :: allowed_integers(:)
      integer, allocatable :: integers(:)
      character(:), allocatable :: text
      integer :: integer_value
      real(REAL64) :: real_value
      logical :: logical_value
      integer :: i, status

      value_type = spec%get_value_type()
      outcome%passed = .true.

      if (spec%is_list()) then
         if (.not. spec%has_allowed_values()) then
            _RETURN(_SUCCESS)
         end if
         if (value_type == CALLBACK_VALUE_STRING) then
            call get_string_list(state, host, key, values, _RC)
            allowed = spec%get_allowed_strings(_RC)
            do i = 1, values%size()
               if (.not. contains_name(allowed, values%of(i))) then
                  call append(violations, attribute_value_not_allowed_violation(key, &
                       values%of(i), to_array(allowed)))
                  outcome%passed = .false.
               end if
            end do
         else
            call get_integer_list(state, host, key, integers, _RC)
            allowed_integers = spec%get_allowed_integers(_RC)
            do i = 1, size(integers)
               if (.not. any(allowed_integers == integers(i))) then
                  call append(violations, attribute_value_not_allowed_violation(key, &
                       integer_text(int(integers(i), INT64)), integers_to_array(allowed_integers)))
                  outcome%passed = .false.
               end if
            end do
         end if
         _RETURN(_SUCCESS)
      end if

      if (value_type == CALLBACK_VALUE_STRING) then
         if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=text, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, text, _RC)
         end if
         if (spec%has_allowed_values()) then
            allowed = spec%get_allowed_strings(_RC)
            if (.not. contains_name(allowed, text)) then
               call append(violations, attribute_value_not_allowed_violation(key, text, to_array(allowed)))
               outcome%passed = .false.
            end if
         end if
      else if (value_type == CALLBACK_VALUE_INTEGER) then
         if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=integer_value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, integer_value, _RC)
         end if
         text = integer_text(int(integer_value, INT64))
         if (spec%has_allowed_values()) then
            allowed_integers = spec%get_allowed_integers(_RC)
            if (.not. any(allowed_integers == integer_value)) then
               call append(violations, attribute_value_not_allowed_violation(key, text, &
                    integers_to_array(allowed_integers)))
               outcome%passed = .false.
            end if
         end if
      else if (value_type == CALLBACK_VALUE_REAL) then
         if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=real_value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, real_value, _RC)
         end if
         text = real_text(real_value)
      else
         if (len(host) == 0) then
            call MAPL_InfoGetShared(state, key=key, value=logical_value, _RC)
         else
            call MAPL_InfoGetShared(state, host, key, logical_value, _RC)
         end if
         text = merge('true ', 'false', logical_value)
         text = trim(text)
      end if

      outcome%has_value = .true.
      outcome%value_text = text

      _RETURN(_SUCCESS)
   end subroutine check_value

   subroutine get_string_list(state, host, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: host
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: values
      integer, optional, intent(out) :: rc

      integer :: status

      if (len(host) == 0) then
         call MAPL_InfoGetShared(state, key=key, values=values, _RC)
      else
         call MAPL_InfoGetShared(state, host, key, values, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_string_list

   subroutine get_integer_list(state, host, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: host
      character(*), intent(in) :: key
      integer, allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status

      if (len(host) == 0) then
         call MAPL_InfoGetShared(state, key=key, values=values, _RC)
      else
         call MAPL_InfoGetShared(state, host, key, values, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_integer_list

   ! Hosts are the callback State itself and every field or bundle
   ! member, declared or not.
   subroutine check_unknown_keys(state, iface, interface_name, violations, rc)
      type(ESMF_State), intent(in) :: state
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: interface_name
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      integer, optional, intent(out) :: rc

      character(ESMF_MAXSTR), allocatable :: item_names(:)
      character(ESMF_MAXSTR), allocatable :: legal(:)
      type(ESMF_StateItem_Flag) :: item_type
      integer :: item_count, i, status

      legal = legal_names(iface)

      call check_host_keys(state, '', iface, interface_name, legal, violations, _RC)

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(item_names(item_count))
      if (item_count > 0) then
         call ESMF_StateGet(state, itemNameList=item_names, _RC)
      end if
      do i = 1, item_count
         item_type = item_type_of(state, trim(item_names(i)), _RC)
         if (.not. hosts_info(item_type)) cycle
         call check_host_keys(state, trim(item_names(i)), iface, interface_name, legal, &
              violations, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine check_unknown_keys

   ! host is a member name, or '' for the State's own Info.
   subroutine check_host_keys(state, host, iface, interface_name, legal, violations, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: host
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: interface_name
      character(*), intent(in) :: legal(:)
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)
      integer, optional, intent(out) :: rc

      type(StringVector) :: keys
      integer :: status

      if (len(host) == 0) then
         call MAPL_InfoGetKeys(state, interface_name, keys, _RC)
      else
         call MAPL_InfoGetKeys(state, host, interface_name, keys, _RC)
      end if
      call report_unknown(iface, interface_name, keys, legal, violations)

      _RETURN(_SUCCESS)
   end subroutine check_host_keys

   subroutine report_unknown(iface, interface_name, keys, legal, violations)
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: interface_name
      type(StringVector), intent(in) :: keys
      character(*), intent(in) :: legal(:)
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)

      character(:), allocatable :: name
      integer :: i

      do i = 1, keys%size()
         ! A copy, not associate: gfortran frees an associate to the
         ! element pointer keys%of(i) returns, corrupting keys.
         name = keys%of(i)
         if (iface%is_attribute(name) .or. iface%is_call_scalar(name)) cycle
         call append(violations, unknown_key_violation(interface_name // '/' // name, legal))
      end do
   end subroutine report_unknown

   ! Declared attribute names, then call-scalar names, each in the
   ! interface's own (alphabetical) map order.
   function legal_names(iface) result(names)
      type(CallbackInterface), intent(in) :: iface
      character(ESMF_MAXSTR), allocatable :: names(:)

      type(CallbackAttributeSpecMap), target :: attributes
      type(CallbackCallScalarSpecMap), target :: call_scalars
      type(CallbackAttributeSpecMapIterator) :: attribute_iter
      type(CallbackCallScalarSpecMapIterator) :: scalar_iter
      integer :: i

      attributes = iface%get_attributes()
      call_scalars = iface%get_call_scalars()
      allocate(names(attributes%size() + call_scalars%size()))
      i = 0
      attribute_iter = attributes%ftn_begin()
      do while (attribute_iter /= attributes%ftn_end())
         call attribute_iter%next()
         i = i + 1
         names(i) = attribute_iter%first()
      end do
      scalar_iter = call_scalars%ftn_begin()
      do while (scalar_iter /= call_scalars%ftn_end())
         call scalar_iter%next()
         i = i + 1
         names(i) = scalar_iter%first()
      end do
   end function legal_names

   ! A rule is evaluated only when its attribute passed its own checks
   ! and has a value (stored or defaulted) equal to the rule's value.
   subroutine check_rules(iface, attributes, interface_name, outcomes, present_arguments, violations)
      type(CallbackInterface), intent(in) :: iface
      type(CallbackAttributeSpecMap), target, intent(in) :: attributes
      character(*), intent(in) :: interface_name
      type(AttributeOutcome), intent(in) :: outcomes(:)
      type(StringVector), intent(in) :: present_arguments
      type(CallbackContractViolation), allocatable, intent(inout) :: violations(:)

      type(CallbackRuleVector) :: rules
      type(CallbackRule) :: rule
      type(StringVector) :: required
      type(CallbackAttributeSpec) :: spec
      character(:), allocatable :: attribute_name
      integer :: i, j, k

      rules = iface%get_rules()
      do i = 1, rules%size()
         rule = rules%of(i)
         attribute_name = rule%get_attribute()
         k = attribute_index(attributes, attribute_name)
         if (k == 0) cycle
         if (.not. outcomes(k)%passed) cycle
         if (.not. outcomes(k)%has_value) cycle
         spec = iface%get_attribute(attribute_name)
         if (.not. rule_value_matches(spec, outcomes(k)%value_text, rule%get_value())) cycle
         required = rule%get_required_arguments()
         do j = 1, required%size()
            if (contains_name(present_arguments, required%of(j))) cycle
            call append(violations, rule_not_met_violation(interface_name // '/' // attribute_name, &
                 rule%get_value(), required%of(j)))
         end do
      end do
   end subroutine check_rules

   integer function attribute_index(attributes, name) result(k)
      type(CallbackAttributeSpecMap), target, intent(in) :: attributes
      character(*), intent(in) :: name

      type(CallbackAttributeSpecMapIterator) :: iter
      integer :: i

      k = 0
      i = 0
      iter = attributes%ftn_begin()
      do while (iter /= attributes%ftn_end())
         call iter%next()
         i = i + 1
         if (iter%first() == name) then
            k = i
            return
         end if
      end do
   end function attribute_index

   ! Integers and reals compare by value, so '+1' matches 1 and '2.' matches 2.0.
   logical function rule_value_matches(spec, value_text, rule_value) result(matches)
      type(CallbackAttributeSpec), intent(in) :: spec
      character(*), intent(in) :: value_text
      character(*), intent(in) :: rule_value

      type(CallbackValueType) :: value_type
      integer(INT64) :: a_int, b_int
      real(REAL64) :: a_real, b_real
      integer :: status_a, status_b

      value_type = spec%get_value_type()
      if (value_type == CALLBACK_VALUE_INTEGER) then
         read(value_text, *, iostat=status_a) a_int
         read(rule_value, *, iostat=status_b) b_int
         matches = status_a == 0 .and. status_b == 0 .and. a_int == b_int
      else if (value_type == CALLBACK_VALUE_REAL) then
         read(value_text, *, iostat=status_a) a_real
         read(rule_value, *, iostat=status_b) b_real
         matches = status_a == 0 .and. status_b == 0 .and. a_real == b_real
      else
         matches = value_text == rule_value
      end if
   end function rule_value_matches

   subroutine log_default(state, handler_name, key, value_text)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: handler_name
      character(*), intent(in) :: key
      character(*), intent(in) :: value_text

      class(Logger), pointer :: lgr
      character(ESMF_MAXSTR) :: state_name
      integer :: status

      state_name = ''
      call ESMF_StateGet(state, name=state_name, rc=status)
      lgr => logging%get_logger('mapl.generic')
      ! Pre-concatenated: pflogger would treat '%' in the text as a format.
      call lgr%info("callback State '" // trim(state_name) // "' of Handler '" // handler_name // &
           "': '" // key // "' is not set; using its default " // value_text)
   end subroutine log_default

   function default_text(spec) result(text)
      type(CallbackAttributeSpec), intent(in) :: spec
      character(:), allocatable :: text

      type(CallbackValueType) :: value_type
      type(StringVector) :: strings
      integer(INT64), allocatable :: integers(:)
      real(REAL64), allocatable :: reals(:)
      logical, allocatable :: logicals(:)
      integer :: i

      value_type = spec%get_value_type()
      if (.not. spec%is_list()) then
         if (value_type == CALLBACK_VALUE_INTEGER) then
            text = integer_text(spec%get_default_integer())
         else if (value_type == CALLBACK_VALUE_REAL) then
            text = real_text(spec%get_default_real())
         else if (value_type == CALLBACK_VALUE_LOGICAL) then
            text = trim(merge('true ', 'false', spec%get_default_logical()))
         else
            text = spec%get_default_string()
         end if
         return
      end if

      text = '['
      if (value_type == CALLBACK_VALUE_INTEGER) then
         integers = spec%get_default_integers()
         do i = 1, size(integers)
            if (i > 1) text = text // ', '
            text = text // integer_text(integers(i))
         end do
      else if (value_type == CALLBACK_VALUE_REAL) then
         reals = spec%get_default_reals()
         do i = 1, size(reals)
            if (i > 1) text = text // ', '
            text = text // real_text(reals(i))
         end do
      else if (value_type == CALLBACK_VALUE_LOGICAL) then
         logicals = spec%get_default_logicals()
         do i = 1, size(logicals)
            if (i > 1) text = text // ', '
            text = text // trim(merge('true ', 'false', logicals(i)))
         end do
      else
         strings = spec%get_default_strings()
         do i = 1, strings%size()
            if (i > 1) text = text // ', '
            text = text // strings%of(i)
         end do
      end if
      text = text // ']'
   end function default_text

   function integer_text(value) result(text)
      integer(INT64), intent(in) :: value
      character(:), allocatable :: text

      character(32) :: buffer

      write(buffer, '(i0)') value
      text = trim(buffer)
   end function integer_text

   ! Shortest of '(g0)' with trailing zeros dropped, keeping one digit
   ! after the point: 2.0, 0.25, 1.0E+20.
   function real_text(value) result(text)
      real(REAL64), intent(in) :: value
      character(:), allocatable :: text

      character(64) :: buffer
      integer :: point, last, exponent_at

      write(buffer, '(g0)') value
      text = trim(adjustl(buffer))
      exponent_at = scan(text, 'eEdD')
      point = index(text, '.')
      if (point == 0) return
      if (exponent_at > 0) then
         last = exponent_at - 1
      else
         last = len(text)
      end if
      do while (last > point + 1 .and. text(last:last) == '0')
         last = last - 1
      end do
      if (exponent_at > 0) then
         text = text(1:last) // text(exponent_at:)
      else
         text = text(1:last)
      end if
      if (text(1:1) == '.') text = '0' // text
   end function real_text

   function to_array(names) result(array)
      type(StringVector), intent(in) :: names
      character(ESMF_MAXSTR), allocatable :: array(:)

      integer :: i

      allocate(array(names%size()))
      do i = 1, names%size()
         array(i) = names%of(i)
      end do
   end function to_array

   function integers_to_array(values) result(array)
      integer(INT64), intent(in) :: values(:)
      character(32), allocatable :: array(:)

      integer :: i

      allocate(array(size(values)))
      do i = 1, size(values)
         array(i) = integer_text(values(i))
      end do
   end function integers_to_array

end module mapl_CallbackStateConformance_mod
