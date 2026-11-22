#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackContractViolation: one conformance problem found on a Handler's
! callback State. Carries the internal short reason code already used
! by today's conformance path (GraphBuilder_CallbackWiring_smod.F90's
! export_conforms_to_interface - "missing_member:<name>",
! "kind_mismatch:<name>", "dims_mismatch:<name>",
! "unreadable_member:<name>") extended with one code per problem category a
! realized-state checker can find, plus the already-worded report line
! (the member if any, what was found, and what is legal; the shared
! header supplies the interface, its version, the Handler and the
! callback State once per report, see CallbackContractReport.F90).
!
! One named constructor function per violation category rather than a
! single general-purpose constructor: each category needs a different
! set of facts to word its line (CallbackArgumentSpec.F90's own several
! optional constructor arguments would not fit here, since a kind
! mismatch and a disallowed value share no common field beyond the
! name).
!------------------------------------------------------------------------------
module mapl_CallbackContractViolation_mod
   implicit none(type, external)
   private

   public :: CallbackContractViolation
   public :: missing_argument_violation
   public :: unreadable_argument_violation
   public :: kind_mismatch_violation
   public :: dims_mismatch_violation
   public :: missing_attribute_violation
   public :: attribute_type_violation
   public :: attribute_value_not_allowed_violation
   public :: rule_not_met_violation
   public :: unknown_key_violation
   public :: weight_missing_violation
   public :: weight_geometry_violation

   type :: CallbackContractViolation
      private
      character(:), allocatable :: code
      character(:), allocatable :: text
   contains
      procedure :: get_code => violation_get_code
      procedure :: get_text => violation_get_text
   end type CallbackContractViolation

contains

   function new_violation(code, text) result(violation)
      character(*), intent(in) :: code
      character(*), intent(in) :: text
      type(CallbackContractViolation) :: violation

      violation%code = code
      violation%text = text
   end function new_violation

   function violation_get_code(this) result(code)
      class(CallbackContractViolation), intent(in) :: this
      character(:), allocatable :: code

      code = this%code
   end function violation_get_code

   function violation_get_text(this) result(text)
      class(CallbackContractViolation), intent(in) :: this
      character(:), allocatable :: text

      text = this%text
   end function violation_get_text

   ! Spec scenarios F1 "Required argument missing" and MF1 "Required
   ! weight missing": weight_of names the argument a missing required
   ! weight belongs to.
   function missing_argument_violation(argument_name, weight_of) result(violation)
      character(*), intent(in) :: argument_name
      character(*), optional, intent(in) :: weight_of
      type(CallbackContractViolation) :: violation

      if (present(weight_of)) then
         violation = new_violation('missing_member:' // argument_name, &
              "argument '" // argument_name // "' (the weight of '" // weight_of // "') is missing")
      else
         violation = new_violation('missing_member:' // argument_name, &
              "argument '" // argument_name // "' is missing")
      end if
   end function missing_argument_violation

   ! A member is declared under this name but could not be read back as
   ! a VariableSpec (VariableSpec%get_member failed) - a real
   ! structural problem with the declaration, distinct from
   ! missing_argument_violation's "this argument was never declared".
   function unreadable_argument_violation(argument_name) result(violation)
      character(*), intent(in) :: argument_name
      type(CallbackContractViolation) :: violation

      violation = new_violation('unreadable_member:' // argument_name, &
           "argument '" // argument_name // "' is declared but could not be read as a member")
   end function unreadable_argument_violation

   ! Spec scenario F2 "Kind mismatch".
   function kind_mismatch_violation(argument_name, found_kind, expected_kind) result(violation)
      character(*), intent(in) :: argument_name
      character(*), intent(in) :: found_kind
      character(*), intent(in) :: expected_kind
      type(CallbackContractViolation) :: violation

      violation = new_violation('kind_mismatch:' // argument_name, &
           "argument '" // argument_name // "' has kind '" // found_kind // &
           "', expected '" // expected_kind // "'")
   end function kind_mismatch_violation

   ! Declaration-level dims check (CallbackArgumentSpec%get_dims, 'xy' or
   ! 'xyz'), compared against the matched export's declared member; the
   ! realized-State checker (CallbackStateConformance.F90) does not check
   ! dims at all, only the declaration-level check does.
   function dims_mismatch_violation(argument_name, found_dims, expected_dims) result(violation)
      character(*), intent(in) :: argument_name
      character(*), intent(in) :: found_dims
      character(*), intent(in) :: expected_dims
      type(CallbackContractViolation) :: violation

      violation = new_violation('dims_mismatch:' // argument_name, &
           "argument '" // argument_name // "' has dims '" // found_dims // &
           "', expected '" // expected_dims // "'")
   end function dims_mismatch_violation

   ! Spec scenarios F3, MF6 "Required attribute missing". host is the
   ! member name the attribute is hosted on, or '' for a per-State
   ! attribute (the callback State itself is already named once in the
   ! report header).
   function missing_attribute_violation(key, host) result(violation)
      character(*), intent(in) :: key
      character(*), intent(in) :: host
      type(CallbackContractViolation) :: violation

      if (len(host) == 0) then
         violation = new_violation('missing_attribute:' // key, &
              "'" // key // "' is missing")
      else
         violation = new_violation('missing_attribute:' // key, &
              "'" // key // "' is missing on member '" // host // "'")
      end if
   end function missing_attribute_violation

   ! Spec scenario F4 "Attribute type".
   function attribute_type_violation(key, found_type, expected_type) result(violation)
      character(*), intent(in) :: key
      character(*), intent(in) :: found_type
      character(*), intent(in) :: expected_type
      type(CallbackContractViolation) :: violation

      violation = new_violation('attribute_type:' // key, &
           "'" // key // "' is stored as '" // found_type // &
           "', declared as '" // expected_type // "'")
   end function attribute_type_violation

   ! Spec scenarios F5, MF5 "Value not allowed". allowed_values is the
   ! legal set in declaration order - wording follows the spec's
   ! "Message wording" scenario exactly: "'<key>' = '<value>' is not
   ! allowed" and "allowed values: <v1>, <v2>, ...".
   function attribute_value_not_allowed_violation(key, value, allowed_values) result(violation)
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      character(*), intent(in) :: allowed_values(:)
      type(CallbackContractViolation) :: violation

      violation = new_violation('attribute_value_not_allowed:' // key, &
           "'" // key // "' = '" // value // "' is not allowed; allowed values: " // &
           join(allowed_values))
   end function attribute_value_not_allowed_violation

   ! Spec scenarios F6, MF7 "Rule not met": rule_key/rule_value name the
   ! triggering attribute = value pair, required_argument_name the
   ! argument the rule requires that is missing.
   function rule_not_met_violation(rule_key, rule_value, required_argument_name) result(violation)
      character(*), intent(in) :: rule_key
      character(*), intent(in) :: rule_value
      character(*), intent(in) :: required_argument_name
      type(CallbackContractViolation) :: violation

      violation = new_violation('rule_not_met:' // required_argument_name, &
           "rule '" // rule_key // " = " // rule_value // "' requires argument '" // &
           required_argument_name // "', which is missing")
   end function rule_not_met_violation

   ! Spec scenarios F8, MF8 "Unknown key in the interface namespace".
   ! legal_names is the interface's declared attribute and call-scalar
   ! names, in sorted order.
   function unknown_key_violation(key, legal_names) result(violation)
      character(*), intent(in) :: key
      character(*), intent(in) :: legal_names(:)
      type(CallbackContractViolation) :: violation

      violation = new_violation('unknown_key:' // key, &
           "'" // key // "' is not a declared attribute or call scalar; legal names: " // &
           join(legal_names))
   end function unknown_key_violation

   ! Spec scenario MF1b "Optional weighted pair half bound".
   function weight_missing_violation(weight_name, argument_name) result(violation)
      character(*), intent(in) :: weight_name
      character(*), intent(in) :: argument_name
      type(CallbackContractViolation) :: violation

      violation = new_violation('weight_missing:' // weight_name, &
           "'" // weight_name // "' is missing as the weight of '" // argument_name // "'")
   end function weight_missing_violation

   ! Spec scenario MF4 "Weight on another geometry".
   function weight_geometry_violation(weight_name, argument_name) result(violation)
      character(*), intent(in) :: weight_name
      character(*), intent(in) :: argument_name
      type(CallbackContractViolation) :: violation

      violation = new_violation('weight_geometry:' // weight_name, &
           "'" // weight_name // "' is on a different geometry than '" // argument_name // "'")
   end function weight_geometry_violation

   ! Comma-space joins names(:) for an "allowed values:"/"legal names:"
   ! list - trim(to_string(...)) is not involved; names are already
   ! character, this just inserts the ", " separators.
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

end module mapl_CallbackContractViolation_mod
