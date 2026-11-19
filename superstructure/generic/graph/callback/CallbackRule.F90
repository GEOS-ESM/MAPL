#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackRule: one rule of a CallbackInterface (spec/15-callbacks.md):
! "when attribute a has value v, arguments x, ... are required". It
! stores the attribute name, the triggering value as a string, and the
! names of the required arguments in the order given.
!
! This type only stores and exposes the rule. Whether the attribute and
! arguments are declared, whether the arguments are optional, and whether
! the value is legal for the attribute's type are checked against the
! whole interface, not here. The value and argument names are stored
! trimmed. The value may be empty; duplicate argument names are kept.
!------------------------------------------------------------------------------
module mapl_CallbackRule_mod
   use gFTL2_StringVector, only: StringVector
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackRule

   type :: CallbackRule
      private
      character(:), allocatable :: attribute
      character(:), allocatable :: value
      type(StringVector) :: required_arguments
   contains
      procedure :: get_attribute => rule_get_attribute
      procedure :: get_value => rule_get_value
      procedure :: get_required_arguments => rule_get_required_arguments
      procedure :: num_required_arguments => rule_num_required_arguments
   end type CallbackRule

   interface CallbackRule
      module procedure new_CallbackRule
   end interface CallbackRule

contains

   function failure_message(text, attribute) result(msg)
      character(*), intent(in) :: text
      character(*), intent(in) :: attribute
      character(:), allocatable :: msg

      msg = 'CallbackRule: ' // text // ': ' // attribute
   end function failure_message

   ! True when every name has at least one non-blank character.
   logical function all_names_nonblank(names) result(ok)
      character(*), intent(in) :: names(:)

      integer :: i

      ok = .true.
      do i = 1, size(names)
         ok = ok .and. len_trim(names(i)) > 0
      end do
   end function all_names_nonblank

   ! "When attribute has value, the arguments in requires are required."
   ! value is the string form of the triggering value. requires must
   ! name at least one argument.
   function new_CallbackRule(attribute, value, requires, rc) result(rule)
      character(*), intent(in) :: attribute
      character(*), intent(in) :: value
      character(*), intent(in) :: requires(:)
      integer, optional, intent(out) :: rc
      type(CallbackRule) :: rule

      character(:), allocatable :: msg
      integer :: i

      msg = failure_message('attribute name must not be empty', attribute)
      _ASSERT(len_trim(attribute) > 0, msg)
      msg = failure_message('a rule must require at least one argument', &
           attribute)
      _ASSERT(size(requires) > 0, msg)
      msg = failure_message('required argument name must not be empty', &
           attribute)
      _ASSERT(all_names_nonblank(requires), msg)

      rule%attribute = attribute
      rule%value = trim(value)
      do i = 1, size(requires)
         call rule%required_arguments%push_back(trim(requires(i)))
      end do

      _RETURN(_SUCCESS)
   end function new_CallbackRule

   function rule_get_attribute(this) result(attribute)
      class(CallbackRule), intent(in) :: this
      character(:), allocatable :: attribute

      attribute = this%attribute
   end function rule_get_attribute

   function rule_get_value(this) result(value)
      class(CallbackRule), intent(in) :: this
      character(:), allocatable :: value

      value = this%value
   end function rule_get_value

   function rule_get_required_arguments(this) result(names)
      class(CallbackRule), intent(in) :: this
      type(StringVector) :: names

      names = this%required_arguments
   end function rule_get_required_arguments

   integer function rule_num_required_arguments(this) result(n)
      class(CallbackRule), intent(in) :: this

      n = this%required_arguments%size()
   end function rule_num_required_arguments

end module mapl_CallbackRule_mod
