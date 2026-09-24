#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackMethodSpec: one callback method's argument-access declarations
! (spec/15-callbacks.md REQ-CB-004) - argument name -> AccessSpec,
! allowing one argument shared across an interface's methods to carry a
! different access mode in each method (e.g. the PassiveTracer example,
! §15.3: "tracers" is OUT in "get" and IN in "put").
!
! A thin wrapper around a new AccessSpecMap (callback-data-model-
! registry design.md Decision 2), not a bespoke map and not a reuse of
! ArgumentSpecMap - ArgumentSpec carries an unused kind-constraint field
! that would be dead weight on every entry here (see design.md Decision
! 2's rejected alternative).
!------------------------------------------------------------------------------
module mapl_CallbackMethodSpec_mod
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_AccessSpecMap_mod, only: AccessSpecMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackMethodSpec

   type :: CallbackMethodSpec
      private
      type(AccessSpecMap) :: accesses
   contains
      procedure :: declare_argument_access => methodspec_declare_argument_access
      procedure :: is_argument_access => methodspec_is_argument_access
      procedure :: get_argument_access => methodspec_get_argument_access
      procedure :: get_argument_accesses => methodspec_get_argument_accesses
   end type CallbackMethodSpec

contains

   ! REQ-CB-004: declares this method's access mode for one argument
   ! name. Rejects a duplicate declaration for the same argument name on
   ! this method, leaving the existing declaration unchanged - the same
   ! no-silent-replace convention used throughout this module family.
   subroutine methodspec_declare_argument_access(this, argument_name, access, rc)
      class(CallbackMethodSpec), intent(inout) :: this
      character(*), intent(in) :: argument_name
      type(AccessSpec), intent(in) :: access
      integer, optional, intent(out) :: rc

      _ASSERT(this%accesses%count(argument_name) == 0, 'CallbackMethodSpec: argument access already declared for this method')

      call this%accesses%insert(argument_name, access)

      _RETURN(_SUCCESS)
   end subroutine methodspec_declare_argument_access

   logical function methodspec_is_argument_access(this, argument_name) result(is_access)
      class(CallbackMethodSpec), intent(in) :: this
      character(*), intent(in) :: argument_name

      is_access = this%accesses%count(argument_name) > 0
   end function methodspec_is_argument_access

   function methodspec_get_argument_access(this, argument_name, rc) result(access)
      class(CallbackMethodSpec), target, intent(in) :: this
      character(*), intent(in) :: argument_name
      integer, optional, intent(out) :: rc
      type(AccessSpec) :: access

      type(AccessSpec), pointer :: found

      found => this%accesses%at(argument_name)
      _ASSERT(associated(found), 'CallbackMethodSpec: get_argument_access - argument access not declared')
      access = found

      _RETURN(_SUCCESS)
   end function methodspec_get_argument_access

   function methodspec_get_argument_accesses(this) result(accesses)
      class(CallbackMethodSpec), intent(in) :: this
      type(AccessSpecMap) :: accesses

      accesses = this%accesses
   end function methodspec_get_argument_accesses

end module mapl_CallbackMethodSpec_mod
