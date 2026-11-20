#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackMethodSpec: one callback method's argument-access declarations
! (spec/15-callbacks.md) - argument name -> AccessSpec,
! allowing one argument shared across an interface's methods to carry a
! different access mode in each method (e.g. the PassiveTracer example,
! §15.3: "tracers" is OUT in "get" and IN in "put").
!
! A thin wrapper around an AccessSpecMap, not a bespoke map and not a
! reuse of ArgumentSpecMap - ArgumentSpec carries an unused
! kind-constraint field that would be dead weight on every entry here.
!
! Per-argument options (CallbackArgumentOptions) live in a second map keyed
! by the same argument names; every declared argument has an entry in both,
! so an argument declared without options reports the default (no options).
!------------------------------------------------------------------------------
module mapl_CallbackMethodSpec_mod
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_AccessSpecMap_mod, only: AccessSpecMap
   use mapl_CallbackArgumentOptions_mod, only: CallbackArgumentOptions
   use mapl_CallbackArgumentOptionsMap_mod, only: CallbackArgumentOptionsMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackMethodSpec

   type :: CallbackMethodSpec
      private
      type(AccessSpecMap) :: accesses
      type(CallbackArgumentOptionsMap) :: options
   contains
      procedure :: declare_argument_access => methodspec_declare_argument_access
      procedure :: is_argument_access => methodspec_is_argument_access
      procedure :: get_argument_access => methodspec_get_argument_access
      procedure :: get_argument_options => methodspec_get_argument_options
      procedure :: get_argument_accesses => methodspec_get_argument_accesses
   end type CallbackMethodSpec

contains

   ! Declares this method's access mode, and optionally its per-argument
   ! options, for one argument name. Rejects a duplicate declaration for the
   ! same argument name on this method, leaving the existing declaration
   ! unchanged - the same no-silent-replace convention used throughout this
   ! module family. Without options, the argument reports the default
   ! (empty) options.
   subroutine methodspec_declare_argument_access(this, argument_name, &
        access, options, rc)
      class(CallbackMethodSpec), intent(inout) :: this
      character(*), intent(in) :: argument_name
      type(AccessSpec), intent(in) :: access
      type(CallbackArgumentOptions), optional, intent(in) :: options
      integer, optional, intent(out) :: rc

      type(CallbackArgumentOptions) :: declared_options

      _ASSERT(this%accesses%count(argument_name) == 0, 'CallbackMethodSpec: argument access already declared for this method')

      if (present(options)) declared_options = options

      call this%accesses%insert(argument_name, access)
      call this%options%insert(argument_name, declared_options)

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

   function methodspec_get_argument_options(this, argument_name, rc) &
        result(options)
      class(CallbackMethodSpec), target, intent(in) :: this
      character(*), intent(in) :: argument_name
      integer, optional, intent(out) :: rc
      type(CallbackArgumentOptions) :: options

      type(CallbackArgumentOptions), pointer :: found
      character(:), allocatable :: msg

      found => this%options%at(argument_name)
      msg = 'CallbackMethodSpec: get_argument_options - ' &
           // 'argument access not declared'
      _ASSERT(associated(found), msg)
      options = found

      _RETURN(_SUCCESS)
   end function methodspec_get_argument_options

   function methodspec_get_argument_accesses(this) result(accesses)
      class(CallbackMethodSpec), intent(in) :: this
      type(AccessSpecMap) :: accesses

      accesses = this%accesses
   end function methodspec_get_argument_accesses

end module mapl_CallbackMethodSpec_mod
