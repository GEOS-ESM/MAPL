#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackInterface: a reusable callback contract (spec/15-callbacks.md
! REQ-CB-002/003) - argument name -> CallbackArgumentSpec, plus method
! name -> CallbackMethodSpec. Built through a declare_argument /
! declare_method / set_method_argument_access sequence, each rejecting a
! duplicate name at its own level (no-silent-replace, matching
! MethodGraphNode.declare_argument's own convention).
!
! Carries no service-name field of its own (REQ-CB-002: "Service name is
! the key in CallbackInterfaceRegistry's lookup map and MUST NOT be
! duplicated as a field inside CallbackInterface itself") -
! CallbackInterfaceRegistry.F90 owns that association.
!
! set_method_argument_access requires both the method name and the
! argument name to already be declared on this interface before
! mutating the named method's own CallbackMethodSpec in place via
! %at() pointer access - the same "declare then mutate via %at()
! pointer" idiom MethodGraphNode.F90's own node_declare_argument/
! node_bind_argument already established, applied one level deeper
! (callback-data-model-registry design.md Decision 3). This ordering
! requirement (argument must be declared before any method may declare
! access to it) is enforced by an explicit _ASSERT, not a silent no-op
! (design.md Risks).
!------------------------------------------------------------------------------
module mapl_CallbackInterface_mod
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec
   use mapl_CallbackMethodSpecMap_mod, only: CallbackMethodSpecMap
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: CallbackInterface

   type :: CallbackInterface
      private
      type(CallbackArgumentSpecMap) :: arguments
      type(CallbackMethodSpecMap) :: methods
   contains
      procedure :: declare_argument => interface_declare_argument
      procedure :: is_argument => interface_is_argument
      procedure :: get_argument => interface_get_argument
      procedure :: get_arguments => interface_get_arguments
      procedure :: declare_method => interface_declare_method
      procedure :: is_method => interface_is_method
      procedure :: get_method => interface_get_method
      procedure :: get_methods => interface_get_methods
      procedure :: set_method_argument_access => interface_set_method_argument_access
   end type CallbackInterface

contains

   ! REQ-CB-003: declares a named argument with its expected kind.
   ! Rejects a duplicate name, leaving the existing declaration
   ! unchanged (spec scenario "Duplicate argument name is rejected").
   subroutine interface_declare_argument(this, name, expected_kind, rc)
      class(CallbackInterface), intent(inout) :: this
      character(*), intent(in) :: name
      type(MAPL_StateItem_Flag), intent(in) :: expected_kind
      integer, optional, intent(out) :: rc

      _ASSERT(this%arguments%count(name) == 0, 'CallbackInterface: argument name already declared')

      call this%arguments%insert(name, CallbackArgumentSpec(name, expected_kind))

      _RETURN(_SUCCESS)
   end subroutine interface_declare_argument

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

      found => this%arguments%at(name)
      _ASSERT(associated(found), 'CallbackInterface: get_argument - argument not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function interface_get_argument

   function interface_get_arguments(this) result(arguments)
      class(CallbackInterface), intent(in) :: this
      type(CallbackArgumentSpecMap) :: arguments

      arguments = this%arguments
   end function interface_get_arguments

   ! REQ-CB-003: declares a named method with no argument-access entries
   ! yet - set_method_argument_access (below) fills those in. Rejects a
   ! duplicate name, leaving the existing declaration unchanged (spec
   ! scenario "Duplicate method name is rejected").
   subroutine interface_declare_method(this, name, rc)
      class(CallbackInterface), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(CallbackMethodSpec) :: empty

      _ASSERT(this%methods%count(name) == 0, 'CallbackInterface: method name already declared')

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

      found => this%methods%at(name)
      _ASSERT(associated(found), 'CallbackInterface: get_method - method not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function interface_get_method

   function interface_get_methods(this) result(methods)
      class(CallbackInterface), intent(in) :: this
      type(CallbackMethodSpecMap) :: methods

      methods = this%methods
   end function interface_get_methods

   ! REQ-CB-004: assigns method_name's access mode for argument_name.
   ! Requires both names to already be declared on this interface (spec
   ! scenarios "Method argument access for an undeclared argument/method
   ! is rejected") before mutating the method's own CallbackMethodSpec
   ! in place via %at() pointer access (module header, design.md
   ! Decision 3).
   subroutine interface_set_method_argument_access(this, method_name, argument_name, access, rc)
      class(CallbackInterface), intent(inout) :: this
      character(*), intent(in) :: method_name
      character(*), intent(in) :: argument_name
      type(AccessSpec), intent(in) :: access
      integer, optional, intent(out) :: rc

      type(CallbackMethodSpec), pointer :: spec_ptr
      integer :: status

      _ASSERT(this%is_method(method_name), 'CallbackInterface: set_method_argument_access - method not declared')
      _ASSERT(this%is_argument(argument_name), 'CallbackInterface: set_method_argument_access - argument not declared')

      spec_ptr => this%methods%at(method_name)
      _ASSERT(associated(spec_ptr), 'CallbackInterface: set_method_argument_access - method not found')

      call spec_ptr%declare_argument_access(argument_name, access, _RC)

      _RETURN(_SUCCESS)
   end subroutine interface_set_method_argument_access

end module mapl_CallbackInterface_mod
