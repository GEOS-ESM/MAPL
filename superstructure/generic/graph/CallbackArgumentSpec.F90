#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackArgumentSpec: a CallbackInterface's named argument declaration
! (spec/15-callbacks.md REQ-CB-003) - a name plus a required expected
! MAPL_StateItem_Flag kind (e.g. the PassiveTracer example's "tracers,
! ESMF FieldBundle", §15.3). Carries no AccessSpec of its own - access is
! declared per-method by CallbackMethodSpec (REQ-CB-004: "one shared
! argument MAY have different access in different methods"); putting
! AccessSpec here as well, mirroring ArgumentSpec exactly, would invite a
! second, unused source of truth (callback-data-model-registry design.md
! Decision 1).
!
! Unlike ArgumentSpec's optional expected_kind, expected_kind is
! required here: every REQ-CB-003 example states one, and there is no
! unconstrained-callback-argument use case in the spec to support.
!------------------------------------------------------------------------------
module mapl_CallbackArgumentSpec_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   implicit none(type, external)
   private

   public :: CallbackArgumentSpec

   type :: CallbackArgumentSpec
      private
      character(:), allocatable :: name
      type(MAPL_StateItem_Flag) :: expected_kind
   contains
      procedure :: get_name => argspec_get_name
      procedure :: get_expected_kind => argspec_get_expected_kind
   end type CallbackArgumentSpec

   interface CallbackArgumentSpec
      module procedure new_CallbackArgumentSpec
   end interface CallbackArgumentSpec

contains

   function new_CallbackArgumentSpec(name, expected_kind) result(spec)
      character(*), intent(in) :: name
      type(MAPL_StateItem_Flag), intent(in) :: expected_kind
      type(CallbackArgumentSpec) :: spec

      spec%name = name
      spec%expected_kind = expected_kind
   end function new_CallbackArgumentSpec

   function argspec_get_name(this) result(name)
      class(CallbackArgumentSpec), intent(in) :: this
      character(:), allocatable :: name

      name = this%name
   end function argspec_get_name

   function argspec_get_expected_kind(this) result(expected_kind)
      class(CallbackArgumentSpec), intent(in) :: this
      type(MAPL_StateItem_Flag) :: expected_kind

      expected_kind = this%expected_kind
   end function argspec_get_expected_kind

end module mapl_CallbackArgumentSpec_mod
