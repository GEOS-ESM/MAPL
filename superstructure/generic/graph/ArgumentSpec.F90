#include "MAPL.h"

!------------------------------------------------------------------------------
! ArgumentSpec: a MethodGraphNode's declared named-argument specification
! (spec/12-methods-and-drivers.md REQ-MTH-002) - a name plus an
! AccessSpec plus an optional expected GraphStateItem kind constraint.
! Mirrors PortSpec's shape exactly (PortSpec.F90), adding exactly one
! field (AccessSpec) on top of it - AccessSpec already encodes
! direction (including the INOUT/UNSPECIFIED cases a two-map input/
! output split cannot represent), so one map keyed by name, each entry
! carrying its own AccessSpec, is sufficient (design.md Decisions).
!
! Declaration-only: this type never holds a bound NodeId (that is
! MethodGraphNode's own on-node StateItemMemberMap binding storage -
! see mapl_MethodGraphNode_mod).
!------------------------------------------------------------------------------
module mapl_ArgumentSpec_mod
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, MAPL_STATEITEM_NOTFOUND
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ArgumentSpec

   type :: ArgumentSpec
      private
      character(:), allocatable :: name
      type(AccessSpec) :: access
      logical :: kind_constrained = .false.
      type(MAPL_StateItem_Flag) :: expected_kind = MAPL_STATEITEM_NOTFOUND
   contains
      procedure :: get_name => argspec_get_name
      procedure :: get_access => argspec_get_access
      procedure :: is_kind_constrained => argspec_is_kind_constrained
      procedure :: get_expected_kind => argspec_get_expected_kind
   end type ArgumentSpec

   interface ArgumentSpec
      module procedure new_ArgumentSpec_any
      module procedure new_ArgumentSpec_with_kind
   end interface ArgumentSpec

contains

   ! Unconstrained argument: "I have an argument named X with access
   ! mode A" with no expected-kind check performed at binding time.
   function new_ArgumentSpec_any(name, access) result(spec)
      character(*), intent(in) :: name
      type(AccessSpec), intent(in) :: access
      type(ArgumentSpec) :: spec

      spec%name = name
      spec%access = access
      spec%kind_constrained = .false.
   end function new_ArgumentSpec_any

   ! Kind-constrained argument: checked against the actual bound value's
   ! kind by whoever calls bind_argument() (MethodGraphNode has no
   ! ComponentGraph reference of its own - design.md Decisions).
   function new_ArgumentSpec_with_kind(name, access, expected_kind) result(spec)
      character(*), intent(in) :: name
      type(AccessSpec), intent(in) :: access
      type(MAPL_StateItem_Flag), intent(in) :: expected_kind
      type(ArgumentSpec) :: spec

      spec%name = name
      spec%access = access
      spec%kind_constrained = .true.
      spec%expected_kind = expected_kind
   end function new_ArgumentSpec_with_kind

   function argspec_get_name(this) result(name)
      class(ArgumentSpec), intent(in) :: this
      character(:), allocatable :: name

      name = this%name
   end function argspec_get_name

   function argspec_get_access(this) result(access)
      class(ArgumentSpec), intent(in) :: this
      type(AccessSpec) :: access

      access = this%access
   end function argspec_get_access

   logical function argspec_is_kind_constrained(this) result(constrained)
      class(ArgumentSpec), intent(in) :: this

      constrained = this%kind_constrained
   end function argspec_is_kind_constrained

   function argspec_get_expected_kind(this, rc) result(expected_kind)
      class(ArgumentSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: expected_kind

      _ASSERT(this%kind_constrained, 'ArgumentSpec: get_expected_kind called on an unconstrained argument')
      expected_kind = this%expected_kind

      _RETURN(_SUCCESS)
   end function argspec_get_expected_kind

end module mapl_ArgumentSpec_mod
