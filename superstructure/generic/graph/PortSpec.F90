#include "MAPL.h"

!------------------------------------------------------------------------------
! PortSpec: a TransformGraphNode's declared named-argument specification
! for one input or output port (spec/10-transforms-and-ports.md
! REQ-XFORM-002/003/004) - a name plus an optional expected GraphStateItem
! kind constraint. Declaration-only: this type never holds a bound
! NodeId (that is the external port-binding table's job,
! REQ-XFORM-005 - see mapl_PortBindingTable_mod).
!------------------------------------------------------------------------------
module mapl_PortSpec_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, MAPL_STATEITEM_NOTFOUND
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: PortSpec

   type :: PortSpec
      private
      character(:), allocatable :: name
      logical :: kind_constrained = .false.
      type(MAPL_StateItem_Flag) :: expected_kind = MAPL_STATEITEM_NOTFOUND
   contains
      procedure :: get_name => portspec_get_name
      procedure :: is_kind_constrained => portspec_is_kind_constrained
      procedure :: get_expected_kind => portspec_get_expected_kind
   end type PortSpec

   interface PortSpec
      module procedure new_PortSpec_any
      module procedure new_PortSpec_with_kind
   end interface PortSpec

contains

   ! Unconstrained port: "I have an input/output named X" with no
   ! expected-kind check performed at binding time.
   function new_PortSpec_any(name) result(spec)
      character(*), intent(in) :: name
      type(PortSpec) :: spec

      spec%name = name
      spec%kind_constrained = .false.
   end function new_PortSpec_any

   ! Kind-constrained port: REQ-XFORM-003's "expected GraphValue kind",
   ! expressed here as an expected MAPL_StateItem_Flag (the concrete
   ! payload's own two-tier classification, spec/04-graph-value-
   ! hierarchy.md REQ-SI-002b), checked by ComponentGraph%bind_port()
   ! against the bound target's GraphStateItem%variant() when the target is
   ! a StateItemNode.
   function new_PortSpec_with_kind(name, expected_kind) result(spec)
      character(*), intent(in) :: name
      type(MAPL_StateItem_Flag), intent(in) :: expected_kind
      type(PortSpec) :: spec

      spec%name = name
      spec%kind_constrained = .true.
      spec%expected_kind = expected_kind
   end function new_PortSpec_with_kind

   function portspec_get_name(this) result(name)
      class(PortSpec), intent(in) :: this
      character(:), allocatable :: name

      name = this%name
   end function portspec_get_name

   logical function portspec_is_kind_constrained(this) result(constrained)
      class(PortSpec), intent(in) :: this

      constrained = this%kind_constrained
   end function portspec_is_kind_constrained

   function portspec_get_expected_kind(this, rc) result(expected_kind)
      class(PortSpec), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: expected_kind

      _ASSERT(this%kind_constrained, 'PortSpec: get_expected_kind called on an unconstrained port')
      expected_kind = this%expected_kind

      _RETURN(_SUCCESS)
   end function portspec_get_expected_kind

end module mapl_PortSpec_mod
