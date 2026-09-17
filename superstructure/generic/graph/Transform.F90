#include "MAPL.h"

!------------------------------------------------------------------------------
! Transform: abstract computation descriptor - named, multi-input/multi-
! output port declarations (REQ-XFORM-002/003/004) plus a deferred
! compute(this, rc) - with no graph/revision/NodeId concept of its own,
! matching how GraphStateItem (StateItemNode's payload) carries no revision
! concept either: both are the "value/behavior" half of their respective
! node kind, deliberately independent of graph plumbing, so each is
! usable/testable in complete isolation from ComponentGraph/
! DependencyNetwork/NodeRevision.
!
! A real Transform (Phase 3/4, in MAPL - e.g. ConvertUnitsTransform)
! extends this directly, declaring its own fixed port shape (via
! declare_input_port()/declare_output_port()) in its own constructor -
! port shape is intrinsic per-subclass domain knowledge, not something
! rediscovered or duplicated by whatever code later wires an instance
! into a graph. TransformGraphNode (see mapl_TransformGraphNode_mod)
! holds one Transform (component name "transformer") and delegates its
! own port-query methods to it, adding only what IS a graph-plumbing
! concern on top: NodeId/lifecycle (inherited via OperationGraphNode/
! BaseGraphNode) and revision/staleness bookkeeping
! (spec/11-revision-and-update.md REQ-REV-005..007).
!------------------------------------------------------------------------------
module mapl_Transform_mod
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_PortSpecMap_mod
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: Transform
   public :: PortSpec
   public :: PortSpecMap
   public :: PortSpecMapIterator
   ! Re-exported so a client importing PortSpecMapIterator from this
   ! module can also compare iterators with ftn_end() - see
   ! mapl_TransformGraphNode_mod's identical rationale for its own
   ! operator re-exports.
   public :: operator(==)
   public :: operator(/=)

   type, abstract :: Transform
      private
      type(PortSpecMap) :: input_ports
      type(PortSpecMap) :: output_ports
   contains
      procedure :: declare_input_port => transform_declare_input_port
      procedure :: declare_output_port => transform_declare_output_port
      procedure :: is_input_port => transform_is_input_port
      procedure :: is_output_port => transform_is_output_port
      procedure :: get_input_port => transform_get_input_port
      procedure :: get_output_port => transform_get_output_port
      procedure :: get_input_ports => transform_get_input_ports
      procedure :: get_output_ports => transform_get_output_ports
      procedure(transform_compute_interface), deferred :: compute
   end type Transform

   abstract interface
      subroutine transform_compute_interface(this, rc)
         import :: Transform
         class(Transform), intent(inout) :: this
         integer, intent(out) :: rc
      end subroutine transform_compute_interface
   end interface

contains

   ! -- port declarations (REQ-XFORM-002/003/004) -------------------------

   subroutine transform_declare_input_port(this, name, rc, expected_kind)
      class(Transform), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      type(PortSpec) :: spec

      _ASSERT(this%input_ports%count(name) == 0, 'Transform: input port name already declared')

      if (present(expected_kind)) then
         spec = PortSpec(name, expected_kind)
      else
         spec = PortSpec(name)
      end if
      call this%input_ports%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine transform_declare_input_port

   subroutine transform_declare_output_port(this, name, rc, expected_kind)
      class(Transform), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      type(PortSpec) :: spec

      _ASSERT(this%output_ports%count(name) == 0, 'Transform: output port name already declared')

      if (present(expected_kind)) then
         spec = PortSpec(name, expected_kind)
      else
         spec = PortSpec(name)
      end if
      call this%output_ports%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine transform_declare_output_port

   logical function transform_is_input_port(this, name) result(is_port)
      class(Transform), intent(in) :: this
      character(*), intent(in) :: name

      is_port = this%input_ports%count(name) > 0
   end function transform_is_input_port

   logical function transform_is_output_port(this, name) result(is_port)
      class(Transform), intent(in) :: this
      character(*), intent(in) :: name

      is_port = this%output_ports%count(name) > 0
   end function transform_is_output_port

   function transform_get_input_port(this, name, rc) result(spec)
      class(Transform), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      type(PortSpec), pointer :: found

      found => this%input_ports%at(name)
      _ASSERT(associated(found), 'Transform: input port not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function transform_get_input_port

   function transform_get_output_port(this, name, rc) result(spec)
      class(Transform), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      type(PortSpec), pointer :: found

      found => this%output_ports%at(name)
      _ASSERT(associated(found), 'Transform: output port not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function transform_get_output_port

   function transform_get_input_ports(this) result(ports)
      class(Transform), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = this%input_ports
   end function transform_get_input_ports

   function transform_get_output_ports(this) result(ports)
      class(Transform), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = this%output_ports
   end function transform_get_output_ports

end module mapl_Transform_mod
