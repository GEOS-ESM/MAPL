#include "MAPL.h"

!------------------------------------------------------------------------------
! MethodGraphNode: concrete OperationGraphNode descendant representing
! an explicitly invoked method - either a GridComp initialize/run phase
! or an attached ESMF State callback method - through one node type
! (spec/03-graph-node-hierarchy.md REQ-NODE-006, spec/12-methods-and-
! drivers.md REQ-MTH-001/002/004/005/006).
!
! Holds exactly one MethodInvocationAdapter (mapl_MethodInvocationAdapter_mod)
! and delegates invocation to it unchanged - the node itself never
! branches on, or otherwise needs to know, which concrete adapter kind
! is attached (REQ-MTH-002). Mirrors TransformGraphNode holding one
! Transform (TransformGraphNode.F90): NodeId/lifecycle come from
! OperationGraphNode/BaseGraphNode, and this type adds exactly the
! plumbing its payload does not carry - named argument declarations
! (ArgumentSpecMap, REQ-MTH-002) and argument bindings.
!
! Argument bindings (name -> bound NodeId) are stored on-node, reusing
! the existing StateItemMemberMap type unchanged, rather than routed
! through the external PortBindingTable/ComponentGraph%bind_port()
! mechanism TransformGraphNode's port bindings use (REQ-XFORM-005):
! MethodGraphNode is permanently excluded from ComponentGraph's
! demand-driven dispatch (REQ-NODE-007, already a no-op class default
! branch in mapl_ComponentGraph_DemandDrivenUpdate_smod), so there is no
! shared traversal code that would otherwise need a per-node-kind
! binding-storage branch (design.md Decisions).
!
! REQ-MTH-004 (import/export as one conceptual method-argument state)
! and REQ-MTH-005/REQ-NODE-008 (no separate "component" node) are
! satisfied by absence: arguments/bindings are a single flat map each,
! with no import/export substructure, and no component-level node type
! is introduced anywhere in this module.
!------------------------------------------------------------------------------
module mapl_MethodGraphNode_mod
   use mapl_OperationGraphNode_mod, only: OperationGraphNode
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_MethodInvocationAdapter_mod, only: MethodInvocationAdapter
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_ArgumentSpec_mod, only: ArgumentSpec
   use mapl_ArgumentSpecMap_mod, only: ArgumentSpecMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, operator(==)
   use ESMF, only: ESMF_Clock
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: MethodGraphNode

   type, extends(OperationGraphNode) :: MethodGraphNode
      private
      class(MethodInvocationAdapter), allocatable :: adapter
      type(ArgumentSpecMap) :: arguments
      type(StateItemMemberMap) :: bindings
   contains
      procedure :: declare_argument => node_declare_argument
      procedure :: is_argument => node_is_argument
      procedure :: get_argument => node_get_argument
      procedure :: get_arguments => node_get_arguments
      procedure :: bind_argument => node_bind_argument
      procedure :: get_argument_binding => node_get_argument_binding
      procedure :: get_argument_bindings => node_get_argument_bindings
      procedure :: invoke => node_invoke
   end type MethodGraphNode

   interface MethodGraphNode
      module procedure new_MethodGraphNode
   end interface MethodGraphNode

contains

   ! adapter is optional so a MethodGraphNode MAY be constructed with no
   ! invocation adapter attached yet - invoke() then fails loudly
   ! (below) rather than silently doing nothing (spec scenario "No
   ! adapter attached is not invocable").
   function new_MethodGraphNode(id, adapter) result(node)
      type(NodeId), intent(in) :: id
      class(MethodInvocationAdapter), optional, intent(in) :: adapter
      type(MethodGraphNode) :: node

      node%BaseGraphNode = BaseGraphNode(id)
      if (present(adapter)) allocate(node%adapter, source=adapter)
   end function new_MethodGraphNode

   ! REQ-MTH-002: named argument declaration, with access mode and
   ! optional expected-kind constraint. Rejects a duplicate name,
   ! leaving the existing declaration unchanged (spec scenario
   ! "Duplicate argument name is rejected").
   subroutine node_declare_argument(this, name, access, rc, expected_kind)
      class(MethodGraphNode), intent(inout) :: this
      character(*), intent(in) :: name
      type(AccessSpec), intent(in) :: access
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      type(ArgumentSpec) :: spec

      _ASSERT(this%arguments%count(name) == 0, 'MethodGraphNode: argument name already declared')

      if (present(expected_kind)) then
         spec = ArgumentSpec(name, access, expected_kind)
      else
         spec = ArgumentSpec(name, access)
      end if
      call this%arguments%insert(name, spec)

      _RETURN(_SUCCESS)
   end subroutine node_declare_argument

   logical function node_is_argument(this, name) result(is_arg)
      class(MethodGraphNode), intent(in) :: this
      character(*), intent(in) :: name

      is_arg = this%arguments%count(name) > 0
   end function node_is_argument

   function node_get_argument(this, name, rc) result(spec)
      class(MethodGraphNode), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(ArgumentSpec) :: spec

      type(ArgumentSpec), pointer :: found

      found => this%arguments%at(name)
      _ASSERT(associated(found), 'MethodGraphNode: get_argument - argument not declared')
      spec = found

      _RETURN(_SUCCESS)
   end function node_get_argument

   function node_get_arguments(this) result(arguments)
      class(MethodGraphNode), intent(in) :: this
      type(ArgumentSpecMap) :: arguments

      arguments = this%arguments
   end function node_get_arguments

   ! REQ-MTH-002: binds a previously-declared argument to a concrete
   ! value identity. Requires the argument to already be declared (spec
   ! scenario "Binding an undeclared argument is rejected"); rejects
   ! rebinding an already-bound name, matching every other
   ! no-silent-replace registration API in this repo (ComponentGraph
   ! node/port registration, GraphStateItem membership maps,
   ! PortBindingTable). If the declaration is kind-constrained,
   ! actual_kind MUST be supplied and MUST match (spec scenarios
   ! "Kind-constrained argument accepts/rejects a ... binding") -
   ! MethodGraphNode has no ComponentGraph reference of its own, so the
   ! actual kind is supplied directly by the caller rather than looked
   ! up (design.md Decisions).
   subroutine node_bind_argument(this, name, target_id, rc, actual_kind)
      class(MethodGraphNode), intent(inout) :: this
      character(*), intent(in) :: name
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: actual_kind

      type(ArgumentSpec) :: spec
      type(MAPL_StateItem_Flag) :: expected_kind
      integer :: status

      _ASSERT(this%is_argument(name), 'MethodGraphNode: bind_argument - argument name not declared')
      _ASSERT(this%bindings%count(name) == 0, 'MethodGraphNode: bind_argument - argument name is already bound')

      spec = this%get_argument(name, _RC)
      if (spec%is_kind_constrained()) then
         _ASSERT(present(actual_kind), 'MethodGraphNode: bind_argument - argument is kind-constrained but no actual_kind supplied')
         expected_kind = spec%get_expected_kind(_RC)
         _ASSERT(actual_kind == expected_kind, 'MethodGraphNode: bind_argument - bound value kind does not match declared argument kind constraint')
      end if

      call this%bindings%insert(name, target_id)

      _RETURN(_SUCCESS)
   end subroutine node_bind_argument

   function node_get_argument_binding(this, name) result(target_id)
      class(MethodGraphNode), target, intent(in) :: this
      character(*), intent(in) :: name
      type(NodeId), pointer :: target_id

      target_id => this%bindings%at(name)
   end function node_get_argument_binding

   function node_get_argument_bindings(this) result(bindings)
      class(MethodGraphNode), intent(in) :: this
      type(StateItemMemberMap) :: bindings

      bindings = this%bindings
   end function node_get_argument_bindings

   ! REQ-MTH-002/003: gathers the node's current argument declarations
   ! and bindings and calls the attached adapter's own invoke() - no
   ! other logic, no branching on the adapter's dynamic type. Fails
   ! loudly if no adapter is attached (spec scenario "No adapter
   ! attached is not invocable"). clock (REQ-MTH-006) is passed through
   ! unchanged, never stored as a NodeId or other graph-visible entity.
   subroutine node_invoke(this, rc, clock)
      class(MethodGraphNode), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_Clock), optional, intent(in) :: clock

      integer :: status

      _ASSERT(allocated(this%adapter), 'MethodGraphNode: invoke called with no invocation adapter attached')

      call this%adapter%invoke(this%arguments, this%bindings, clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine node_invoke

end module mapl_MethodGraphNode_mod
