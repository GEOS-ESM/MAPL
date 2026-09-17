#include "MAPL.h"

!------------------------------------------------------------------------------
! TransformGraphNode: concrete OperationGraphNode descendant representing
! a demand-driven MAPL Transform (spec/03-graph-node-hierarchy.md
! REQ-NODE-005, spec/10-transforms-and-ports.md REQ-XFORM-001..004).
!
! Holds exactly one Transform (component "transformer", see
! mapl_Transform_mod for why port declarations and compute() live there,
! not here) and delegates every port-query method to it unchanged -
! REQ-XFORM-004's "declaration belongs to the Transform" is honored by
! the delegation target, not by duplicating storage here. This mirrors
! StateItemNode holding a GraphStateItem as "payload": the node adds exactly
! the graph-plumbing concerns its payload/transformer does not carry -
! NodeId/lifecycle (inherited via OperationGraphNode/BaseGraphNode) and,
! here, the staleness/baseline bookkeeping the demand-driven update
! algorithm needs (spec/11-revision-and-update.md REQ-REV-005..007):
! has_run()/needs_execution()/record_run(), exposed as methods per
! REQ-REV-003's "expose via methods, not raw field access" pattern,
! already established by StateItemNode's own revision accessors.
! ComponentGraph%update() (mapl_ComponentGraph_DemandDrivenUpdate_smod)
! is a thin traversal/orchestration layer with no persistent state of
! its own; this node carries all of its own execution-history state.
!
! Port *bindings* (which concrete NodeId fills a declared port, for a
! given DependencyNetworkId) are explicitly NOT stored here - see
! mapl_PortBindingTable_mod / ComponentGraph%bind_port(), REQ-XFORM-005.
!------------------------------------------------------------------------------
module mapl_TransformGraphNode_mod
   use mapl_OperationGraphNode_mod, only: OperationGraphNode
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_Transform_mod, only: Transform, PortSpec, PortSpecMap, PortSpecMapIterator, &
                                   operator(==), operator(/=)
   use mapl_PortNameRevisionMap_mod
   use mapl_NodeRevision_mod, only: NodeRevision, operator(==), operator(/=)
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: TransformGraphNode
   public :: Transform
   public :: PortSpec
   public :: PortSpecMap
   public :: PortSpecMapIterator
   public :: PortNameRevisionMap
   public :: PortNameRevisionMapIterator
   ! Re-exported so that a client (e.g.
   ! mapl_ComponentGraph_DemandDrivenUpdate_smod) importing
   ! PortSpecMapIterator/PortNameRevisionMapIterator from this module can
   ! also compare iterators with ftn_end() - the comparison operators are
   ! merged generics (multiple specific implementations under one shared
   ! name, resolved by argument type at each call site), matching the
   ! pattern mapl_StateItem_mod already uses for MAPL_StateItem_Flag's
   ! operator(==)/operator(/=).
   public :: operator(==)
   public :: operator(/=)

   type, extends(OperationGraphNode) :: TransformGraphNode
      private
      class(Transform), allocatable :: transformer
      type(PortNameRevisionMap) :: last_run_input_revisions
      logical :: has_run_flag = .false.
   contains
      ! -- port queries: pure delegation to the held Transform ------------
      procedure :: declare_input_port => node_declare_input_port
      procedure :: declare_output_port => node_declare_output_port
      procedure :: is_input_port => node_is_input_port
      procedure :: is_output_port => node_is_output_port
      procedure :: get_input_port => node_get_input_port
      procedure :: get_output_port => node_get_output_port
      procedure :: get_input_ports => node_get_input_ports
      procedure :: get_output_ports => node_get_output_ports
      ! -- staleness / execution bookkeeping (REQ-REV-005..007) -----------
      procedure :: has_run => node_has_run
      procedure :: needs_execution => node_needs_execution
      procedure :: record_run => node_record_run
      procedure :: execute => node_execute
   end type TransformGraphNode

   interface TransformGraphNode
      module procedure new_TransformGraphNode
   end interface TransformGraphNode

contains

   function new_TransformGraphNode(id, transformer) result(node)
      type(NodeId), intent(in) :: id
      class(Transform), intent(in) :: transformer
      type(TransformGraphNode) :: node

      node%BaseGraphNode = BaseGraphNode(id)
      allocate(node%transformer, source=transformer)
   end function new_TransformGraphNode

   ! -- port queries: pure delegation ---------------------------------------
   ! Declaration ownership lives on Transform (mapl_Transform_mod) - a
   ! concrete Transform subclass normally declares its own fixed port
   ! shape in its own constructor, but these delegators remain available
   ! for callers (including synthetic test doubles) that prefer to
   ! configure a generic Transform instance after construction instead.

   subroutine node_declare_input_port(this, name, rc, expected_kind)
      class(TransformGraphNode), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      call this%transformer%declare_input_port(name, rc=rc, expected_kind=expected_kind)
   end subroutine node_declare_input_port

   subroutine node_declare_output_port(this, name, rc, expected_kind)
      class(TransformGraphNode), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag), optional, intent(in) :: expected_kind

      call this%transformer%declare_output_port(name, rc=rc, expected_kind=expected_kind)
   end subroutine node_declare_output_port

   logical function node_is_input_port(this, name) result(is_port)
      class(TransformGraphNode), intent(in) :: this
      character(*), intent(in) :: name

      is_port = this%transformer%is_input_port(name)
   end function node_is_input_port

   logical function node_is_output_port(this, name) result(is_port)
      class(TransformGraphNode), intent(in) :: this
      character(*), intent(in) :: name

      is_port = this%transformer%is_output_port(name)
   end function node_is_output_port

   function node_get_input_port(this, name, rc) result(spec)
      class(TransformGraphNode), intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      spec = this%transformer%get_input_port(name, rc=rc)
   end function node_get_input_port

   function node_get_output_port(this, name, rc) result(spec)
      class(TransformGraphNode), intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      spec = this%transformer%get_output_port(name, rc=rc)
   end function node_get_output_port

   function node_get_input_ports(this) result(ports)
      class(TransformGraphNode), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = this%transformer%get_input_ports()
   end function node_get_input_ports

   function node_get_output_ports(this) result(ports)
      class(TransformGraphNode), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = this%transformer%get_output_ports()
   end function node_get_output_ports

   ! -- staleness / execution bookkeeping (REQ-REV-005..007) ----------------

   logical function node_has_run(this) result(has_run)
      class(TransformGraphNode), intent(in) :: this

      has_run = this%has_run_flag
   end function node_has_run

   ! REQ-REV-006 steps 2-4: stale if never run, or if any current
   ! declared-input revision differs from (or is absent from) the
   ! baseline recorded after the last successful execution.
   function node_needs_execution(this, current_input_revisions) result(stale)
      class(TransformGraphNode), target, intent(in) :: this
      type(PortNameRevisionMap), target, intent(in) :: current_input_revisions
      logical :: stale

      type(PortNameRevisionMapIterator) :: iter
      character(:), allocatable :: name
      type(NodeRevision) :: current_rev
      type(NodeRevision), pointer :: baseline_rev

      if (.not. this%has_run_flag) then
         stale = .true.
         return
      end if

      stale = .false.
      iter = current_input_revisions%ftn_begin()
      do while (iter /= current_input_revisions%ftn_end())
         call iter%next()
         name = iter%first()
         current_rev = iter%second()
         baseline_rev => this%last_run_input_revisions%at(name)
         if (.not. associated(baseline_rev)) then
            stale = .true.
            return
         end if
         if (current_rev /= baseline_rev) then
            stale = .true.
            return
         end if
      end do
   end function node_needs_execution

   ! REQ-REV-006 step 6: the executed input revisions become the new
   ! baseline for future staleness comparisons.
   subroutine node_record_run(this, current_input_revisions, rc)
      class(TransformGraphNode), intent(inout) :: this
      type(PortNameRevisionMap), intent(in) :: current_input_revisions
      integer, optional, intent(out) :: rc

      this%last_run_input_revisions = current_input_revisions
      this%has_run_flag = .true.

      _RETURN(_SUCCESS)
   end subroutine node_record_run

   ! REQ-XFORM-001: invokes the held Transform. Does not itself touch any
   ! input/output NodeId or advance any revision - that orchestration is
   ! ComponentGraph%update()'s job (mapl_ComponentGraph_DemandDrivenUpdate_smod),
   ! which calls this only after needs_execution() reports true and
   ! advances outputs / records the baseline only after this succeeds.
   subroutine node_execute(this, rc)
      class(TransformGraphNode), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(allocated(this%transformer), 'TransformGraphNode: execute called with no Transform attached')
      call this%transformer%compute(_RC)

      _RETURN(_SUCCESS)
   end subroutine node_execute

end module mapl_TransformGraphNode_mod
