#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphBuilder: the Phase 3b integration layer between the graph-neutral
! ComponentGraph/DependencyNetwork core and the rest of MAPL
! (spec/08-graph-builder.md REQ-GB-001/002/003, this change's proposal.md).
!
! Scope (roadmap sub-change 3b, spec/20-implementation-roadmap.md
! sec 20.4.1): advertising (creating StateItemNodes for advertised
! import/export/internal items), ordinary (exact short-name match)
! connection resolution restricted to MatchConnection - the concrete
! Connection subtype behind OuterMetaComponent%connect_all's "magic
! connect" behavior (superstructure/generic/OuterMetaComponent/connect_all.F90)
! - public-port/child-proxy population, and validate/freeze. Explicitly
! NOT in scope here: SimpleConnection/ReexportConnection, extension/
! mismatch-chain creation (3c), wildcard/callback resolution (Phase 4),
! and compiled execution (Phase 5/Q9) - see proposal.md "Explicitly out
! of scope".
!
! Design (design.md - Decisions): a stateless-per-call procedure set, not
! a persistent object - every procedure here takes the OuterMetaComponent
! it operates on as an explicit argument and reads/writes only that
! component's own ComponentGraph (obtained via the existing 3a accessor,
! get_component_graph) plus, for cross-boundary connection resolution,
! a named child's ComponentGraph/ComponentSpec reached via
! OuterMetaComponent's own get_child_component_graph()/
! get_child_component_spec() accessors (graphbuilder-code-quality-cleanup
! change) - a framework-internal carve-out (REQ-GB-002), not part of
! OuterMetaComponent's general public API (REQ-HIER-003/005), that these
! two accessors document at their own declaration site.
!
! Advertised-item and cross-graph-proxy identity lookup both reuse
! ComponentGraph's existing, already-specified "semantic resource index"
! (REQ-CG-001, add_resource_index()/get_resource_index()) rather than
! inventing a new lookup table: this is exactly the graph-neutral,
! public, name-keyed NodeId lookup REQ-CG-001 already provides, and using
! it here needs no ComponentGraph API change (design.md Non-Goals).
!
! Cross-component (parent/child) connections: a dependency edge can only
! be added to a network owned by the graph that owns both its endpoint
! NodeIds (ComponentGraph%add_dependency enforces this). A MatchConnection
! whose source or destination names a child therefore cannot be wired
! directly into that child's own network (nor would that be legal - a
! parent may not reach into a child's internal DependencyNetwork,
! REQ-HIER-005). Instead, for any endpoint that is not "<self>", this
! module creates (or reuses a cached) parent-local proxy StateItemNode
! standing in for that child's item (REQ-HIER-006), registers it in the
! PARENT's own graph, and records it via add_child_port_binding; the
! actual dependency edge is then always added to the calling component's
! own default network, between two of its own (real-or-proxy) NodeIds.
!
! Two-phase timing (matches the legacy Connection activate()/connect()
! split, not just its matching logic): the legacy imperative coupler
! does NOT form real wiring during GENERIC_INIT_ADVERTISE.
! Connection%activate() there only marks which imports/exports are
! "active", which existing MAPL machinery uses to decide (a) whether an
! unresolved import needs to bubble up to the parent
! (propagate_unsatisfied_imports) and (b) whether an export actually
! needs to be allocated at all (only allocated if some active import
! needs it) - see initialize_advertise.F90's process_connections()
! (calls activate()) versus initialize_accept_transfer.F90's
! process_connections() (calls connect(), the real wiring step).
! GraphBuilder mirrors this with two separate entry points instead of
! one:
!   - graphbuilder_check_unsatisfied_imports(): the activate()-time
!     analog - read-only, determines which ordinary-match imports would
!     be satisfied without creating any graph structure (no proxy nodes,
!     no dependency edges). Called from initialize_advertise.F90.
!   - graphbuilder_resolve_connections(): the connect()-time analog -
!     the real graph mutation (proxies + dependency edges). Called from
!     initialize_accept_transfer.F90, alongside the real connect() call,
!     followed by graphbuilder_freeze() there (freezing before real
!     wiring exists would be premature).
!------------------------------------------------------------------------------
module mapl_GraphBuilder_mod
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_ComponentSpec_mod, only: ComponentSpec
   use mapl_VariableSpec_mod, only: VariableSpec
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator
   use mapl_VariableSpecVector_mod, only: operator(/=)
   use mapl_Connection_mod, only: Connection
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_MatchConnection_mod, only: MatchConnection
   use mapl_ConnectionPt_mod, only: ConnectionPt
   use mapl_VirtualConnectionPt_mod, only: VirtualConnectionPt
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_NodeId_mod, only: NodeId
   use mapl_PortId_mod, only: PortId
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   use gFTL2_StringVector, only: StringVector
   use pflogger, only: Logger
   use esmf, only: ESMF_StateIntent_Flag, ESMF_STATEINTENT_IMPORT, &
        ESMF_STATEINTENT_EXPORT
   use esmf, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphBuilder
   public :: item_key
   public :: proxy_key

   ! Stateless-per-call by design (design.md Decisions): GraphBuilder
   ! carries no state of its own - every bound procedure still takes the
   ! OuterMetaComponent it operates on as an explicit argument, exactly
   ! as the free functions this type replaces did. All bindings are
   ! NOPASS so each procedure's signature is untouched by this
   ! type-bound wrapping; only the calling convention changes, e.g.
   ! `gb = GraphBuilder(); call gb%run_advertise_hook(this)`.
   type :: GraphBuilder
   contains
      procedure, nopass :: advertise => graphbuilder_advertise
      procedure, nopass :: check_unsatisfied_imports => graphbuilder_check_unsatisfied_imports
      procedure, nopass :: resolve_connections => graphbuilder_resolve_connections
      procedure, nopass :: freeze => graphbuilder_freeze
      procedure, nopass :: run_advertise_hook => graphbuilder_run_advertise_hook
      procedure, nopass :: run_activate_hook => graphbuilder_run_activate_hook
      procedure, nopass :: run_connect_hook => graphbuilder_run_connect_hook
   end type GraphBuilder

   ! Matches StateRegistry_Hierarchy_smod's own SELF sentinel
   ! (superstructure/generic/registry/StateRegistry_Hierarchy_smod.F90):
   ! a ConnectionPt component_name of "<self>" always refers to the
   ! component the connection was declared on.
   character(*), parameter :: SELF_COMPONENT_NAME = '<self>'

   ! Per-match callback signature for for_each_matching_import() below -
   ! same shape/precedent as OuterMetaComponent's own I_child_op
   ! (superstructure/generic/OuterMetaComponent.F90), passed an internal
   ! (CONTAINS-nested) procedure by each caller so it can host-associate
   ! whatever extra context (this, src_pt, unresolved, ...) it needs
   ! without threading it through this interface.
   abstract interface
      subroutine I_match_op(var_spec, rc)
         import VariableSpec
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc
      end subroutine I_match_op
   end interface

contains

   ! ============================================================
   ! Task 2: Advertising
   ! ============================================================

   ! REQ-GB-003 "Advertising state items" / "Creating StateItemNodes":
   ! one StateItemNode per advertised import/export/internal item, plus
   ! (task 4.1) import/export items are recorded as public ports - MAPL's
   ! existing coupling semantics already treat every import/export as
   ! externally addressable (any sibling's connect_all can reach it by
   ! name) while internal items are component-private, so that existing
   ! distinction is reused here as "declared a public port" rather than
   ! inventing a new marker on VariableSpec/ComponentSpec.
   subroutine graphbuilder_advertise(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      comp_spec => this%get_component_spec()
      graph => this%get_component_graph()

      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            call advertise_one(graph, var_spec, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_advertise

   subroutine advertise_one(graph, var_spec, rc)
      type(ComponentGraph), intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: key
      type(NodeId), pointer :: existing
      type(NodeId) :: id
      type(StateItemNode) :: node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id

      key = item_key(var_spec%state_intent, var_spec%short_name)

      ! Idempotent re-advertisement (spec scenario "Re-advertising the
      ! same item does not duplicate its node"): if this identity is
      ! already indexed, this item already has a node - nothing to do.
      existing => graph%get_resource_index(key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      id = graph%next_node_id(_RC)

      ! payload/revision are left default-initialized (no ESMF handle
      ! allocated, NodeRevision invalid): this item is advertised, not
      ! yet realized - REALIZE happens in a later init phase this slice
      ! does not touch (design.md Non-Goals).
      node = StateItemNode(id, payload, revision)
      call graph%register_node(node, _RC)
      call graph%add_resource_index(key, id, _RC)

      if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_import_port(port_id, id, _RC)
      else if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_export_port(port_id, id, _RC)
      end if
      ! INTERNAL items get neither: they are not visible outside the
      ! component (task 4.1's "declared a public port" does not apply).

      _RETURN(_SUCCESS)
   end subroutine advertise_one

   ! Advertised-item identity key: state intent + short name, unique
   ! within one component (spec "Advertising creates graph state-item
   ! nodes" - "retrievable afterward by the item's identity"). Also used,
   ! unqualified by component, as the per-child lookup key when this
   ! component's own advertised items are read from a *different*
   ! component's ComponentGraph (see get_or_make_local_node_id below).
   function item_key(state_intent, short_name) result(key)
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      character(:), allocatable :: key

      character(:), allocatable :: intent_str

      if (state_intent == ESMF_STATEINTENT_IMPORT) then
         intent_str = 'IMPORT'
      else if (state_intent == ESMF_STATEINTENT_EXPORT) then
         intent_str = 'EXPORT'
      else
         intent_str = 'INTERNAL'
      end if
      key = intent_str // ':' // short_name
   end function item_key

   ! Resource-index key for a parent-local proxy node standing in for a
   ! named child's item (module header - "Cross-component (parent/child)
   ! connections"). Exposed (like item_key above) so callers with
   ! legitimate reason to identify a cached proxy - tests, or a later
   ! sub-change (3c) needing to find/reuse the same proxy - do not need
   ! to duplicate this module's private key format.
   function proxy_key(comp_name, state_intent, short_name) result(key)
      character(*), intent(in) :: comp_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      character(:), allocatable :: key

      key = 'PROXY:' // comp_name // ':' // item_key(state_intent, short_name)
   end function proxy_key

   ! ============================================================
   ! Task 3: Ordinary connection resolution
   ! ============================================================

   ! GENERIC_INIT_ADVERTISE-time analog of legacy Connection%activate()
   ! (module header - "Two-phase timing"): for each MatchConnection,
   ! determine which ordinary-match destination imports have a matching
   ! source export, WITHOUT creating any graph structure (no proxy
   ! nodes, no dependency edges - those are graphbuilder_resolve_connections'
   ! job, at connect() time). Read-only with respect to the graph;
   ! purely populates the unresolved-imports report, which is what
   ! legacy's own propagate_unsatisfied_imports() decision (bubble an
   ! unsatisfied import to the parent) is based on at this same phase.
   subroutine graphbuilder_check_unsatisfied_imports(this, unusable, unresolved_imports, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(StringVector), optional, intent(out) :: unresolved_imports
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(StringVector) :: unresolved

      comp_spec => this%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call check_match_connection_unsatisfied(this, c, unresolved, _RC)
            class default
               ! Wildcard/callback/reexport/simple connections: out of
               ! scope for this slice - deliberately skipped.
            end select
         end do
      end associate

      if (present(unresolved_imports)) unresolved_imports = unresolved

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_check_unsatisfied_imports

   ! Shared by check_match_connection_unsatisfied() and
   ! resolve_match_connection() below: both used to independently repeat
   ! this same "which of dst_spec's imports does dst_pt's declared
   ! pattern match" do/if/if scan (module header - "same destination-
   ! import filtering"). Owns the loop and both guard clauses; each
   ! caller supplies only the per-match step, as an internal procedure so
   ! it can host-associate whatever extra context it needs (this,
   ! src_pt, unresolved, ...) - same style as OuterMetaComponent's own
   ! apply_to_children_custom()/I_child_op.
   subroutine for_each_matching_import(dst_spec, dst_pt, op, rc)
      type(ComponentSpec), intent(in) :: dst_spec
      type(ConnectionPt), intent(in) :: dst_pt
      procedure(I_match_op) :: op
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      associate (e => dst_spec%var_specs%ftn_end())
         iter = dst_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT)) cycle
            if (.not. dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, var_spec%short_name))) cycle
            call op(var_spec, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine for_each_matching_import

   ! Read-only counterpart of resolve_match_connection() below: same
   ! destination-import filtering and same-name export existence check,
   ! but never calls get_or_make_local_node_id() or add_dependency() -
   ! no proxy node or dependency edge is created here.
   subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(StringVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(ComponentSpec), pointer :: dst_spec, src_spec

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      dst_spec => component_spec_for(this, dst_pt%component_name, _RC)
      src_spec => component_spec_for(this, src_pt%component_name, _RC)

      call for_each_matching_import(dst_spec, dst_pt, check_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine check_one(var_spec, rc)
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc

         logical :: has_export

         has_export = var_specs_has_export(src_spec, var_spec%short_name)
         if (.not. has_export) then
            call unresolved%push_back(dst_pt%component_name // ':' // var_spec%short_name)
         end if

         _RETURN(_SUCCESS)
      end subroutine check_one

   end subroutine check_match_connection_unsatisfied

   ! GENERIC_INIT_ACCEPT_TRANSFER-time analog of legacy
   ! Connection%connect() (module header - "Two-phase timing"): the real
   ! graph mutation - proxy nodes and dependency edges - restricted to
   ! MatchConnection (see module header). Any other declared Connection
   ! subtype (SimpleConnection, ReexportConnection, ...) is left
   ! untouched - out of scope for this slice, not an oversight
   ! (proposal.md "Explicitly out of scope"). REQ-GB-003 "Resolving
   ! connections" / "Creating dependency networks".
   !
   ! Idempotency: ESMF's Provide/Accept/Realize transfer negotiation can
   ! invoke GENERIC_INIT_ACCEPT_TRANSFER (and hence this) more than once
   ! per component within a single Initialize sequence - legacy's own
   ! MatchConnection%connect()/SimpleConnection%connect()
   ! (superstructure/generic/connection/*.F90) guard against exactly this
   ! with a per-connection `consumed` flag. GraphBuilder has no equivalent
   ! per-connection state to reuse, so it guards at the graph level
   ! instead: once this component's graph is frozen, every ordinary
   ! connection it declares has already been resolved, so a repeat call
   ! is a safe no-op rather than an attempt to mutate a frozen graph.
   subroutine graphbuilder_resolve_connections(this, unusable, unresolved_imports, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(StringVector), optional, intent(out) :: unresolved_imports
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(StringVector) :: unresolved

      graph => this%get_component_graph()
      if (graph%is_frozen()) then
         if (present(unresolved_imports)) unresolved_imports = unresolved
         _RETURN(_SUCCESS)
      end if

      comp_spec => this%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call resolve_match_connection(this, c, unresolved, _RC)
            class default
               ! Wildcard/callback/reexport/simple connections: out of
               ! scope for this slice - deliberately skipped.
            end select
         end do
      end associate

      if (present(unresolved_imports)) unresolved_imports = unresolved

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_resolve_connections

   ! Mirrors MatchConnection%activate()'s own two-step structure
   ! (superstructure/generic/connection/MatchConnection.F90) - filter the
   ! destination's imports against the declared destination pattern, then
   ! for each match require an EXACT-name export on the source side -
   ! but recomputes it directly against ComponentSpec%var_specs
   ! (design.md Decisions: "reuses MatchConnection's semantics, not its
   ! implementation") rather than going through StateRegistry's
   ! subregistry/family machinery. VirtualConnectionPt%matches() and the
   ! ConnectionPt/VirtualConnectionPt read-only accessors are ordinary
   ! public query methods on the Connection's own declared data, not part
   ! of that execution machinery, and are used here as-is.
   subroutine resolve_match_connection(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(StringVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(ComponentSpec), pointer :: dst_spec
      type(ComponentGraph), pointer :: this_graph
      type(DependencyNetworkId) :: net_id

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      dst_spec => component_spec_for(this, dst_pt%component_name, _RC)

      this_graph => this%get_component_graph()
      net_id = this_graph%get_default_network_id()

      call for_each_matching_import(dst_spec, dst_pt, resolve_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine resolve_one(var_spec, rc)
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc

         integer :: status
         type(NodeId) :: import_node_id, export_node_id
         logical :: has_export

         import_node_id = get_or_make_local_node_id(this, dst_pt%component_name, &
              ESMF_STATEINTENT_IMPORT, var_spec%short_name, _RC)

         has_export = find_export_node_id(this, src_pt%component_name, var_spec%short_name, &
              export_node_id, _RC)

         if (has_export) then
            call this_graph%add_dependency(net_id, export_node_id, import_node_id, _RC)
         else
            ! REQ scenario "Import with no matching export is left
            ! unresolved" - reported, not silently dropped.
            call unresolved%push_back(dst_pt%component_name // ':' // var_spec%short_name)
         end if

         _RETURN(_SUCCESS)
      end subroutine resolve_one

   end subroutine resolve_match_connection

   logical function find_export_node_id(this, comp_name, short_name, export_node_id, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: src_spec

      src_spec => component_spec_for(this, comp_name, _RC)
      found = var_specs_has_export(src_spec, short_name)
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      export_node_id = get_or_make_local_node_id(this, comp_name, ESMF_STATEINTENT_EXPORT, short_name, _RC)

      _RETURN(_SUCCESS)
   end function find_export_node_id

   logical function var_specs_has_export(comp_spec, short_name) result(has_export)
      type(ComponentSpec), intent(in) :: comp_spec
      character(*), intent(in) :: short_name

      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      has_export = .false.
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT .and. var_spec%short_name == short_name) then
               has_export = .true.
               return
            end if
         end do
      end associate
   end function var_specs_has_export

   ! ============================================================
   ! Task 4: Public ports and child proxies
   ! ============================================================
   ! (Public-port recording for this component's OWN items happens in
   ! advertise_one() above - task 4.1. This section is the child-proxy
   ! half of task 4, task 4.2, needed by connection resolution above.)

   logical function is_self(this, comp_name) result(self_ref)
      class(OuterMetaComponent), intent(in) :: this
      character(*), intent(in) :: comp_name

      self_ref = (comp_name == SELF_COMPONENT_NAME) .or. (comp_name == this%get_name())
   end function is_self

   ! REQ-GB-002 permits GraphBuilder this reach into a named child's own
   ! spec/graph; it is never exposed through OuterMetaComponent's
   ! general public API (REQ-HIER-005) - see
   ! get_child_component_spec()/get_child_component_graph()'s own
   ! interface comment in OuterMetaComponent.F90 for the visibility
   ! rationale.
   function component_spec_for(this, comp_name, rc) result(comp_spec)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      integer, optional, intent(out) :: rc
      type(ComponentSpec), pointer :: comp_spec

      integer :: status

      if (is_self(this, comp_name)) then
         comp_spec => this%get_component_spec()
      else
         comp_spec => this%get_child_component_spec(comp_name, _RC)
      end if

      _RETURN(_SUCCESS)
   end function component_spec_for

   ! Resolves a (component, intent, short_name) reference to a NodeId in
   ! THIS component's own graph:
   !  - "<self>"/this's own name: the item's own NodeId (must already
   !    have been advertised - graphbuilder_advertise() runs first).
   !  - a named child: a cached-or-newly-created parent-local proxy
   !    StateItemNode (REQ-HIER-006), reusing one already created for the
   !    same child+item via the same resource-index mechanism used for
   !    advertised-item identity (module header - "Cross-component
   !    (parent/child) connections").
   function get_or_make_local_node_id(this, comp_name, state_intent, short_name, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      character(:), allocatable :: item_id_key
      character(:), allocatable :: pkey
      type(NodeId), pointer :: existing
      type(NodeId), pointer :: child_item_id
      type(ComponentGraph), pointer :: child_graph
      type(StateItemNode) :: proxy_node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id

      this_graph => this%get_component_graph()
      item_id_key = item_key(state_intent, short_name)

      if (is_self(this, comp_name)) then
         existing => this_graph%get_resource_index(item_id_key)
         _ASSERT(associated(existing), &
              'GraphBuilder: connection references an item this component never advertised: ' // short_name)
         node_id = existing
         _RETURN(_SUCCESS)
      end if

      pkey = proxy_key(comp_name, state_intent, short_name)
      existing => this_graph%get_resource_index(pkey)
      if (associated(existing)) then
         node_id = existing
         _RETURN(_SUCCESS)
      end if

      child_graph => this%get_child_component_graph(comp_name, _RC)
      child_item_id => child_graph%get_resource_index(item_id_key)
      _ASSERT(associated(child_item_id), &
           'GraphBuilder: connection references an item child "' // comp_name // '" never advertised: ' // short_name)

      node_id = this_graph%next_node_id(_RC)
      proxy_node = StateItemNode(node_id, payload, revision)
      call this_graph%register_node(proxy_node, _RC)
      call this_graph%add_resource_index(pkey, node_id, _RC)

      port_id = this_graph%next_port_id(_RC)
      call this_graph%add_child_port_binding(port_id, node_id, _RC)

      _RETURN(_SUCCESS)
   end function get_or_make_local_node_id

   ! ============================================================
   ! Task 5: Validate and freeze
   ! ============================================================

   ! ComponentGraph%freeze() already validates before transitioning
   ! lifecycle state (ComponentGraph.F90 graph_freeze: validate() is
   ! called first, and the frozen flag is only set on success) - this is
   ! a thin, directly-testable wrapper, not a reimplementation.
   subroutine graphbuilder_freeze(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph

      graph => this%get_component_graph()
      call graph%freeze(_RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_freeze

   ! ============================================================
   ! Lifecycle-hook wrappers (task 2.3 / 3.4)
   ! ============================================================
   !
   ! design.md Decisions ("Invocation point"): GraphBuilder calls are
   ! ADDITIONAL to the existing legacy advertise/connect calls, never a
   ! replacement, and MUST NOT alter initialize_advertise.F90's or
   ! initialize_accept_transfer.F90's own error propagation. The
   ! procedures above use the normal MAPL rc/status-check convention (so
   ! they remain directly unit-testable, task 6, with ordinary pass/fail
   ! semantics); these three wrappers are what actually get called from
   ! those lifecycle hooks - they catch any GraphBuilder-internal failure
   ! locally and report it through the component's own logger rather than
   ! propagating it, so a GraphBuilder defect can never break existing
   ! MAPL component initialization (design.md Risks/Trade-offs).
   !
   ! These three are reached from their call sites only via the public
   ! GraphBuilder type's bindings of the same name minus the
   ! graphbuilder_ prefix (graphbuilder-code-quality-cleanup change),
   ! e.g. `type(GraphBuilder) :: gb; call gb%run_advertise_hook(this)`.
   !
   ! Called from initialize_advertise.F90 (GENERIC_INIT_ADVERTISE, mirrors
   ! self_advertise() + activate()):
   !   graphbuilder_run_advertise_hook -> gb%run_advertise_hook - creates
   !     StateItemNodes
   !   graphbuilder_run_activate_hook -> gb%run_activate_hook -
   !     unresolved-imports report only
   ! Called from initialize_accept_transfer.F90 (GENERIC_INIT_ACCEPT_TRANSFER,
   ! mirrors connect()):
   !   graphbuilder_run_connect_hook -> gb%run_connect_hook - real
   !     proxies/edges, then freeze

   subroutine graphbuilder_run_advertise_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status

      call graphbuilder_advertise(this, status)
      call report_if_failed(this, 'advertise', status)
   end subroutine graphbuilder_run_advertise_hook

   subroutine graphbuilder_run_activate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(StringVector) :: unresolved
      integer :: i
      character(:), pointer :: unresolved_item
      class(Logger), pointer :: lgr

      call graphbuilder_check_unsatisfied_imports(this, unresolved_imports=unresolved, rc=status)
      call report_if_failed(this, 'check_unsatisfied_imports', status)

      lgr => this%get_logger()
      do i = 1, unresolved%size()
         unresolved_item => unresolved%of(i)
         call lgr%warning( &
              'GraphBuilder: no ordinary-match export found for unresolved import %a', unresolved_item)
      end do
   end subroutine graphbuilder_run_activate_hook

   subroutine graphbuilder_run_connect_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status

      call graphbuilder_resolve_connections(this, rc=status)
      call report_if_failed(this, 'resolve_connections', status)

      call graphbuilder_freeze(this, status)
      call report_if_failed(this, 'freeze', status)
   end subroutine graphbuilder_run_connect_hook

   subroutine report_if_failed(this, step_name, status)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: step_name
      integer, intent(in) :: status

      class(Logger), pointer :: lgr

      if (status /= 0) then
         lgr => this%get_logger()
         call lgr%warning( &
              'GraphBuilder: %a step failed with rc=%i0 - continuing without graph representation for this step', &
              step_name, status)
      end if
   end subroutine report_if_failed

end module mapl_GraphBuilder_mod
