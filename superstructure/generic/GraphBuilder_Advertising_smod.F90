#include "MAPL.h"

! Task 2: Advertising
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_Advertising_smod
   use esmf, only: operator(==), operator(/=)
   use mapl_CompositeStateMaterialization_mod, only: materialize_composite
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
        TYPE_KIND_CHARACTERISTIC_KIND, VERTICAL_COORDINATE_CHARACTERISTIC_KIND, &
        ATTRIBUTES_CHARACTERISTIC_KIND, UNGRIDDED_DIMS_CHARACTERISTIC_KIND, &
        STANDARD_NAME_CHARACTERISTIC_KIND, QUANTITY_TYPE_CHARACTERISTIC_KIND, &
        CONSERVATION_CHARACTERISTIC_KIND, NORMALIZATION_CHARACTERISTIC_KIND
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_VERTICALGRID, operator(==)
   use mapl_typekind_mod, only: MAPL_TYPEKIND_MIRROR
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic
   use mapl_VerticalCoordinateCharacteristic_mod, only: VerticalCoordinateCharacteristic
   ! openspec/changes/phase-12-characteristic-dispatch-wiring: the six
   ! axes 5a2 (state-item-characteristic-subclasses) landed subclasses
   ! for but never wired into a real GraphStateItem's characteristics
   ! map - see that change's proposal.md for the roadmap correction
   ! (vertical-coordinate, above, was already wired; only these six were
   ! not).
   use mapl_AttributesCharacteristic_mod, only: AttributesCharacteristic
   use mapl_UngriddedDimsCharacteristic_mod, only: UngriddedDimsCharacteristic
   use mapl_StandardNameCharacteristic_mod, only: StandardNameCharacteristic
   use mapl_QuantityTypeCharacteristic_mod, only: QuantityTypeCharacteristic
   use mapl_ConservationCharacteristic_mod, only: ConservationCharacteristic
   use mapl_NormalizationCharacteristic_mod, only: NormalizationCharacteristic
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! REQ-GB-003 "Advertising state items" / "Creating StateItemNodes":
   ! one StateItemNode per advertised import/export/internal item, plus
   ! (task 4.1) import/export items are recorded as public ports - MAPL's
   ! existing coupling semantics already treat every import/export as
   ! externally addressable (any sibling's connect_all can reach it by
   ! name) while internal items are component-private, so that existing
   ! distinction is reused here as "declared a public port" rather than
   ! inventing a new marker on VariableSpec/ComponentSpec.
   module subroutine graphbuilder_advertise(this, rc)
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

   module subroutine advertise_one(graph, var_spec, rc)
      type(ComponentGraph), target, intent(inout) :: graph
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
      type(StringVector) :: member_names
      class(GraphNode), pointer :: registered_node

      key = item_key(var_spec%state_intent, var_spec%short_name)

      ! Idempotent re-advertisement (spec scenario "Re-advertising the
      ! same item does not duplicate its node"): if this identity is
      ! already indexed, this item already has a node - nothing to do.
      existing => graph%get_resource_index(key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      ! openspec/changes/composite-state-spec: a VariableSpec with
      ! declared members is a composite - build its real, individually
      ! addressable node tree (mapl_CompositeStateMaterialization_mod)
      ! instead of the flat, unallocated-payload node below. No other
      ! branch of this procedure changes: the same identity key,
      ! resource-index registration, and port registration apply
      ! uniformly to both cases (design.md Decisions).
      member_names = var_spec%get_member_names()
      if (member_names%size() > 0) then
         id = materialize_composite(graph, var_spec, _RC)
      else
         id = graph%next_node_id(_RC)

         ! payload/revision are left default-initialized (no ESMF handle
         ! allocated, NodeRevision invalid): this item is advertised, not
         ! yet realized - REALIZE happens in a later init phase this
         ! slice does not touch (design.md Non-Goals).
         !
         ! openspec/changes/graph-builder-stateitem-resolution, tasks.md
         ! 2.1: identity/resolution-mode fields (short_name/state_intent/
         ! is_inout_borrower/callback_interface_id) are set once here,
         ! from the advertised var_spec, so every later resolution step
         ! reads them off the node instead of re-deriving them from this
         ! var_spec by component name.
         node = StateItemNode(id, payload, revision, short_name=var_spec%short_name, &
              state_intent=var_spec%state_intent, is_inout_borrower=var_spec%is_inout_borrower, &
              callback_interface_id=var_spec%callback_interface_id)
         call graph%register_node(node, _RC)
      end if
      call graph%add_resource_index(key, id, _RC)

      ! openspec/changes/graph-builder-stateitem-resolution, tasks.md
      ! 2.2/2.3: populates the newly-registered node's own
      ! GraphStateItem%characteristics/ungridded_dims/vertical_stagger
      ! from this var_spec's own declared fields - the same
      ! field-to-characteristic mapping the now-removed
      ! build_characteristics used, moved here to run once at advertise
      ! time instead of being rebuilt on every resolution call. Applies
      ! uniformly to both the flat and composite branches above (a
      ! composite tagged MAPL_STATEITEM_VERTICALGRID needs its own
      ! VERTICAL_COORDINATE_CHARACTERISTIC_KIND entry just as much as a
      ! flat item does - materialize_composite itself has no var_spec-
      ! independent way to populate this, so it is done here, on the
      ! node materialize_composite already registered, by id). Guarded
      ! by the idempotent re-advertisement check above - never run for
      ! an already-existing identity.
      !
      ! design.md correction: also sets identity/resolution-mode fields
      ! (short_name/state_intent/is_inout_borrower/callback_interface_id,
      ! task 2.1) on the composite branch's own top-level node, via the
      ! setters StateItemNode.F90 exposes for exactly this reason -
      ! materialize_composite constructs that node with no identity args
      ! at all (its own signature has no var_spec-identity parameters),
      ! so a composite IMPORT/EXPORT would otherwise be invisible to
      ! for_each_matching_import's node-based filter. A no-op re-set for
      ! the flat branch (already set via the constructor above) -
      ! harmless, not worth branching around.
      registered_node => graph%get_node(id)
      _ASSERT(associated(registered_node), 'GraphBuilder: advertise_one - newly-registered node not found')
      select type (registered_node)
      class is (StateItemNode)
         call registered_node%set_short_name(var_spec%short_name)
         call registered_node%set_state_intent(var_spec%state_intent)
         call registered_node%set_is_inout_borrower(var_spec%is_inout_borrower)
         call registered_node%set_callback_interface_id(var_spec%callback_interface_id)
         call populate_characteristics(registered_node, var_spec, _RC)
      class default
         _FAIL('GraphBuilder: advertise_one - newly-registered node is not a StateItemNode')
      end select

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

   ! openspec/changes/graph-builder-stateitem-resolution, tasks.md 2.2/
   ! 2.3: field-to-characteristic mapping moved verbatim from the
   ! now-removed build_characteristics (GraphBuilder_ConnectionResolution_
   ! smod.F90), writing into node's own GraphStateItem%characteristics
   ! via set_characteristic instead of building an ephemeral,
   ! never-stored CharacteristicMap on every resolution call. Also
   ! populates the new ungridded_dims/vertical_stagger fields (task 1.2)
   ! from the same var_spec, alongside characteristics.
   subroutine populate_characteristics(node, var_spec, rc)
      class(StateItemNode), target, intent(inout) :: node
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: payload
      character(20) :: grid_id_buffer

      ! openspec/changes/graph-builder-stateitem-resolution: mutates
      ! node's own stored payload IN PLACE via a pointer
      ! (get_payload_ptr, StateItemNode.F90), never through a
      ! get_payload()/set_payload() copy-out/copy-back round trip - a
      ! composite node's own GraphStateItem carries a populated
      ! StateItemMemberMap, and an intrinsic assignment of that type
      ! (what get_payload()'s own `payload = this%payload` does)
      ! invokes gFTL2's own deep_copy, which a real compiler bug
      ! (observed under gfortran -fcheck=all, not NAG - a known-hard
      ! class of Fortran compiler bug around nested defined-assignment,
      ! not a logic error in gFTL2's own design) can corrupt
      ! nondeterministically for a non-trivial (populated) member map.
      payload => node%get_payload_ptr()

      if (allocated(var_spec%units)) then
         call payload%set_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
              PhysicalUnitsCharacteristic(var_spec%units), _RC)
      end if
      ! openspec/changes/phase-10c-typekind-characteristic, design.md D3:
      ! unlike units, VariableSpec%typekind is a non-allocatable field
      ! with a default (ESMF_TYPEKIND_R4) - every VariableSpec always
      ! has SOME typekind, so there is no "not declared" state to gate
      ! on the way `allocated(var_spec%units)` gates the branch above.
      !
      ! openspec/changes/graph-builder-stateitem-resolution, design.md
      ! correction (surfaced during implementation):
      ! MAPL_TYPEKIND_MIRROR (infrastructure/esmf/TypeKind.F90) is the
      ! one real exception - a declared "inherit whatever the matched
      ! export turns out to be" placeholder (e.g.
      ! StatisticsGridComp.F90's own mirrored imports), not a real
      ! ESMF_TypeKind_Flag TypeKindCharacteristic's label mapping
      ! understands (nor should it try to - the whole point of MIRROR
      ! is "not yet known"). The now-removed build_characteristics
      ! never actually hit this case in practice (it ran on-demand
      ! inside resolve_one, by which point MAPL's own mirroring
      ! machinery - TypekindAspect, a different, independent mechanism
      ! - had already resolved the real kind elsewhere); moving this
      ! call to advertise time means it now runs before any such
      ! resolution could have happened, so the gate is added here,
      ! mirroring allocated(var_spec%units)'s own "not established"
      ! treatment immediately above - a mirrored item simply gets no
      ! TYPE_KIND_CHARACTERISTIC_KIND entry at advertise time.
      if (var_spec%typekind /= MAPL_TYPEKIND_MIRROR) then
         call payload%set_characteristic(TYPE_KIND_CHARACTERISTIC_KIND, &
              TypeKindCharacteristic(var_spec%typekind), _RC)
      end if
      if (allocated(var_spec%vertical_grid)) then
         ! VerticalGrid%get_id() returns an integer identity token;
         ! VerticalCoordinateCharacteristic compares an opaque string
         ! signature (get_signature contract), so it is rendered to text
         ! here rather than changing VerticalCoordinateCharacteristic's
         ! own constructor to know about integer ids specifically.
         write(grid_id_buffer, '(I0)') var_spec%vertical_grid%get_id()
         call payload%set_characteristic(VERTICAL_COORDINATE_CHARACTERISTIC_KIND, &
              VerticalCoordinateCharacteristic(grid_id=trim(grid_id_buffer), &
                   dimensions=var_spec%vertical_grid%get_supported_physical_dimensions()), _RC)
      end if
      ! openspec/changes/vertical-grid-graph-state-item: a composite
      ! VariableSpec tagged state_item_variant == MAPL_STATEITEM_VERTICALGRID
      ! (REQ-GEO-009) has no legacy VerticalGrid object to pull an
      ! identity token from - only its own declared member names
      ! (REQ-GEO-004a: member name = physical dimension, so
      ! get_member_names() already IS the dimension set). Uses the SAME
      ! VerticalCoordinateCharacteristic/VERTICAL_COORDINATE_CHARACTERISTIC_KIND
      ! as the branch above (not a separate characteristic kind) - with
      ! no identity token supplied, needs_extension_for falls back to
      ! dimension-set comparison (VerticalCoordinateCharacteristic.F90's
      ! own module header).
      if (allocated(var_spec%state_item_variant)) then
         if (var_spec%state_item_variant == MAPL_STATEITEM_VERTICALGRID) then
            call payload%set_characteristic(VERTICAL_COORDINATE_CHARACTERISTIC_KIND, &
                 VerticalCoordinateCharacteristic(dimensions=var_spec%get_member_names()), _RC)
         end if
      end if

      ! openspec/changes/phase-12-characteristic-dispatch-wiring,
      ! design.md D1: six more axes, each gated to match its own
      ! VariableSpec field's actual "unset" representation rather than
      ! one uniform rule.
      !
      ! StandardNameCharacteristic: allocatable, no default - same
      ! `allocated()` gate as units above.
      if (allocated(var_spec%standard_name)) then
         call payload%set_characteristic(STANDARD_NAME_CHARACTERISTIC_KIND, &
              StandardNameCharacteristic(var_spec%standard_name), _RC)
      end if
      ! AttributesCharacteristic: non-allocatable StringVector, default
      ! empty - inserted only when non-empty, to avoid a characteristic
      ! map entry for the overwhelmingly common "no declared shared
      ! attributes" case (design.md D1 - the empty case is vacuously
      ! non-mismatching either way, so this gate is an optimization, not
      ! a correctness requirement).
      if (var_spec%attributes%size() > 0) then
         call payload%set_characteristic(ATTRIBUTES_CHARACTERISTIC_KIND, &
              AttributesCharacteristic(var_spec%attributes), _RC)
      end if
      ! UngriddedDimsCharacteristic: non-allocatable, default "no
      ! ungridded dims" - unconditional insert, mirroring typekind's own
      ! "the default is itself a meaningful, comparable value" precedent
      ! above.
      call payload%set_characteristic(UNGRIDDED_DIMS_CHARACTERISTIC_KIND, &
           UngriddedDimsCharacteristic(var_spec%ungridded_dims), _RC)
      ! QuantityTypeCharacteristic/ConservationCharacteristic/
      ! NormalizationCharacteristic: the three new VariableSpec fields
      ! added by this change (proposal.md - Why) - unconditional insert,
      ! same typekind precedent; each field's own default is itself a
      ! valid, already-tested "matches anything"/structurally-compared
      ! value per that characteristic's own needs_extension_for.
      call payload%set_characteristic(QUANTITY_TYPE_CHARACTERISTIC_KIND, &
           QuantityTypeCharacteristic(quantity_type=var_spec%quantity_type), _RC)
      call payload%set_characteristic(CONSERVATION_CHARACTERISTIC_KIND, &
           ConservationCharacteristic(var_spec%conservation), _RC)
      call payload%set_characteristic(NORMALIZATION_CHARACTERISTIC_KIND, &
           NormalizationCharacteristic(var_spec%normalization), _RC)

      ! tasks.md 2.3: ungridded_dims/vertical_stagger, alongside
      ! characteristics.
      call payload%set_ungridded_dims(var_spec%ungridded_dims)
      if (allocated(var_spec%vertical_stagger)) then
         call payload%set_vertical_stagger(var_spec%vertical_stagger)
      end if

      ! design.md correction (surfaced during implementation): the
      ! originating var_spec's own per-item EXPLICIT geom/vertical_grid
      ! overrides and declared item classification, so
      ! try_materialize_field_extension/materialize_extension_chain and
      ! resolve_export_geom/resolve_export_vgrid need no VariableSpec
      ! reference at all once this item is advertised. The owning
      ! component's own component-wide geom/vgrid DEFAULTS are
      ! deliberately NOT captured here - those remain read live via
      ! OuterMetaComponent%has_geom()/get_geom()/get_vertical_grid() in
      ! GraphBuilder_PortsAndProxies_smod.F90 (GEOM_B genuinely runs
      ! after ADVERTISE, so there is no "once at advertise time" value
      ! to capture for that fallback case).
      if (allocated(var_spec%geom)) call payload%set_declared_geom(var_spec%geom)
      if (allocated(var_spec%vertical_grid)) call payload%set_declared_vertical_grid(var_spec%vertical_grid)
      call payload%set_declared_item_type(var_spec%itemType)

      ! openspec/changes/phase-13-restart-compatibility (docs/graph/spec/
      ! 20-implementation-roadmap.md sec 20.4.13): capture the
      ! originating VariableSpec's own declared restart_mode, same
      ! "materialization input, set once at advertise time" pattern as
      ! declared_geom/declared_vertical_grid above - populate_one
      ! (GraphBuilder_StatePopulation_smod.F90) reads this back when it
      ! creates the real MAPL_NamedAlias.
      if (allocated(var_spec%restart_mode)) call payload%set_declared_restart_mode(var_spec%restart_mode)

      _RETURN(_SUCCESS)
   end subroutine populate_characteristics

end submodule GraphBuilder_Advertising_smod
