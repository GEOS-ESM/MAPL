#include "MAPL.h"

module mapl_FieldClassAspect_mod

   use mapl_ActualConnectionPt_mod
   use mapl_enums_api
   use mapl_AspectId_mod
   use mapl_StateItemAspect_mod
   use mapl_ClassAspect_mod
   use mapl_GeomAspect_mod
   use mapl_HorizontalDimsSpec_mod
   use mapl_VerticalGridAspect_mod
   use mapl_UnitsAspect_mod
   use mapl_TypekindAspect_mod
   use mapl_UngriddedDimsAspect_mod

   use mapl_VerticalGrid_mod
   use mapl_VerticalStaggerLoc_mod
   use mapl_VerticalStaggerLoc_mod
   use mapl_UngriddedDims_mod

   use mapl_NullTransform_mod
   use mapl_ExtensionTransform_mod
   use mapl_MultiState_mod
   use mapl_ESMF_Utilities_mod, only: get_substate

   use mapl_field_api
   use mapl_field_bundle_api
   use mapl_FieldInfo_mod, only: FieldInfoSetInternal
   use mapl_RestartModes_mod, only: RestartMode

   use mapl_FieldUtilities_mod
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use esmf
   use pflogger

   implicit none(type,external)
   private

   public :: FieldClassAspect
   public :: to_FieldClassAspect

   interface to_FieldClassAspect
      procedure :: to_fieldclassaspect_from_poly
      procedure :: to_fieldclassaspect_from_map
   end interface to_FieldClassAspect

   type, extends(ClassAspect) :: FieldClassAspect
      private
      logical :: is_created = .false.
      type(ESMF_Field) :: payload
      character(:), allocatable :: long_name
      real(kind=ESMF_KIND_R4), allocatable :: fill_value
      type(RestartMode), allocatable :: restart_mode
   contains
      procedure :: get_aspect_order
      procedure :: get_mandatory_aspect_ids
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches => matches_a
      procedure :: connect_to_import
      procedure :: connect_to_export
      procedure :: inherit_descriptive_metadata
      procedure :: get_long_name

      procedure :: create
      procedure :: update_payload
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: get_payload
      procedure, nopass :: get_aspect_id
   end type FieldClassAspect

   interface
      module function matches_a(src, dst) result(matches)
         logical matches
         class(FieldClassAspect), intent(in) :: src
         class(StateItemAspect), intent(in) :: dst
      end function matches_a
   end interface

   interface FieldClassAspect
      procedure :: new_FieldClassAspect
   end interface FieldClassAspect


contains

   function new_FieldClassAspect( &
        long_name, &
        fill_value, &
        restart_mode) result(aspect)
      type(FieldClassAspect) :: aspect
      character(*), optional, intent(in) :: long_name
      real(kind=ESMF_KIND_R4), optional, intent(in) :: fill_value
      type(RestartMode), optional, intent(in) :: restart_mode

      ! NOTE: long_name is intentionally left unallocated when not supplied
      ! (rather than defaulted to a literal 'unknown') so that allocated(...)
      ! can be used downstream (connect_to_export, add_to_state) to
      ! distinguish "not assigned here" from "explicitly assigned".
      ! standard_name is no longer a FieldClassAspect member - it is handled
      ! entirely by the sibling StandardNameAspect (STANDARD_NAME_ASPECT_ID);
      ! see generic/standard-name-enforcement.
      if (present(long_name)) then
         aspect%long_name = long_name
      end if

      if (present(fill_value)) then
         aspect%fill_value = fill_value
      end if

      if (present(restart_mode)) then
         aspect%restart_mode = restart_mode
      end if
   end function new_FieldClassAspect

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(FieldClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [ &
           CLASS_ASPECT_ID, &
           ATTRIBUTES_ASPECT_ID, &
           UNGRIDDED_DIMS_ASPECT_ID, &
           QUANTITY_TYPE_ASPECT_ID, &
           CONSERVATION_ASPECT_ID, &
           GEOM_ASPECT_ID, &
           VERTICAL_GRID_ASPECT_ID, &
           NORMALIZATION_ASPECT_ID, &
           UNITS_ASPECT_ID, &
           STANDARD_NAME_ASPECT_ID, &
           TYPEKIND_ASPECT_ID &
           ]

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order

   function get_mandatory_aspect_ids(this) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(FieldClassAspect), intent(in) :: this

      aspect_ids = [ &
           ATTRIBUTES_ASPECT_ID, &
           UNGRIDDED_DIMS_ASPECT_ID, &
           QUANTITY_TYPE_ASPECT_ID, &
           CONSERVATION_ASPECT_ID, &
           GEOM_ASPECT_ID, &
           VERTICAL_GRID_ASPECT_ID, &
           NORMALIZATION_ASPECT_ID, &
           UNITS_ASPECT_ID, &
           STANDARD_NAME_ASPECT_ID, &
           TYPEKIND_ASPECT_ID &
           ]

   end function get_mandatory_aspect_ids

   subroutine create(this, other_aspects, rc)
      class(FieldClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      call mapl_FieldSet(this%payload, allocation_status=MAPL_STATEITEM_ALLOCATION_CREATED, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(other_aspects)
   end subroutine create

   ! Called (via update_payload_from_aspects) alongside every other
   ! characteristic aspect's own update_payload (units, typekind, geom, ...)
   ! - i.e. the same point in the lifecycle (StateItemSpec%create) where the
   ! rest of a field's descriptive metadata is attached to the payload.
   !
   ! MAPL_FieldSet resolves long_name via ESMF_NamedAliasGet on `field`,
   ! scoping the write to whatever alias id that specific handle resolves to.
   ! `field` here is `this%payload` itself (see get_payload), which has never
   ! been wrapped in a NamedAlias at this point, so it resolves to id=0.
   ! This does NOT collapse per-connection-endpoint metadata: each state
   ! placement made later via add_to_state (Import, re-export, ...) is
   ! wrapped in its own NamedAlias and stores its own value under that
   ! alias's own nonzero id, which always takes precedence over id=0 for
   ! that placement.  id=0 only matters as the fallback resolved by a field
   ! handle that is never wrapped in a NamedAlias at all - e.g. a field
   ! pulled out of a service bundle via ServiceClassAspect%add_to_bundle, or
   ! a hand-built field in a unit test - which would otherwise default all
   ! the way down to 'unknown'.
   ! (Bracket/Vector/VectorBracketClassAspect route their per-component
   ! FieldClassAspect through a *local* update_payload helper that only
   ! forwards to sibling aspects (units, typekind, ...) and never calls the
   ! component's own update_payload - so this fix does not yet reach them;
   ! see follow-up.)
   ! standard_name is no longer written here at all - it is a single
   ! unaliased/field-wide value owned entirely by StandardNameAspect's own
   ! update_payload (see generic/standard-name-enforcement), which is one of
   ! the "sibling characteristic aspects" this comment refers to.
   subroutine update_payload(this, field, bundle, state, rc)
      class(FieldClassAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      if (present(field)) then
         call MAPL_FieldSet(field, long_name=this%long_name, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(bundle)
      _UNUSED_DUMMY(state)
   end subroutine update_payload

   subroutine activate(this, rc)
      class(FieldClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      call FieldInfoSetInternal(info, allocation_status=MAPL_STATEITEM_ALLOCATION_ACTIVE, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine activate

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(FieldClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      call mapl_FieldEmptyComplete(this%payload, _RC)

      if (allocated(this%fill_value)) then
         call FieldSet(this%payload, this%fill_value, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(other_aspects)
   end subroutine allocate

   subroutine destroy(this, rc)
      class(FieldClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, nogarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   subroutine connect_to_import(this, import, rc)
      class(FieldClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      type(FieldClassAspect) :: import_
      integer :: status

      _RETURN_IF(allocated(this%fill_value))

      import_ = to_FieldClassAspect(import, _RC)
      if (allocated(import_%fill_value)) then ! import wins (for now)
         this%fill_value = import_%fill_value
      end if

      _RETURN(_SUCCESS)
   end subroutine connect_to_import

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(FieldClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(FieldClassAspect) :: export_
      type(ESMF_Info) :: info
      integer :: status

      export_ = to_FieldClassAspect(export, _RC)
      call this%destroy(_RC) ! import is replaced by export/extension
      this%payload = export_%payload

      ! long_name: keep this (import) side's own explicitly declared value if
      ! it has one; otherwise inherit from the predecessor (export_).
      ! One-directional: unlike fill_value, this must NOT converge
      ! bidirectionally - the export's own declared name must never be
      ! overwritten by a downstream import's declaration (see connect_to_import,
      ! which is intentionally left untouched).
      ! standard_name is no longer handled here at all - it is a single
      ! unaliased/field-wide value owned by StandardNameAspect, connected via
      ! its own connect_to_export (see generic/standard-name-enforcement).
      call mirror_name(this%long_name, export_%long_name)

      call mirror(this%fill_value, export_%fill_value)

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
!#      call FieldInfoSetInternal(info, allocation_status=MAPL_STATEITEM_ALLOCATION_ALLOCATED, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   contains

      subroutine mirror(dst, src)
         real, allocatable, intent(inout) :: dst
         real, allocatable, intent(in) :: src

         character(100) :: buffer
         class(Logger), pointer :: lgr

         if (.not. allocated(src)) return

         if (.not. allocated(dst)) then
            dst = src
            return
         end if

         ! TODO: Problematic case: both allocated with different values.
         if (dst /= src) then
            lgr => logging%get_logger('mapl.generic')
            write(buffer,*) actual_pt
            call lgr%info('Mismatched default values for %a src = %g0~; dst = %g0 (src value wins)', trim(buffer), src, dst)
         end if

      end subroutine mirror

   end subroutine connect_to_export

   ! Character-string counterpart of the internal mirror() above (fill_value), but
   ! with different precedence semantics deliberately: dst (this aspect's own
   ! declared value) always wins if assigned; src (the predecessor) is only
   ! adopted when dst was left unassigned.  No mismatch logging - differing
   ! long_name on each side of a connection is the expected, common case, not
   ! an authoring error.  Shared by connect_to_export (aliasing an existing
   ! field) and inherit_descriptive_metadata (a fresh FieldClassAspect
   ! superseding a non-Field predecessor, e.g. an ExpressionClassAspect).
   subroutine mirror_name(dst, src)
      character(:), allocatable, intent(inout) :: dst
      character(:), allocatable, intent(in) :: src

      if (allocated(dst)) return
      if (allocated(src)) dst = src

   end subroutine mirror_name

   subroutine inherit_descriptive_metadata(this, predecessor, rc)
      class(FieldClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: predecessor
      integer, optional, intent(out) :: rc

      character(:), allocatable :: predecessor_long_name

      call predecessor%get_long_name(predecessor_long_name)

      call mirror_name(this%long_name, predecessor_long_name)

      _RETURN(_SUCCESS)
   end subroutine inherit_descriptive_metadata

   subroutine get_long_name(this, long_name)
      class(FieldClassAspect), intent(in) :: this
      character(:), allocatable, intent(out) :: long_name

      if (allocated(this%long_name)) long_name = this%long_name
   end subroutine get_long_name

   function to_fieldclassaspect_from_poly(aspect, rc) result(field_aspect)
      type(FieldClassAspect) :: field_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (FieldClassAspect)
         field_aspect = aspect
      class default
         _FAIL('aspect is not FieldClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_fieldclassaspect_from_poly

   function to_fieldclassaspect_from_map(map, rc) result(field_aspect)
      type(FieldClassAspect) :: field_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      field_aspect = to_FieldClassAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_fieldclassaspect_from_map

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      transform = NullTransform()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
      _UNUSED_DUMMY(other_aspects)
   end function make_transform

   logical function supports_conversion_general(src)
      class(FieldClassAspect), intent(in) :: src

      supports_conversion_general = .false.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(FieldClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias, existing_field
      type(esmf_StateItem_Flag) :: itemType
      type(ESMF_State) :: state, substate
      type(ESMF_Info) :: info
      logical :: is_alias
      character(:), allocatable :: full_name, inner_name, intent
      integer :: idx, alias_id, status

      intent = actual_pt%get_state_intent()
      call multi_state%get_state(state, intent, _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      ! MAPL_NamedAlias (not a bare ESMF_NamedAlias) so this new placement's
      ! own alias id starts out with whatever long_name/restart_mode
      ! `this%payload` currently resolves to; the explicit writes below then
      ! authoritatively override with this aspect's own (possibly
      ! connection-inherited) values. (standard_name is unaliased/field-wide
      ! - see StandardNameAspect's own add_to_state-independent
      ! update_payload - so it is unaffected by which alias this is.)
      alias = MAPL_NamedAlias(this%payload, name=inner_name, _RC)

      call ESMF_StateGet(substate, itemName=inner_name, itemType=itemType, _RC)
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
         if (intent /= 'import') then
            call ESMF_StateGet(substate, itemName=inner_name, field=existing_field, _RC)
            is_alias = mapl_FieldsAreAliased(alias, existing_field, _RC)
            _ASSERT(is_alias, 'Different fields added under the same name in state.')
         end if
      end if
      call ESMF_StateAddReplace(substate, [alias], _RC)

      if (allocated(this%restart_mode) .or. allocated(this%long_name)) then
         call ESMF_NamedAliasGet(alias, id=alias_id, _RC)
         call ESMF_InfoGetFromHost(alias, info, _RC)

         if (allocated(this%restart_mode)) then
            call FieldInfoSetInternal(info, alias_id, this%restart_mode, _RC)
         end if

         ! long_name is per-connection-endpoint metadata: this placement
         ! (this specific alias - the Export's own, a connected Import's
         ! own, or an intermediate transform hop's own) records its own
         ! value, independent of every other placement of the same
         ! underlying field.  See connect_to_export for how an endpoint that
         ! declares none inherits from its connection predecessor.
         ! (standard_name is unaliased/field-wide, written by
         ! StandardNameAspect's own update_payload, not here.)
         if (allocated(this%long_name)) then
            call FieldInfoSetInternal(info, named_alias_id=alias_id, &
                 long_name=this%long_name, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, field_bundle, rc)
      class(FieldClassAspect), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleAdd(field_bundle, [this%payload], multiflag=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   subroutine get_payload(this, unusable, field, bundle, state, rc)
      class(FieldClassAspect), intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(esmf_Field), optional, allocatable, intent(out) :: field
      type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
      type(esmf_State), optional, allocatable, intent(out) :: state
      integer, optional, intent(out) :: rc

      field = this%payload

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(bundle)
      _UNUSED_DUMMY(state)
   end subroutine get_payload


   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

end module mapl_FieldClassAspect_mod
