#include "MAPL.h"

module mapl_ExpressionClassAspect_mod

   use mapl_AspectId_mod
   use mapl_StateItemAspect_mod
   use mapl_ClassAspect_mod
   use mapl_FieldClassAspect_mod
   use mapl_GeomAspect_mod
   use mapl_HorizontalDimsSpec_mod
   use mapl_VerticalGridAspect_mod
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_VerticalStaggerLoc_mod
   use mapl_UnitsAspect_mod
   use mapl_TypekindAspect_mod
   use mapl_UngriddedDimsAspect_mod

   use mapl_StateRegistry_mod
   use mapl_EvalTransform_mod
   use mapl_NullTransform_mod
   use mapl_ComponentDriver_mod
   use mapl_ComponentDriver_mod
   use mapl_ComponentDriverVector_mod
   use mapl_ExtensionTransform_mod
   use mapl_MultiState_mod
   use mapl_ESMF_Utilities_mod, only: get_substate

   use mapl_VirtualConnectionPt_mod
   use mapl_VirtualConnectionPtVector_mod
   use mapl_ActualConnectionPt_mod
   use mapl_StateItemSpec_mod
   use mapl_StateItemSpec_mod

   use mapl_field_api
   use mapl_FieldUtilities_mod
   use mapl_enums_api, only: MAPL_STATEITEM_ALLOCATION_CREATED
   use mapl_StateArithmeticParser_mod
   use gftl2_StringVector

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use esmf

   implicit none(type,external)
   private

   public :: ExpressionClassAspect
   public :: to_ExpressionClassAspect
   public :: check_vertical_stagger_consistency

   interface to_ExpressionClassAspect
      procedure :: to_expressionclassaspect_from_poly
      procedure :: to_expressionclassaspect_from_map
   end interface to_ExpressionClassAspect

   ! No payload - just a placehold for expression
   type, extends(ClassAspect) :: ExpressionClassAspect
      private
      character(:), allocatable :: expression
      type(StateRegistry), pointer :: registry => null()
      type(ESMF_Field) :: payload ! to hold metadata
      ! Descriptive metadata declared on this export item itself (e.g. E_sum's own
      ! standard_name/long_name).  Not used by this aspect directly - exposed via
      ! get_standard_name()/get_long_name() so that whatever FieldClassAspect ends
      ! up superseding this one (via StateItemSpec%make_extension) can inherit it
      ! rather than silently losing it.  See generic/field-name-propagation.
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
   contains
      procedure :: get_aspect_order
      procedure :: get_mandatory_aspect_ids
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches
      procedure :: connect_to_import
      procedure :: connect_to_export

      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure, nopass :: get_aspect_id
      procedure :: get_payload
      procedure :: get_standard_name
      procedure :: get_long_name
   end type ExpressionClassAspect

   interface ExpressionClassAspect
      procedure :: new_ExpressionClassAspect
   end interface ExpressionClassAspect

contains

   function new_ExpressionClassAspect(expression, registry, standard_name, long_name) result(aspect)
      type(ExpressionClassAspect) :: aspect
      character(*), intent(in) :: expression
      type(StateRegistry), target, intent(in) :: registry
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name

      aspect%expression = expression
      aspect%registry => registry

      if (present(standard_name)) aspect%standard_name = standard_name
      if (present(long_name)) aspect%long_name = long_name

   end function new_ExpressionClassAspect

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(ExpressionClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [ &
           GEOM_ASPECT_ID, &
           VERTICAL_GRID_ASPECT_ID, &
           TYPEKIND_ASPECT_ID, &
           CLASS_ASPECT_ID &
           ]

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order

   function get_mandatory_aspect_ids(this) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(ExpressionClassAspect), intent(in) :: this

      aspect_ids = [AspectId:: ]
   end function get_mandatory_aspect_ids


   subroutine create(this, other_aspects, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVector) :: expression_variables

      this%payload = ESMF_FieldEmptyCreate(name='expression', _RC)
      call mapl_FieldSet(this%payload, allocation_status=MAPL_STATEITEM_ALLOCATION_CREATED, _RC)

      ! Best-effort, early attempt at the same resolution
      ! check_vertical_stagger_consistency performs again (more reliably) at
      ! actual connection time (make_transform): if this expression's own
      ! vertical_dim_spec was omitted (VERTICAL_STAGGER_INVALID - see
      ! ComponentSpecParser/parse_var_specs.F90) and its referenced variables
      ! already have a consistent, resolved vertical stagger *at this point*
      ! (e.g. an explicit vertical_dim_spec on a variable declared earlier in
      ! the same component's YAML), adopt it directly now.
      !
      ! This must be attempted here, not only in make_transform, because
      ! VERTICAL_GRID_ASPECT_ID precedes CLASS_ASPECT_ID in get_aspect_order:
      ! an expression whose own VerticalGridAspect is still unresolved
      ! (VERTICAL_STAGGER_INVALID) when this item is first connected crashes
      ! while extending the VERTICAL_GRID_ASPECT_ID aspect itself, before
      ! CLASS_ASPECT_ID - and this expression's own make_transform - is ever
      ! reached. See design.md Decisions.
      expression_variables = parser_variables_in_expression(this%expression, _RC)
      call check_vertical_stagger_consistency(this, other_aspects, expression_variables, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine activate(this, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

        type(StateItemSpec), pointer :: extension
        type(StateItemSpec), pointer :: spec
        type(StringVector) :: expression_variables
        type(StringVectorIterator) :: iter
        character(:), pointer :: variable
        type(VirtualConnectionPt) :: v_pt

        expression_variables = parser_variables_in_expression(this%expression, _RC)
        associate(b => expression_variables%begin(), e => expression_variables%end())
          iter = b
          do while (iter /= e)
             variable => iter%of()
             v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, variable)
             extension => this%registry%get_primary_spec(v_pt, _RC)
             spec => extension
             call spec%activate(_RC)
             call iter%next()
          enddo
        end associate
      _RETURN(ESMF_SUCCESS)
    end subroutine activate

   ! noop
   subroutine allocate(this, other_aspects, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(other_aspects)
   end subroutine allocate

   ! no op
   subroutine destroy(this, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine destroy

   ! no op
   subroutine connect_to_import(this, import, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(import)
   end subroutine connect_to_import

   ! no op
   subroutine connect_to_export(this, export, actual_pt, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(export)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_expressionclassaspect_from_poly(aspect, rc) result(expression_aspect)
      type(ExpressionClassAspect) :: expression_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (ExpressionClassAspect)
         expression_aspect = aspect
      class default
         _FAIL('aspect is not ExpressionClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_expressionclassaspect_from_poly

   function to_expressionclassaspect_from_map(map, rc) result(expression_aspect)
      type(ExpressionClassAspect) :: expression_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      expression_aspect = to_ExpressionClassAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_expressionclassaspect_from_map

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(ExpressionClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ComponentDriverVector), target :: input_couplers
      class(ComponentDriver), pointer :: coupler
      type(VirtualConnectionPtVector), target :: inputs

      type(MultiState) :: multi_state
      type(VirtualConnectionPt), pointer :: v_pt
      type(ActualConnectionPt) :: a_pt
      type(StateItemSpec), pointer :: new_extension
      type(StateItemSpec), pointer :: new_spec
      type(StateItemSpec), target :: goal_spec
      class(StateItemAspect), pointer :: class_aspect
      type(AspectMap), pointer :: goal_aspects
      type(ESMF_Field), allocatable :: field
      type(VirtualConnectionPtVector) :: empty
      integer :: n
      type(StringVector) :: expression_variables
      type(StringVectorIterator) :: iter
      character(:), pointer :: variable

      transform = NullTransform()
      multi_state = MultiState()

      select type (dst)
      type is (FieldClassAspect)

         expression_variables = parser_variables_in_expression(src%expression, _RC)
         associate (b => expression_variables%begin(), e => expression_variables%end())
         iter = b
         do while (iter /= e)
            variable => iter%of()
            call inputs%push_back(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, variable))
            call iter%next()
         enddo
         end associate

         ! Best-effort consistency check: whichever referenced variables
         ! already have a resolved vertical stagger at this point (their own
         ! VerticalGridAspect is not itself still mirrored) are compared to
         ! each other and to this item's own (already resolved, since
         ! VERTICAL_GRID_ASPECT_ID precedes CLASS_ASPECT_ID in
         ! get_aspect_order) resolved stagger. A variable whose stagger is
         ! not yet resolved is skipped rather than treated as an error - see
         ! design.md Decisions.
         call check_vertical_stagger_consistency(src, other_aspects, expression_variables, _RC)

         goal_spec = StateItemSpec(ESMF_STATEINTENT_EXPORT, other_aspects, empty)
         goal_aspects => goal_spec%get_aspects()
         n = goal_aspects%erase(CLASS_ASPECT_ID)
         call goal_aspects%insert(CLASS_ASPECT_ID, FieldClassAspect(standard_name='', long_name=''))
         call goal_spec%create(_RC)
         call goal_spec%allocate(_RC)

         do i = 1, inputs%size()
            v_pt => inputs%of(i)
            new_extension => src%registry%extend(v_pt, goal_spec, _RC)
            coupler => new_extension%get_producer()
            if (associated(coupler)) then
               call input_couplers%push_back(coupler)
            end if
            new_spec => new_extension

            class_aspect => new_spec%get_aspect(CLASS_ASPECT_ID, _RC)
            select type(class_aspect)
            type is (FieldClassAspect)
               call class_aspect%get_payload(field=field, _RC)
               a_pt = ActualConnectionPt(v_pt)
               call class_aspect%add_to_state(multi_state, a_pt, _RC)
            class default
               _FAIL("unsupported aspect type; must be FieldClassAspect")
            end select
        end do

         deallocate(transform)
         allocate(transform, source=EvalTransform(src%expression, multi_state%exportState, input_couplers))
      class default
         _FAIL('expression connected to non-field')
      end select

      _RETURN(_SUCCESS)
   end function make_transform

   ! Best-effort: compares whichever of this expression's referenced
   ! variables (plus the expression's own already-declared value, if any)
   ! already have a resolved vertical stagger at this point; a variable that
   ! is still genuinely mirrored (not yet resolved) is skipped rather than
   ! treated as an error. See design.md Decisions for why this cannot be a
   ! hard requirement.
   !
   ! If this expression's own vertical_dim_spec was omitted (parsed as
   ! VERTICAL_STAGGER_INVALID - see ComponentSpecParser/parse_var_specs.F90)
   ! and a consistent value is found among its resolved inputs, that value is
   ! adopted directly on this expression's own VerticalGridAspect here.
   !
   ! This direct approach - rather than forcing ASPECT_STATUS_MIRRORED in
   ! create() and relying on the generic mirror/ExtendTransform machinery -
   ! is required because a component-level vertical grid resource
   ! (MAPL_GridCompSetVerticalGrid) unconditionally overwrites *every* item's
   ! VerticalGridAspect in the registry to ASPECT_STATUS_SPECIFIED (see
   ! StateItemSpec::set_geometry/target_set_geom) well before this transform
   ! ever runs, including this expression's - even though it never touches
   ! vertical_stagger itself. is_mirror()/characteristic_state is therefore
   ! not a reliable signal of "not yet declared"; VERTICAL_STAGGER_INVALID is.
   ! By the time this make_transform runs (at actual connection time), that
   ! one-time component-level override has already happened, so directly
   ! setting the resolved value here is safe and final. See design.md
   ! Decisions.
   subroutine check_vertical_stagger_consistency(src, other_aspects, expression_variables, rc)
      class(ExpressionClassAspect), intent(in) :: src
      type(AspectMap), intent(in) :: other_aspects
      type(StringVector), intent(in) :: expression_variables
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n_vars
      character(:), allocatable :: variable
      type(VirtualConnectionPt) :: v_pt
      type(StateItemSpec), pointer :: var_spec
      type(VerticalGridAspect) :: var_vgrid
      type(VerticalGridAspect) :: own_vgrid
      type(VerticalStaggerLoc) :: var_stagger
      type(VerticalStaggerLoc) :: common_stagger
      type(VerticalStaggerLoc) :: own_stagger
      class(VerticalGrid), pointer :: var_vgrid_ptr
      class(VerticalGrid), allocatable :: common_vgrid
      logical :: own_is_unresolved
      logical :: have_common
      logical :: mismatch
      character(:), allocatable :: report
      class(StateItemAspect), pointer :: own_poly

      n_vars = expression_variables%size()
      _RETURN_IF(n_vars == 0)

      have_common = .false.
      mismatch = .false.
      report = ''

      own_vgrid = to_VerticalGridAspect(other_aspects, _RC)
      own_stagger = own_vgrid%get_vertical_stagger(_RC)
      ! Two independent signals both mean "not actually declared, treat as a
      ! candidate for resolution rather than an authoritative value to check
      ! others against": VERTICAL_STAGGER_INVALID is how an omitted
      ! vertical_dim_spec is parsed from YAML (see
      ! ComponentSpecParser/parse_var_specs.F90), but not every
      ! ExpressionClassAspect item goes through that parser - e.g. ExtData's
      ! "Derived" exports build their own VariableSpec without ever setting
      ! vertical_stagger at all, which leaves it at VerticalGridAspect's own
      ! CENTER-if-absent constructor default instead. is_mirror() catches
      ! that case (still genuinely unconnected). Conversely, is_mirror()
      ! alone is not reliable either - see the create()/make_transform
      ! comments above for why a component-level vertical grid resource can
      ! leave is_mirror() false while vertical_stagger is still
      ! VERTICAL_STAGGER_INVALID. Only treating both as false means this
      ! expression's own value was genuinely, deliberately declared.
      own_is_unresolved = (own_stagger == VERTICAL_STAGGER_INVALID) .or. own_vgrid%is_mirror()
      if (.not. own_is_unresolved) then
         common_stagger = own_stagger
         have_common = .true.
         report = report // ' expr=' // own_stagger%to_string()
      end if

      do i = 1, n_vars
         variable = expression_variables%of(i)
         v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, variable)

         ! A referenced variable may not be registered yet at all (this is
         ! called early, from create(), as well as later from
         ! make_transform): treat "not yet in the registry" the same as "not
         ! yet resolved" - skip, best effort - rather than treating it as an
         ! error. See design.md Decisions.
         if (.not. src%registry%has_virtual_pt(v_pt)) cycle

         var_spec => src%registry%get_primary_spec(v_pt, _RC)
         var_vgrid = to_VerticalGridAspect(var_spec%get_aspects(), _RC)

         if (var_vgrid%is_mirror()) cycle ! not yet resolved - skip (best effort)

         var_stagger = var_vgrid%get_vertical_stagger(_RC)
         report = report // ' ' // variable // '=' // var_stagger%to_string()

         if (.not. have_common) then
            common_stagger = var_stagger
            have_common = .true.
            if (var_stagger /= VERTICAL_STAGGER_NONE) then
               var_vgrid_ptr => var_vgrid%get_vertical_grid(_RC)
               allocate(common_vgrid, source=var_vgrid_ptr)
            end if
         else if (var_stagger /= common_stagger) then
            mismatch = .true.
         end if
      end do

      if (mismatch) then
         report = 'ExpressionClassAspect: inconsistent vertical_dim_spec among expression "' &
              // src%expression // '" and its resolved inputs:' // report
      end if
      _ASSERT(.not. mismatch, report)

      if (own_is_unresolved .and. have_common) then
         own_poly => other_aspects%at(VERTICAL_GRID_ASPECT_ID, _RC)
         select type (own_poly)
         type is (VerticalGridAspect)
            call own_poly%set_vertical_stagger(common_stagger)
            if (allocated(common_vgrid)) call own_poly%set_vertical_grid(common_vgrid)
         end select
      end if

      _RETURN(_SUCCESS)
   end subroutine check_vertical_stagger_consistency

   logical function supports_conversion_general(src)
      class(ExpressionClassAspect), intent(in) :: src

      supports_conversion_general = .true.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   ! Expressions can only evaluate to fields
   logical function supports_conversion_specific(src, dst)
      class(ExpressionClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.
      select type (dst)
      type is (FieldClassAspect)
         supports_conversion_specific = .true.
      end select

      _UNUSED_DUMMY(src)
   end function supports_conversion_specific

   ! No op
   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(ExpressionClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(multi_state)
      _UNUSED_DUMMY(actual_pt)
   end subroutine add_to_state

   ! noop
   subroutine add_to_bundle(this, field_bundle, rc)
      class(ExpressionClassAspect), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(field_bundle)
   end subroutine add_to_bundle

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

  function matches(src, dst)
      logical :: matches
      class(ExpressionClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.

!#      select type(dst)
!#      class is (FieldClassAspect)
!#         matches = .true.
!#      end select

      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
   end function matches

   subroutine get_payload(this, unusable, field, bundle, state, rc)
      class(ExpressionClassAspect), intent(in) :: this
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

   subroutine get_standard_name(this, standard_name)
      class(ExpressionClassAspect), intent(in) :: this
      character(:), allocatable, intent(out) :: standard_name

      if (allocated(this%standard_name)) standard_name = this%standard_name
   end subroutine get_standard_name

   subroutine get_long_name(this, long_name)
      class(ExpressionClassAspect), intent(in) :: this
      character(:), allocatable, intent(out) :: long_name

      if (allocated(this%long_name)) long_name = this%long_name
   end subroutine get_long_name

end module mapl_ExpressionClassAspect_mod
