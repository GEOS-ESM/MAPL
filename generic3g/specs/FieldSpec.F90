#include "MAPL_Generic.h"

#if defined _SET_FIELD
#  undef _SET_FIELD
#endif
#define _SET_FIELD(A, B, F) A%F = B%F

#if defined(_SET_ALLOCATED_FIELD)
#  undef _SET_ALLOCATED_FIELD
#endif
#define _SET_ALLOCATED_FIELD(A, B, F) if(allocated(B%F)) _SET_FIELD(A, B, F)

module mapl3g_FieldSpec

   use mapl3g_StateItemSpec
   use mapl3g_UngriddedDims
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_MultiState
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_ExtensionAction
   use mapl3g_VerticalGrid
   use mapl3g_VerticalRegridAction
   use mapl3g_VerticalDimSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_NullAction
   use mapl3g_CopyAction
   use mapl3g_RegridAction
   use mapl3g_EsmfRegridder, only: EsmfRegridderParam
   use mapl3g_ConvertUnitsAction
   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use mapl3g_LU_Bound
   use mapl3g_geom_mgr, only: MAPL_SameGeom
   use mapl3g_FieldDictionary
   use mapl3g_GriddedComponentDriver
   use mapl3g_VariableSpec
   use udunits2f, only: UDUNITS_are_convertible => are_convertible, udunit
   use gftl2_StringVector
   use esmf
   use nuopc

   implicit none
   private

   public :: FieldSpec
   public :: new_FieldSpec_geom

   ! Two FieldSpec's can be connected if:
   !   1) They only differ in the following components:
   !      - geom (couple with Regridder)
   !      - vertical_regrid (couple with VerticalRegridder)
   !      - typekind (Copy)
   !      - units (Convert)
   !      - frequency_spec (tbd)
   !      - halo width (tbd)
   !   2) They have the same values for
   !      - ungridded_dims
   !      - standard_name
   !      - long_name
   !      - regrid_param
   !      - default_value
   !   3) The attributes of destination spec are a subset of the
   !      attributes of the source spec.

   type, extends(StateItemSpec) :: FieldSpec

      private
      type(ESMF_Geom), allocatable :: geom
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalDimSpec) :: vertical_dim_spec = VERTICAL_DIM_UNKNOWN
      type(ESMF_typekind_flag) :: typekind = ESMF_TYPEKIND_R4
      type(UngriddedDims) :: ungridded_dims
      type(StringVector) :: attributes
      type(EsmfRegridderParam) :: regrid_param

      ! Metadata
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
      character(:), allocatable :: units
      ! TBD
!#      type(FrequencySpec) :: freq_spec
!#      class(AbstractFrequencySpec), allocatable :: freq_spec
!#      integer :: halo_width = 0

      type(ESMF_Field) :: payload
      real, allocatable :: default_value
      type(VariableSpec) :: variable_spec

      logical :: is_created = .false.

   contains

      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_payload

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: extension_cost
      procedure :: make_extension

      procedure :: set_info
      procedure :: initialize

   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geom
      module procedure new_FieldSpec_varspec
!#      module procedure new_FieldSpec_defaults
   end interface FieldSpec

   interface match
      procedure :: match_geom
      procedure :: match_typekind
      procedure :: match_string
      procedure :: match_vertical_dim_spec
      procedure :: match_ungridded_dims
   end interface match

   interface can_match
      procedure :: can_match_geom
      procedure :: can_match_vertical_grid
   end interface can_match

   interface get_cost
      procedure :: get_cost_geom
      procedure :: get_cost_typekind
      procedure :: get_cost_string
   end interface get_cost

   interface update_item
      procedure update_item_geom
      procedure update_item_typekind
      procedure update_item_string
   end interface update_item

contains

   function new_FieldSpec_geom(unusable, geom, vertical_grid, vertical_dim_spec, typekind, ungridded_dims, &
        standard_name, long_name, units, &
        attributes, regrid_param, default_value) result(field_spec)
      type(FieldSpec) :: field_spec

      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(UngriddedDims), intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      type(StringVector), optional, intent(in) :: attributes
      type(EsmfRegridderParam), optional, intent(in) :: regrid_param

      ! optional args last
      real, optional, intent(in) :: default_value

      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method
      integer :: status

      if (present(geom)) field_spec%geom = geom
      if (present(vertical_grid)) field_spec%vertical_grid = vertical_grid
      field_spec%vertical_dim_spec = vertical_dim_spec
      field_spec%typekind = typekind
      field_spec%ungridded_dims = ungridded_dims

      if (present(standard_name)) field_spec%standard_name = standard_name
      if (present(long_name)) field_spec%long_name = long_name
      if (present(units)) field_spec%units = units
      if (present(attributes)) field_spec%attributes = attributes

      ! regrid_param
      field_spec%regrid_param = EsmfRegridderParam() ! use default regrid method
      regrid_method = get_regrid_method_(field_spec%standard_name)
      field_spec%regrid_param = EsmfRegridderParam(regridmethod=regrid_method)
      if (present(regrid_param)) field_spec%regrid_param = regrid_param

      if (present(default_value)) field_spec%default_value = default_value

   end function new_FieldSpec_geom

   function new_FieldSpec_varspec(variable_spec) result(field_spec)
      type(FieldSpec) :: field_spec
      class(VariableSpec), intent(in) :: variable_spec

      field_spec%variable_spec = variable_spec
      field_spec%long_name = ' '
      !wdb fixme deleteme  long_name is set here based on the VariableSpec
      !                    make_FieldSpec method

   end function new_FieldSpec_varspec
      
   function get_regrid_method_(stdname, rc) result(regrid_method)
      type(ESMF_RegridMethod_Flag) :: regrid_method
      character(:), allocatable, intent(in) :: stdname
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: field_dictionary_file = "field_dictionary.yml"
      type(FieldDictionary) :: field_dict
      logical :: file_exists
      integer :: status

      regrid_method = ESMF_REGRIDMETHOD_BILINEAR ! default value
      if (allocated(stdname)) then
         inquire(file=trim(field_dictionary_file), exist=file_exists)
         if (file_exists) then
            field_dict = FieldDictionary(filename=field_dictionary_file, _RC)
            regrid_method = field_dict%get_regrid_method(stdname, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
   end function get_regrid_method_

   subroutine initialize(this, geom, vertical_grid, registry, rc)
      class(FieldSpec), intent(inout) :: this
      type(ESMF_Geom), intent(inout) :: geom
      class(VerticalGrid), intent(in) :: vertical_grid
      class(StateRegistry), intent(in) :: registry
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method
      type(ActualPtVector) :: dependencies

      _UNUSED_DUMMY(registry)

      associate (variable_spec => this%variable_spec)
         if(allocated(this%geom)) deallocate(this%geom)
         this%geom = geom
         if(allocated(this%vertical_grid) deallocate(this%vertical_grid)
         this%vertical_grid = vertical_grid
         _SET_FIELD(this, variable_spec, vertical_dim_spec)
         _SET_FIELD(this, variable_spec, typekind)
         _SET_FIELD(this, variable_spec, ungridded_dims)
         _SET_FIELD(this, variable_spec, attributes)
         _SET_ALLOCATED_FIELD(this, variable_spec, standard_name)
         _SET_ALLOCATED_FIELD(this, variable_spec, units)
         _SET_ALLOCATED_FIELD(this, variable_spec, default_value)

         this%regrid_param = EsmfRegridderParam() ! use default regrid method
         regrid_method = get_regrid_method_(this%standard_name)
         this%regrid_param = EsmfRegridderParam(regridmethod=regrid_method)

         dependencies = variable_spec%make_dependencies(_RC)
         call this%set_dependencies(dependencies)
         call this%set_raw_dependencies(this%dependencies)

         if (variable_spec%state_intent == ESMF_STATEINTENT_INTERNAL) then
            call this%set_active()
         end if
      end associate

      _RETURN(_SUCCESS)
      
   end subroutine initialize

!#   function new_FieldSpec_defaults(ungridded_dims, geom, units) result(field_spec)
!#      type(FieldSpec) :: field_spec
!#      type(ExtraDimsSpec), intent(in) :: ungridded_dims
!#      type(ESMF_Geom), intent(in) :: geom
!#      character(*), intent(in) :: units
!#
!#      field_spec = FieldSpec(ungridded_dims, ESMF_TYPEKIND_R4, geom, units)
!#
!#   end function new_FieldSpec_defaults
!#

   subroutine create(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      this%is_created = .true.

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine MAPL_FieldEmptySet(field, geom, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Geom), intent(inout) :: geom
      integer, optional, intent(out) ::rc

      type(ESMF_GeomType_Flag) :: geom_type
      type(ESMF_Grid) :: grid
      type(ESMF_Mesh) :: mesh
      type(ESMF_XGrid) :: xgrid
      type(ESMF_LocStream) :: locstream
      integer :: status

      call ESMF_GeomGet(geom, geomtype=geom_type, _RC)
      if(geom_type == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         call ESMF_FieldEmptySet(field, grid, _RC)
      else if (geom_type == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomGet(geom, mesh=mesh, _RC)
         call ESMF_FieldEmptySet(field, mesh, _RC)
      else if (geom_type == ESMF_GEOMTYPE_XGRID) then
         call ESMF_GeomGet(geom, xgrid=xgrid, _RC)
         call ESMF_FieldEmptySet(field, xgrid, _RC)
      else if (geom_type == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom, locstream=locstream, _RC)
         call ESMF_FieldEmptySet(field, locstream, _RC)
      else
         _FAIL('Unsupported type of Geom')
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_FieldEmptySet

   subroutine destroy(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, nogarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus
      type(LU_Bound), allocatable :: bounds(:)

      _RETURN_UNLESS(this%is_active())

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      call MAPL_FieldEmptySet(this%payload, this%geom, _RC)

      bounds = get_ungridded_bounds(this, _RC)
      call ESMF_FieldEmptyComplete(this%payload, this%typekind, &
           ungriddedLBound=bounds%lower,  &
           ungriddedUBound=bounds%upper,  &
           _RC)
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _ASSERT(fstatus == ESMF_FIELDSTATUS_COMPLETE, 'ESMF field status problem.')
      if (allocated(this%default_value)) then
         call FieldSet(this%payload, this%default_value, _RC)
      end if

      call this%set_info(this%payload, _RC)

      _RETURN(ESMF_SUCCESS)

   end subroutine allocate

   function get_ungridded_bounds(this, rc) result(bounds)
      type(LU_Bound), allocatable :: bounds(:)
      type(FieldSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer:: num_levels
      type(LU_Bound) :: vertical_bounds

      _ASSERT(this%vertical_dim_spec /= VERTICAL_DIM_UNKNOWN, 'vertical_dim_spec has not been specified')

      bounds = this%ungridded_dims%get_bounds()
      if (this%vertical_dim_spec == VERTICAL_DIM_NONE) return

      vertical_bounds = get_vertical_bounds(this%vertical_dim_spec, this%vertical_grid, _RC)
      bounds = [vertical_bounds, bounds]

      _RETURN(_SUCCESS)
   end function get_ungridded_bounds

   function get_vertical_bounds(vertical_dim_spec, vertical_grid, rc) result(bounds)
      type(LU_Bound) :: bounds
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      class(VerticalGrid), intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(vertical_dim_spec /= VERTICAL_DIM_UNKNOWN, 'vertical_dim_spec has not been specified')
      bounds%lower = 1
      bounds%upper = vertical_grid%get_num_levels()

      if (vertical_dim_spec == VERTICAL_DIM_EDGE) then
         bounds%upper = bounds%upper + 1
      end if

      _RETURN(_SUCCESS)
   end function get_vertical_bounds

  subroutine connect_to(this, src_spec, actual_pt, rc)
      class(FieldSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      integer :: status
      interface mirror
         procedure :: mirror_geom
         procedure :: mirror_vertical_grid
         procedure :: mirror_typekind
         procedure :: mirror_string
         procedure :: mirror_real
         procedure :: mirror_vertical_dim_spec
         procedure :: mirror_ungriddedDims
      end interface mirror

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (FieldSpec)
         ! Import fields are preemptively created just so that they
         ! can still be queried even when not satisfied.  It is
         ! possible that such is not really necessary.  But for now
         ! when an import is ultimately connected we must destroy the
         ! ESMF_Field object before copying the payload from the
         ! source spec.
         call this%destroy(_RC)
         this%payload = src_spec%payload

         call mirror(dst=this%geom, src=src_spec%geom)
         call mirror(dst=this%vertical_grid, src=src_spec%vertical_grid)
         call mirror(dst=this%typekind, src=src_spec%typekind)
         call mirror(dst=this%units, src=src_spec%units)
         call mirror(dst=this%vertical_dim_spec, src=src_spec%vertical_dim_spec)
         call mirror(dst=this%default_value, src=src_spec%default_value)
         call mirror(dst=this%ungridded_dims, src=src_spec%ungridded_dims)

      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   contains


      subroutine mirror_geom(dst, src)
         type(ESMF_Geom), allocatable, intent(inout) :: dst, src

         _ASSERT(allocated(dst) .or. allocated(src), 'cannot double mirror')
         if (allocated(dst) .and. .not. allocated(src)) then
            src = dst
            return
         end if

         if (allocated(src) .and. .not. allocated(dst)) then
            dst = src
            return
         end if

         _ASSERT(MAPL_SameGeom(dst, src), 'cannot connect mismatched geom without coupler.')

      end subroutine mirror_geom

      subroutine mirror_vertical_grid(dst, src)
         class(VerticalGrid), allocatable, intent(inout) :: dst, src

         _ASSERT(allocated(dst) .or. allocated(src), 'cannot double mirror')
         if (allocated(dst) .and. .not. allocated(src)) then
            src = dst
            return
         end if

         if (allocated(src) .and. .not. allocated(dst)) then
            dst = src
            return
         end if

!         _ASSERT(MAPL_SameVerticalGrid(dst, src), 'cannot connect mismatched geom without coupler.')

      end subroutine mirror_vertical_grid


      subroutine mirror_typekind(dst, src)
         type(ESMF_TypeKind_Flag), intent(inout) :: dst, src

         if (dst == src) return

         if (dst == MAPL_TYPEKIND_MIRROR) then
            dst = src
         end if

         if (src == MAPL_TYPEKIND_MIRROR) then
            src = dst
         end if

         _ASSERT(dst == src, 'unsupported typekind mismatch')
      end subroutine mirror_typekind

      ! Earlier checks should rule out double-mirror before this is
      ! called.
      subroutine mirror_vertical_dim_spec(dst, src)
         type(VerticalDimSpec), intent(inout) :: dst, src

         if (dst == src) return

         if (dst == VERTICAL_DIM_MIRROR) then
            dst = src
         end if

         if (src == VERTICAL_DIM_MIRROR) then
            src = dst
         end if

         _ASSERT(dst == src, 'unsupported vertical_dim_spec mismatch')
      end subroutine mirror_vertical_dim_spec

      subroutine mirror_string(dst, src)
         character(len=:), allocatable, intent(inout) :: dst, src

         if (allocated(dst) .eqv. allocated(src)) return

         if (.not. allocated(dst)) then
            dst = src
         end if

         if (.not. allocated(src)) then
            src = dst
         end if

      end subroutine mirror_string

      subroutine mirror_real(dst, src)
         real, allocatable, intent(inout) :: dst, src

         if (allocated(dst) .eqv. allocated(src)) return

         if (.not. allocated(dst)) then
            dst = src
         end if

         if (.not. allocated(src)) then
            src = dst
         end if

      end subroutine mirror_real

      subroutine mirror_ungriddedDims(dst, src)
         type(UngriddedDims), intent(inout) :: dst, src

         type(UngriddedDims) :: mirror_dims
         mirror_dims = mirror_ungridded_dims()

         if (dst == src) return

         if (dst == mirror_dims) then
            dst = src
         end if

         if (src == mirror_dims) then
            src = dst
         end if

      end subroutine mirror_ungriddedDims

   end subroutine connect_to



   logical function can_connect_to(this, src_spec, rc)
      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      logical :: can_convert_units
      integer :: status

      select type(src_spec)
      class is (FieldSpec)
         can_convert_units = can_connect_units(this%units, src_spec%units, _RC)

         can_connect_to = all ([ &
              can_match(this%geom,src_spec%geom), &
              can_match(this%vertical_grid, src_spec%vertical_grid), &
              match(this%vertical_dim_spec,src_spec%vertical_dim_spec), &
              match(this%ungridded_dims,src_spec%ungridded_dims), &
              includes(this%attributes, src_spec%attributes), &
              can_convert_units &
              ])
      class default
         can_connect_to = .false.
      end select
      _RETURN(_SUCCESS)

   contains

      logical function includes(mandatory, provided)
         type(StringVector), target, intent(in) :: mandatory
         type(StringVector), target, intent(in) :: provided

         integer :: i, j
         character(:), pointer :: attribute_name

         m: do i = 1, mandatory%size()
            attribute_name => mandatory%of(i)
            p: do j = 1, provided%size()
               if (attribute_name == provided%of(j)) cycle m
            end do p
            ! ith not found
            includes = .false.
            return
         end do m

         includes = .true.
      end function includes
   end function can_connect_to


   logical function same_typekind(a, b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b
      same_typekind = (a%typekind == b%typekind)
   end function same_typekind

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(FieldSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: full_name, inner_name
      integer :: idx

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(FieldSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleAdd(bundle, [this%payload], multiflag=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   integer function extension_cost(this, src_spec, rc) result(cost)
      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      cost = 0
      select type (src_spec)
      type is (FieldSpec)
         cost = cost + get_cost(this%geom, src_spec%geom)
         cost = cost + get_cost(this%typekind, src_spec%typekind)
         cost = cost + get_cost(this%units, src_spec%units)
      class default
         _FAIL('Cannot extend to this StateItemSpec subclass.')
      end select

      _RETURN(_SUCCESS)
   end function extension_cost


   subroutine make_extension(this, dst_spec, new_spec, action, rc)
      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      class(StateItemSpec), allocatable, intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status
      type(FieldSpec) :: tmp_spec

      select type(dst_spec)
      type is (FieldSpec)
         call make_extension_safely(this, dst_spec, tmp_spec, action, _RC)
         allocate(new_spec, source=tmp_spec)
      class default
         _FAIL('Unsupported subclass.')
      end select

      _RETURN(_SUCCESS)
   end subroutine make_extension

   subroutine make_extension_safely(this, dst_spec, new_spec, action, rc)
      class(FieldSpec), intent(in) :: this
      type(FieldSpec), intent(in) :: dst_spec
      type(FieldSpec), intent(out) :: new_spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver), pointer :: v_in_coupler
      type(GriddedComponentDriver), pointer :: v_out_coupler
      type(ESMF_Field) :: v_in_coord, v_out_coord

      new_spec = this ! plus one modification from below
      _ASSERT(allocated(this%geom), 'Source spec must specify a valid geom.')
      if (.not. same_geom(this%geom, dst_spec%geom)) then
         action = RegridAction(this%geom, dst_spec%geom, dst_spec%regrid_param)
         new_spec%geom = dst_spec%geom
         _RETURN(_SUCCESS)
      end if
      
      _ASSERT(allocated(this%vertical_grid), 'Source spec must specify a valid vertical grid.')
      if (.not. same_vertical_grid(this%vertical_grid, dst_spec%vertical_grid)) then
         _HERE
         call this%vertical_grid%get_coordinate_field(v_in_coord, v_in_coupler, &
              'ignore', this%geom, this%typekind, this%units, _RC)
         call this%vertical_grid%get_coordinate_field(v_out_coord, v_out_coupler, &
              'ignore', dst_spec%geom, dst_spec%typekind, dst_spec%units, _RC)
         action = VerticalRegridAction(v_in_coord, v_out_coupler, v_out_coord, v_out_coupler, VERTICAL_REGRID_LINEAR)
         _RETURN(_SUCCESS)
      end if
      
!#   if (.not. same_freq_spec(this%freq_spec, dst_spec%freq_spec)) then
!#      action = VerticalRegridAction(this%freq_spec, dst_spec%freq_spec
!#      new_spec%freq_spec = dst_spec%freq_spec
!!$         _RETURN(_SUCCESS)
!#   end if
      
      if (this%typekind  /=  dst_spec%typekind) then
         action = CopyAction(this%typekind, dst_spec%typekind)
         new_spec%typekind = dst_spec%typekind
         _RETURN(_SUCCESS)
      end if
      
      if (.not. same_units(this%units, dst_spec%units)) then
         action = ConvertUnitsAction(this%units, dst_spec%units)
         new_spec%units = dst_spec%units
         _RETURN(_SUCCESS)
      end if
      
      _FAIL('No extensions found for this.')
   
   contains

      
      logical function same_geom(src_geom, dst_geom)
         type(ESMF_Geom), intent(in) :: src_geom
         type(ESMF_Geom), allocatable, intent(in) :: dst_geom
         
         same_geom = .true.
         if (.not. allocated(dst_geom)) return ! mirror geom
         
         same_geom = MAPL_SameGeom(src_geom, dst_geom)
         
      end function same_geom
 
     logical function same_vertical_grid(src_grid, dst_grid)
        class(VerticalGrid), intent(in) :: src_grid
        class(VerticalGrid), allocatable, intent(in) :: dst_grid
         
         same_vertical_grid = .true.
         if (.not. allocated(dst_grid)) return ! mirror geom
         
         same_vertical_grid = src_grid%same_id(dst_grid)
         
      end function same_vertical_grid
      
      logical function same_units(src_units, dst_units)
         character(*), intent(in) :: src_units
         character(:), allocatable, intent(in) :: dst_units
         
         same_units = .true.
         if (.not. allocated(dst_units)) return ! mirror units
      
         same_units = (src_units == dst_units)
         
      end function same_units
      
   end subroutine make_extension_safely



   logical function can_match_geom(a, b) result(can_match)
      type(ESMF_Geom), allocatable, intent(in) :: a, b

      integer :: n_mirror

      ! At most one geom can be mirror (unallocated).
      ! Otherwise, assume ESMF can provide regrid
      n_mirror = count([.not. allocated(a), .not. allocated(b)])
      can_match = n_mirror <= 1

   end function can_match_geom

   logical function can_match_vertical_grid(a, b) result(can_match)
      class(VerticalGrid), allocatable, intent(in) :: a, b

      integer :: n_mirror

      ! At most one grid can be mirror (unallocated).
      ! Otherwise, see if regrid is supported
      n_mirror = count([.not. allocated(a), .not. allocated(b)])
      can_match = n_mirror <= 1

   end function can_match_vertical_grid


   logical function match_geom(a, b) result(match)
      type(ESMF_Geom), allocatable, intent(in) :: a, b

      integer :: status
      integer :: n_mirror

      ! At most one geom can be mirror (unallocated).
      ! Otherwise, assume ESMF can provide regrid
      n_mirror = count([.not. allocated(a), .not. allocated(b)])

      select case (n_mirror)
      case (0)
         match = MAPL_SameGeom(a,b)
      case (1)
         match = .true.
      case (2)
         match = .true.
      end select

   end function match_geom

   logical function match_typekind(a, b) result(match)
      type(ESMF_TypeKind_Flag), intent(in) :: a, b

      integer :: n_mirror

      n_mirror = count([a%dkind,b%dkind] == MAPL_TYPEKIND_MIRROR%dkind)
      match = (n_mirror == 1) .or. (n_mirror == 0 .and. a == b)

   end function match_typekind

   logical function match_string(a, b) result(match)
      character(:), allocatable, intent(in) :: a, b

      logical :: mirror_a, mirror_b

      match = (mirror(a) .neqv. mirror(b))
      if (match) return

      ! Neither is mirror
      if (allocated(a) .and. allocated(b)) then
         match = (a == b)
         return
      end if

      ! Both are mirror
      match = .false.
   end function match_string

   logical function match_vertical_dim_spec(a, b) result(match)
      type(VerticalDimSpec), intent(in) :: a, b

      integer :: n_mirror

      n_mirror = count([a,b] == VERTICAL_DIM_MIRROR)
      match = (n_mirror == 1) .or. (n_mirror == 0 .and. a == b)

   end function match_vertical_dim_spec

   logical function match_ungridded_dims(a, b) result(match)
      type(UngriddedDims), intent(in) :: a, b

      type(UngriddedDims) :: mirror_dims
      integer :: n_mirror

      mirror_dims = MIRROR_UNGRIDDED_DIMS()
      n_mirror = count([a == mirror_dims, b == mirror_dims])
      match = (n_mirror == 1) .or. (n_mirror == 0 .and. a == b)

   end function match_ungridded_dims

   logical function mirror(str)
      character(:), allocatable :: str

      mirror = .not. allocated(str)
      if (mirror) return

      mirror = (str == '_MIRROR_')
   end function mirror

   logical function can_connect_units(dst_units, src_units, rc)
      character(:), allocatable, intent(in) :: dst_units
      character(:), allocatable, intent(in) :: src_units
      integer, optional, intent(out) :: rc

      integer :: status

      ! If mirror or same, we can connect without a coupler
      can_connect_units = match(dst_units, src_units)
      _RETURN_IF(can_connect_units)

      ! Otherwise need a coupler, but need to check if units are convertible
      can_connect_units = UDUNITS_are_convertible(src_units, dst_units, _RC)
      _RETURN(_SUCCESS)
   end function can_connect_units

   integer function get_cost_geom(a, b) result(cost)
      type(ESMF_GEOM), allocatable, intent(in) :: a, b
      cost = 0
      if (.not. match(a,b)) cost = 1
   end function get_cost_geom

   integer function get_cost_typekind(a, b) result(cost)
      type(ESMF_TypeKind_Flag), intent(in) :: a, b
      cost = 0
      if (.not. match(a,b)) cost = 1
   end function get_cost_typekind

   integer function get_cost_string(a, b) result(cost)
      character(:), allocatable, intent(in) :: a, b
      cost = 0
      if (.not. match(a,b)) cost = 1
   end function get_cost_string

   logical function update_item_geom(a, b)
      type(ESMF_GEOM), allocatable, intent(inout) :: a
      type(ESMF_GEOM), allocatable, intent(in) :: b

      update_item_geom = .false.

      if (.not. allocated(b)) return ! nothing to do (no coupler)

      if (.not. allocated(a)) then ! Fill-in ExtData (no coupler)
         a = b
         return
      end if

      if (MAPL_SameGeom(a,b)) return
      update_item_geom = .true.
      a = b


   end function update_item_geom

   logical function update_item_typekind(a, b)
      type(ESMF_TypeKind_Flag), intent(inout) :: a
      type(ESMF_TypeKind_Flag), intent(in) :: b

      update_item_typekind = .false.
      if (.not. match(a, b)) then
         a = b
         update_item_typekind = .true.
      end if

   end function update_item_typekind

   logical function update_item_string(a, b)
      character(:), allocatable, intent(inout) :: a
      character(:), allocatable, intent(in) :: b

      update_item_string = .false.
      if (.not. match(a, b)) then
         a = b
         update_item_string = .true.
      end if
   end function update_item_string

   function get_payload(this) result(payload)
      type(ESMF_Field) :: payload
      class(FieldSpec), intent(in) :: this
      payload = this%payload
   end function get_payload

   subroutine set_info(this, field, rc)
      class(FieldSpec), intent(in) :: this
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: ungridded_dims_info
      type(ESMF_Info) :: vertical_dim_info
      type(ESMF_Info) :: vertical_grid_info

      type(ESMF_Info) :: field_info

      call ESMF_InfoGetFromHost(field, field_info, _RC)

      ungridded_dims_info = this%ungridded_dims%make_info(_RC)
      call ESMF_InfoSet(field_info, key='MAPL/ungridded_dims', value=ungridded_dims_info, _RC)
      call ESMF_InfoDestroy(ungridded_dims_info, _RC)

      vertical_dim_info = this%vertical_dim_spec%make_info(_RC)
      call ESMF_InfoSet(field_info, key='MAPL/vertical_dim', value=vertical_dim_info, _RC)
      call ESMF_InfoDestroy(vertical_dim_info, _RC)

      vertical_grid_info = this%vertical_grid%make_info(_RC)
      call ESMF_InfoSet(field_info, key='MAPL/vertical_grid', value=vertical_grid_info, _RC)
      call ESMF_InfoDestroy(vertical_grid_info, _RC)

      if (allocated(this%units)) then
         call ESMF_InfoSet(field_info, key='MAPL/units', value=this%units, _RC)
      end if
      if (allocated(this%long_name)) then
         call ESMF_InfoSet(field_info, key='MAPL/long_name', value=this%long_name, _RC)
      end if
      if (allocated(this%standard_name)) then
         call ESMF_InfoSet(field_info, key='MAPL/standard_name', value=this%standard_name, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine set_info
    
end module mapl3g_FieldSpec
#undef _SET_FIELD
#undef _SET_ALLOCATED_FIELD
