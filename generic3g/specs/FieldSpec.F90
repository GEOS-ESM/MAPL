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

   use mapl3g_VerticalStaggerLoc
   use mapl3g_StateItemSpec
   use mapl3g_WildcardSpec
   use mapl3g_UngriddedDims
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_MultiState
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_InfoUtilities
   use mapl3g_ExtensionAction
   use mapl3g_VerticalGrid
   use mapl3g_BasicVerticalGrid
   use mapl3g_FixedLevelsVerticalGrid
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
   use mapl3g_VariableSpec, only: VariableSpec
   use mapl3g_VerticalRegridMethod
   use udunits2f, only: UDUNITS_are_convertible => are_convertible, udunit
   use gftl2_StringVector
   use esmf
   use nuopc
   use mapl3g_Field_API

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

      type(ESMF_Geom), allocatable :: geom
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalDimSpec) :: vertical_dim_spec = VERTICAL_DIM_UNKNOWN
      type(ESMF_Typekind_flag) :: typekind = ESMF_TYPEKIND_R4
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
!#      type(VariableSpec) :: variable_spec

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

      procedure :: make_adapters

      procedure :: set_geometry

! #ifndef __GFORTRAN__
      procedure :: write_formatted
! #endif
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geom
      module procedure new_FieldSpec_varspec
   end interface FieldSpec

   interface match
      procedure :: match_geom
      procedure :: match_string
      procedure :: match_vertical_dim_spec
      procedure :: match_ungridded_dims
   end interface match

   interface can_match
      procedure :: can_match_geom
      procedure :: can_match_vertical_grid
   end interface can_match

   type, extends(StateItemAdapter) :: GeomAdapter
      private
      type(ESMF_Geom), allocatable :: geom
      type(EsmfRegridderParam) :: regrid_param
   contains
      procedure :: adapt_one => adapt_geom
      procedure :: match_one => adapter_match_geom
   end type GeomAdapter

   interface GeomAdapter
      procedure :: new_GeomAdapter
   end interface GeomAdapter

   type, extends(StateItemAdapter) :: VerticalGridAdapter
      private
      class(VerticalGrid), allocatable :: vertical_grid
      type(ESMF_Geom), allocatable :: geom
      type(ESMF_TypeKind_Flag) :: typekind
      character(:), allocatable :: units
      type(VerticalDimSpec), allocatable :: vertical_dim_spec
      type(VerticalRegridMethod), allocatable :: regrid_method
   contains
      procedure :: adapt_one => adapt_vertical_grid
      procedure :: match_one => adapter_match_vertical_grid
   end type VerticalGridAdapter

   interface VerticalGridAdapter
      procedure :: new_VerticalGridAdapter
   end interface VerticalGridAdapter

   type, extends(StateItemAdapter) :: TypeKindAdapter
      private
      type(ESMF_Typekind_Flag) :: typekind
   contains
      procedure :: adapt_one => adapt_typekind
      procedure :: match_one => adapter_match_typekind
   end type TypeKindAdapter

   interface TypeKindAdapter
      procedure :: new_TypeKindAdapter
   end interface TypeKindAdapter

   type, extends(StateItemAdapter) :: UnitsAdapter
      private
      character(:), allocatable :: units
   contains
      procedure :: adapt_one => adapt_units
      procedure :: match_one => adapter_match_units
   end type UnitsAdapter

   interface UnitsAdapter
      procedure :: new_UnitsAdapter
   end interface UnitsAdapter

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
      if (present(regrid_param)) field_spec%regrid_param = regrid_param

      if (present(default_value)) field_spec%default_value = default_value
   end function new_FieldSpec_geom

   function new_FieldSpec_varspec(variable_spec) result(field_spec)
      type(FieldSpec) :: field_spec
      class(VariableSpec), intent(in) :: variable_spec

      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method

      _SET_FIELD(field_spec, variable_spec, vertical_dim_spec)
      _SET_FIELD(field_spec, variable_spec, typekind)
      _SET_FIELD(field_spec, variable_spec, ungridded_dims)
      _SET_FIELD(field_spec, variable_spec, attributes)
      _SET_FIELD(field_spec, variable_spec, regrid_param)
      _SET_ALLOCATED_FIELD(field_spec, variable_spec, standard_name)
      _SET_ALLOCATED_FIELD(field_spec, variable_spec, units)
      _SET_ALLOCATED_FIELD(field_spec, variable_spec, default_value)

      field_spec%long_name = 'unknown'
   end function new_FieldSpec_varspec

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(FieldSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method

      if (present(geom)) this%geom = geom
      if (present(vertical_grid)) this%vertical_grid = vertical_grid

      _RETURN(_SUCCESS)
   end subroutine set_geometry

   subroutine create(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      this%is_created = .true.

      _RETURN(ESMF_SUCCESS)
   end subroutine create

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

      integer, allocatable :: num_levels_grid
      integer, allocatable :: num_levels
      type(VerticalStaggerLoc) :: vert_staggerloc

      _RETURN_UNLESS(this%is_active())

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      call ESMF_FieldEmptySet(this%payload, this%geom, _RC)

      if (allocated(this%vertical_grid)) then
         num_levels_grid = this%vertical_grid%get_num_levels()
      end if

      if (this%vertical_dim_spec == VERTICAL_DIM_NONE) then
         vert_staggerloc = VERTICAL_STAGGER_NONE
      else if (this%vertical_dim_spec == VERTICAL_DIM_EDGE) then
         vert_staggerloc = VERTICAL_STAGGER_EDGE
         num_levels = num_levels_grid + 1
      else if (this%vertical_dim_spec == VERTICAL_DIM_CENTER) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         num_levels = num_levels_grid
      else
         _FAIL('unknown stagger')
      end if

      call MAPL_FieldEmptyComplete(this%payload, &
           typekind=this%typekind, &
           ungridded_dims=this%ungridded_dims, &
           num_levels=num_levels, &
           vert_staggerLoc=vert_staggerLoc, &
           units=this%units, &
           standard_name=this%standard_name, &
           long_name=this%long_name, &
           _RC)
    
      bounds = get_ungridded_bounds(this, _RC)

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _ASSERT(fstatus == ESMF_FIELDSTATUS_COMPLETE, 'ESMF field status problem.')

      if (allocated(this%default_value)) then
         call FieldSet(this%payload, this%default_value, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate

! #ifndef __GFORTRAN__
   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FieldSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "FieldSpec("
      if (allocated(this%standard_name)) then
         write(unit, "(a, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "standard name:", this%standard_name
      end if
      if (allocated(this%long_name)) then
         write(unit, "(a, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "long name:", this%long_name
      end if
      if (allocated(this%units)) then
         write(unit, "(a, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "units:", this%units
      end if
      write(unit, "(a, dt'g0')", iostat=iostat, iomsg=iomsg) new_line("a"), this%vertical_dim_spec
      if (allocated(this%vertical_grid)) then
         write(unit, "(a, dt'g0', a)", iostat=iostat, iomsg=iomsg) new_line("a"), this%vertical_grid
      end if
      write(unit, "(a)") ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted
! #endif

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

         ! _ASSERT(MAPL_SameVerticalGrid(dst, src), 'cannot connect mismatched geom without coupler.')
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
              match(this%vertical_dim_spec, src_spec%vertical_dim_spec), &
              match(this%ungridded_dims, src_spec%ungridded_dims), &
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

  function get_payload(this) result(payload)
      type(ESMF_Field) :: payload
      class(FieldSpec), intent(in) :: this
      payload = this%payload
   end function get_payload

   function new_GeomAdapter(geom, regrid_param) result(geom_adapter)
      type(GeomAdapter) :: geom_adapter
      type(ESMF_Geom), optional, intent(in) :: geom
      type(EsmfRegridderParam), optional, intent(in) :: regrid_param

      if (present(geom)) geom_adapter%geom = geom

      geom_adapter%regrid_param = EsmfRegridderParam()
      if (present(regrid_param)) geom_adapter%regrid_param = regrid_param
   end function new_GeomAdapter

   subroutine adapt_geom(this, spec, action, rc)
      class(GeomAdapter), intent(in) :: this
      class(StateItemSpec), intent(inout) :: spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      select type (spec)
      type is (FieldSpec)
         action = RegridAction(spec%geom, this%geom, this%regrid_param)
         spec%geom = this%geom
      end select

      _RETURN(_SUCCESS)
   end subroutine adapt_geom

   logical function adapter_match_geom(this, spec, rc) result(match)
      class(GeomAdapter), intent(in) :: this
      class(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      match = .false.
      select type (spec)
      type is (FieldSpec)
         match = match_geom(spec%geom, this%geom)
      end select

      _RETURN(_SUCCESS)
   end function adapter_match_geom

   function new_VerticalGridAdapter(vertical_grid, geom, typekind, units, vertical_dim_spec, regrid_method) result(vertical_grid_adapter)
      type(VerticalGridAdapter) :: vertical_grid_adapter
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      character(*), optional, intent(in) :: units
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      type(VerticalRegridMethod), optional, intent(in) :: regrid_method

      if (present(vertical_grid)) vertical_grid_adapter%vertical_grid = vertical_grid
      if (present(geom)) vertical_grid_adapter%geom = geom
      vertical_grid_adapter%typekind = typekind
      if (present(units)) vertical_grid_adapter%units = units
      vertical_grid_adapter%vertical_dim_spec = vertical_dim_spec
      if (present(regrid_method)) vertical_grid_adapter%regrid_method = regrid_method
   end function new_VerticalGridAdapter

   subroutine adapt_vertical_grid(this, spec, action, rc)
      class(VerticalGridAdapter), intent(in) :: this
      class(StateItemSpec), intent(inout) :: spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver), pointer :: v_in_coupler
      type(GriddedComponentDriver), pointer :: v_out_coupler
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(ESMF_TypeKind_Flag) :: typekind_in, typekind_out
      integer :: status

      select type (spec)
      type is (FieldSpec)
         ! TODO: DO WE NEED TO RESTRICT SPEC's VERTICAL GRID TO MODEL?
         ! NOTE: we cannot import ModelVerticalGrid (circular dependency)
         _ASSERT(spec%vertical_grid%get_units() == this%vertical_grid%get_units(), 'units must match')
         ! Field (to be regridded) should have the same typekind as the underlying vertical grid
         ! TODO: Should we add a typekind class variable to VerticalGrid?
         _ASSERT(spec%typekind == this%typekind, 'typekind must match')
         call spec%vertical_grid%get_coordinate_field( &
              v_in_coord, v_in_coupler, & ! output
              'ignore', spec%geom, spec%typekind, this%vertical_grid%get_units(), spec%vertical_dim_spec, _RC)
         call this%vertical_grid%get_coordinate_field( &
              v_out_coord, v_out_coupler, & ! output
              'ignore', this%geom, this%typekind, this%units, this%vertical_dim_spec, _RC)
         action = VerticalRegridAction(v_in_coord, v_out_coupler, v_out_coord, v_out_coupler, this%regrid_method)
         spec%vertical_grid = this%vertical_grid
         spec%vertical_dim_spec = this%vertical_dim_spec
      end select

      _RETURN(_SUCCESS)
   end subroutine adapt_vertical_grid

   logical function adapter_match_vertical_grid(this, spec, rc) result(match)

      class(VerticalGridAdapter), intent(in) :: this
      class(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

      match = .false.
      select type (spec)
      type is (FieldSpec)
         match = same_vertical_grid(spec%vertical_grid, this%vertical_grid, _RC)
      end select

      _RETURN(_SUCCESS)

   contains

      logical function same_vertical_grid(src_grid, dst_grid, rc)
         class(VerticalGrid), intent(in) :: src_grid
         class(VerticalGrid), allocatable, intent(in) :: dst_grid
         integer, optional, intent(out) :: rc

         same_vertical_grid = .false.
         if (.not. allocated(dst_grid)) then
            same_vertical_grid = .true.
            _RETURN(_SUCCESS) ! mirror grid
         end if

         same_vertical_grid = src_grid%same_id(dst_grid)
         if (same_vertical_grid) then
            _RETURN(_SUCCESS)
         end if

         select type(src_grid)
         type is(BasicVerticalGrid)
            select type(dst_grid)
            type is(BasicVerticalGrid)
               same_vertical_grid = (src_grid%get_num_levels() == dst_grid%get_num_levels())
            class default
               _FAIL("not implemented yet")
            end select
         type is(FixedLevelsVerticalGrid)
            select type(dst_grid)
            type is(FixedLevelsVerticalGrid)
               same_vertical_grid = (src_grid == dst_grid)
            class default
               same_vertical_grid = .false.
            end select
         class default
            same_vertical_grid = .false.
            ! _FAIL("not implemented yet")
         end select

         _RETURN(_SUCCESS)
      end function same_vertical_grid

   end function adapter_match_vertical_grid

   function new_TypekindAdapter(typekind) result(typekind_adapter)
      type(TypekindAdapter) :: typekind_adapter
      type(ESMF_Typekind_Flag), intent(in) :: typekind

      typekind_adapter%typekind = typekind
   end function new_TypekindAdapter

   subroutine adapt_typekind(this, spec, action, rc)
      class(TypekindAdapter), intent(in) :: this
      class(StateItemSpec), intent(inout) :: spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      select type (spec)
      type is (FieldSpec)
         spec%typekind = this%typekind
         action = CopyAction(spec%typekind, this%typekind)
      end select

      _RETURN(_SUCCESS)
   end subroutine adapt_typekind

   logical function adapter_match_typekind(this, spec, rc) result(match)
      class(TypekindAdapter), intent(in) :: this
      class(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      match = .false.
      select type (spec)
      type is (FieldSpec)
         match = any(this%typekind == [spec%typekind,MAPL_TYPEKIND_MIRROR])
      end select

      _RETURN(_SUCCESS)
   end function adapter_match_typekind

   function new_UnitsAdapter(units) result(units_adapter)
      type(UnitsAdapter) :: units_adapter
      character(*), optional, intent(in) :: units

      if (present(units)) units_adapter%units = units
   end function new_UnitsAdapter

   subroutine adapt_units(this, spec, action, rc)
      class(UnitsAdapter), intent(in) :: this
      class(StateItemSpec), intent(inout) :: spec
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc

      select type (spec)
      type is (FieldSpec)
         action = ConvertUnitsAction(spec%units, this%units)
         spec%units = this%units
      end select

      _RETURN(_SUCCESS)
   end subroutine adapt_units

   logical function adapter_match_units(this, spec, rc) result(match)
      class(UnitsAdapter), intent(in) :: this
      class(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      match = .false.
      select type (spec)
      type is (FieldSpec)
         match = .true.
         if (.not. allocated(this%units)) return
         match = (this%units == spec%units)
      end select

      _RETURN(_SUCCESS)
   end function adapter_match_units

   recursive function make_adapters(this, goal_spec, rc) result(adapters)
      type(StateItemAdapterWrapper), allocatable :: adapters(:)
      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(VerticalGridAdapter) :: vertical_grid_adapter
      integer :: status

      select type (goal_spec)
      type is (FieldSpec)
         allocate(adapters(4))
         allocate(adapters(1)%adapter, source=GeomAdapter(goal_spec%geom, goal_spec%regrid_param))
         vertical_grid_adapter = VerticalGridAdapter( &
              goal_spec%vertical_grid, &
              goal_spec%geom, &
              goal_spec%typekind, &
              goal_spec%units, &
              goal_spec%vertical_dim_spec, &
              VERTICAL_REGRID_LINEAR)
         allocate(adapters(2)%adapter, source=vertical_grid_adapter)
         allocate(adapters(3)%adapter, source=TypeKindAdapter(goal_spec%typekind))
         allocate(adapters(4)%adapter, source=UnitsAdapter(goal_spec%units))
      type is (WildCardSpec)
         adapters = goal_spec%make_adapters(goal_spec, _RC)
      class default
         allocate(adapters(0))
         _FAIL('unsupported subclass of StateItemSpec')
      end select

      _RETURN(_SUCCESS)
   end function make_adapters

end module mapl3g_FieldSpec

#undef _SET_FIELD
#undef _SET_ALLOCATED_FIELD
