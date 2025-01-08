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
   use mapl3g_StateItemAspect
   use mapl3g_AspectCollection
   use mapl3g_GeomAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_HorizontalDimsSpec
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
   use mapl3g_ComponentDriver
   use mapl3g_VariableSpec, only: VariableSpec
   use mapl3g_VerticalRegridMethod
   use mapl3g_AccumulatorActionInterface
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

      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalDimSpec) :: vertical_dim_spec = VERTICAL_DIM_UNKNOWN
      type(StringVector) :: attributes
      type(EsmfRegridderParam) :: regrid_param

      ! Metadata
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
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
      procedure :: get_aspect_priorities

      procedure :: set_geometry

      procedure :: write_formatted
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geom
      module procedure new_FieldSpec_varspec
   end interface FieldSpec

   interface match
      procedure :: match_geom
      procedure :: match_string
      procedure :: match_vertical_dim_spec
   end interface match

   interface can_match
      procedure :: can_match_geom
      procedure :: can_match_vertical_grid
   end interface can_match


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

contains

   function new_FieldSpec_geom(unusable, geom, vertical_grid, vertical_dim_spec, typekind, ungridded_dims, &
        standard_name, long_name, units, &
        attributes, regrid_param, horizontal_dims_spec, default_value, accumulation_type, timestep) result(field_spec)
      type(FieldSpec), target :: field_spec

      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      type(StringVector), optional, intent(in) :: attributes
      type(EsmfRegridderParam), optional, intent(in) :: regrid_param
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec

      ! optional args last
      real, optional, intent(in) :: default_value
      character(*), optional, intent(in) :: accumulation_type
      type(ESMF_TimeInterval), optional, intent(in) :: timestep

      integer :: status
      type(AspectCollection), pointer :: aspects

      aspects => field_spec%get_aspects()

      call aspects%set_geom_aspect(GeomAspect(geom, regrid_param, horizontal_dims_spec))
      call aspects%set_units_aspect(UnitsAspect(units))
      call aspects%set_ungridded_dims_aspect(UngriddedDimsAspect(ungridded_dims))
      call aspects%set_typekind_aspect(TypekindAspect(typekind))
      call aspects%set_frequency_aspect(FrequencyAspect(timestep, accumulation_type))
      
      if (present(vertical_grid)) field_spec%vertical_grid = vertical_grid
      field_spec%vertical_dim_spec = vertical_dim_spec

      if (present(standard_name)) field_spec%standard_name = standard_name
      if (present(long_name)) field_spec%long_name = long_name
 
     if (present(attributes)) field_spec%attributes = attributes

      ! regrid_param

      if (present(default_value)) field_spec%default_value = default_value
   end function new_FieldSpec_geom

   function new_FieldSpec_varspec(variable_spec) result(field_spec)
      type(FieldSpec) :: field_spec
      class(VariableSpec), intent(in) :: variable_spec

      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method

      _SET_FIELD(field_spec, variable_spec, vertical_dim_spec)
      _SET_FIELD(field_spec, variable_spec, attributes)
      _SET_ALLOCATED_FIELD(field_spec, variable_spec, standard_name)
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('UNGRIDDED_DIMS'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('TYPEKIND'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('UNITS'))
      call field_spec%set_aspect(variable_spec%aspects%get_aspect('FREQUENCY'))
!#      _SET_ALLOCATED_FIELD(field_spec, variable_spec, units)
      _SET_ALLOCATED_FIELD(field_spec, variable_spec, default_value)

      field_spec%long_name = 'unknown'

   end function new_FieldSpec_varspec

   subroutine set_geometry(this, geom, vertical_grid, timestep, rc)
      class(FieldSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method
      type(FrequencyAspect), pointer :: frequency_aspect => null()

      call target_set_geom(this, geom)
      if (present(vertical_grid)) this%vertical_grid = vertical_grid
      call target_set_timestep(this, timestep)

      _RETURN(_SUCCESS)
   contains

      ! Helper needed to add target attribute to "this"
      subroutine target_set_geom(this, geom)
         class(FieldSpec), target, intent(inout) :: this
         type(ESMF_Geom), optional, intent(in) :: geom

         type(AspectCollection), pointer :: aspects
         type(GeomAspect), pointer :: geom_aspect
         
         aspects => this%get_aspects()
         geom_aspect => aspects%get_geom_aspect()

         if (associated(geom_aspect)) then
            call geom_aspect%set_geom(geom)
         else
            call aspects%set_geom_aspect(GeomAspect(geom))
         end if
         
      end subroutine target_set_geom

      subroutine target_set_timestep(this, timestep)
         class(FieldSpec), target, intent(inout) :: this
         type(ESMF_TimeInterval), optional, intent(in) :: timestep

         type(AspectCollection), pointer :: aspects
         type(FrequencyAspect), pointer :: frequency_aspect

         if(.not. present(timestep)) return
         aspects => this%get_aspects()
         frequency_aspect => aspects%get_frequency_aspect()

         if (associated(frequency_aspect)) then
            call frequency_aspect%set_timestep(timestep)
            return
         end if
         call aspects%set_frequency_aspect(FrequencySpec(timestep))

      end subroutine target_set_timestep

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

      integer, allocatable :: num_levels_grid
      integer, allocatable :: num_levels
      type(VerticalStaggerLoc) :: vert_staggerloc
      class(StateItemAspect), pointer :: geom_aspect, units_aspect, typekind_aspect, ungridded_dims_aspect
      type(UngriddedDims), pointer :: ungridded_dims
      type(ESMF_TypeKind_Flag) :: typekind
      character(:), allocatable :: units

      _RETURN_UNLESS(this%is_active())

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _RETURN_IF(fstatus == ESMF_FIELDSTATUS_COMPLETE)

      geom_aspect => this%get_aspect('GEOM', _RC)
      select type (geom_aspect)
      class is (GeomAspect)
         call ESMF_FieldEmptySet(this%payload, geom_aspect%geom, _RC)
      class default
         _FAIL('no geom aspect')
      end select

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

      ungridded_dims_aspect => this%get_aspect('UNGRIDDED_DIMS', _RC)
      ungridded_dims => null()
      if (associated(ungridded_dims_aspect)) then
         select type (ungridded_dims_aspect)
         class is (UngriddedDimsAspect)
            if (allocated(ungridded_dims_aspect%ungridded_dims)) then
               ungridded_dims => ungridded_dims_aspect%ungridded_dims
            end if
         class default
            _FAIL('no ungrgeom aspect')
         end select
      end if

      units_aspect => this%get_aspect('UNITS', _RC)
      select type(units_aspect)
      class is (UnitsAspect)
         units = units_aspect%units
      class default
         _FAIL('no units aspect')
      end select

      typekind_aspect => this%get_aspect('TYPEKIND', _RC)
      select type(typekind_aspect)
      class is (TypekindAspect)
         typekind = typekind_aspect%typekind
      class default
         _FAIL('no units aspect')
      end select

      call MAPL_FieldEmptyComplete(this%payload, &
           typekind=typekind, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, &
           vert_staggerLoc=vert_staggerLoc, &
           units=units, &
           standard_name=this%standard_name, &
           long_name=this%long_name, &
           _RC)
      _VERIFY(status)
    
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _ASSERT(fstatus == ESMF_FIELDSTATUS_COMPLETE, 'ESMF field status problem.')

      if (allocated(this%default_value)) then
         call FieldSet(this%payload, this%default_value, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate

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
!#      if (allocated(this%units)) then
!#         write(unit, "(a, a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), "units:", this%units
!#      end if
      write(unit, "(a, dt'g0')", iostat=iostat, iomsg=iomsg) new_line("a"), this%vertical_dim_spec
      if (allocated(this%vertical_grid)) then
         write(unit, "(a, dt'g0', a)", iostat=iostat, iomsg=iomsg) new_line("a"), this%vertical_grid
      end if
      write(unit, "(a)") ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted


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
      class(StateItemAspect), pointer :: geom_aspect, units_aspect, typekind_aspect

      interface mirror
         procedure :: mirror_geom
         procedure :: mirror_vertical_grid
         procedure :: mirror_string
         procedure :: mirror_real
         procedure :: mirror_vertical_dim_spec
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

         geom_aspect => src_spec%get_aspect('GEOM', _RC)
         call this%set_aspect(geom_aspect, _RC)
         units_aspect => src_spec%get_aspect('UNITS', _RC)
         call this%set_aspect(units_aspect, _RC)
         typekind_aspect => src_spec%get_aspect('TYPEKIND', _RC)
         call this%set_aspect(typekind_aspect, _RC)

         call mirror(dst=this%vertical_grid, src=src_spec%vertical_grid)
         call mirror(dst=this%vertical_dim_spec, src=src_spec%vertical_dim_spec)
         call mirror(dst=this%default_value, src=src_spec%default_value)
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

   end subroutine connect_to

   logical function can_connect_to(this, src_spec, rc)

      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      logical :: can_convert_units
      integer :: status
      class(StateItemAspect), pointer :: src_units, dst_units
      type(StringVector), target :: aspect_list
      type(StringVectorIterator) :: aspect_iter


      select type(src_spec)
      class is (FieldSpec)
         aspect_list = src_spec%get_aspect_order(this)
         aspect_iter = aspect_list%ftn_begin()
         associate (e => aspect_list%ftn_end())
           do while (aspect_iter /= e)
              call aspect_iter%next()
              can_connect_to = can_connect_aspect(src_spec, this, aspect_iter%of())
              _RETURN_UNLESS(can_connect_to)
           end do
         end associate

         can_connect_to = all ([ &
              can_match(this%vertical_grid, src_spec%vertical_grid), &
              match(this%vertical_dim_spec, src_spec%vertical_dim_spec), &
              includes(this%attributes, src_spec%attributes) &
              ])
      class default
         can_connect_to = .false.
      end select
      _RETURN(_SUCCESS)

   contains

      logical function can_connect_aspect(src_spec, dst_spec, aspect_name)
         class(StateItemSpec), intent(in) :: src_spec
         class(StateItemSpec), intent(in) :: dst_spec
         character(len=*), intent(in) :: aspect_name

         integer :: status
         class(StateItemAspect), pointer :: src_aspect, dst_aspect

         src_aspect => src_spec%get_aspect(aspect_name)
         if (.not. associated(src_aspect)) then
            can_connect_aspect = .false.
            return
         end if

         dst_aspect => dst_spec%get_aspect(aspect_name)
         if (.not. associated(dst_aspect)) then
            can_connect_aspect = .false.
            return
         end if

         can_connect_aspect = src_aspect%can_connect_to(dst_aspect)

      end function can_connect_aspect

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

      class(ComponentDriver), pointer :: v_in_coupler
      class(ComponentDriver), pointer :: v_out_coupler
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(ESMF_TypeKind_Flag) :: typekind_in, typekind_out
      type(ESMF_Geom) :: geom
      type(ESMF_TypeKind_Flag) :: typekind
      class(StateItemAspect), pointer :: geom_aspect
      class(StateItemAspect), pointer :: units_aspect
      class(StateItemAspect), pointer :: typekind_aspect
      character(:), allocatable :: units
      integer :: status

      select type (spec)
      type is (FieldSpec)
         _ASSERT(spec%vertical_grid%can_connect_to(this%vertical_grid), "cannot connect vertical grids")
         ! TODO: DO WE NEED TO RESTRICT SPEC's VERTICAL GRID TO MODEL?
         ! NOTE: we cannot import ModelVerticalGrid (circular dependency)
         _ASSERT(spec%vertical_grid%get_units() == this%vertical_grid%get_units(), 'units must match')
         ! TODO: Should we add a typekind class variable to VerticalGrid?
         
         geom_aspect => spec%get_aspect('GEOM', _RC)
         select type (geom_aspect)
         class is (GeomAspect)
            geom = geom_aspect%geom
         class default
            _FAIL('no geom aspect')
         end select

         units_aspect => spec%get_aspect('UNITS', _RC)
         select type (units_aspect)
         class is (UnitsAspect)
            units = units_aspect%units
         class default
            _FAIL('no units aspect')
         end select

         typekind_aspect => spec%get_aspect('TYPEKIND', _RC)
         select type (typekind_aspect)
         class is (TypekindAspect)
            typekind = typekind_aspect%typekind
         class default
            _FAIL('no typekind aspect')
         end select

         call spec%vertical_grid%get_coordinate_field( &
              v_in_coord, v_in_coupler, & ! output
              'ignore', geom, typekind, this%vertical_grid%get_units(), spec%vertical_dim_spec, _RC)
         call this%vertical_grid%get_coordinate_field( &
              v_out_coord, v_out_coupler, & ! output
              'ignore', geom, typekind, units, this%vertical_dim_spec, _RC)
         action = VerticalRegridAction(v_in_coord, v_out_coupler, v_out_coord, v_out_coupler, this%regrid_method)
         if (allocated(spec%vertical_grid)) deallocate(spec%vertical_grid)
         allocate(spec%vertical_grid, source=this%vertical_grid)
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
         match = spec%vertical_grid%is_identical_to(this%vertical_grid)
      end select

      _RETURN(_SUCCESS)
   end function adapter_match_vertical_grid


   recursive function make_adapters(this, goal_spec, rc) result(adapters)
      type(StateItemAdapterWrapper), allocatable :: adapters(:)
      class(FieldSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(VerticalGridAdapter) :: vertical_grid_adapter
      class(StateItemAspect), pointer :: geom_aspect, units_aspect, typekind_aspect
      type(ESMF_Geom) :: geom
      type(ESMF_Typekind_Flag) :: typekind
      character(:), allocatable :: units
      integer :: status

      select type (goal_spec)
      type is (FieldSpec)
         ! TODO - convert remaining adapters to aspects
         allocate(adapters(1))

         geom_aspect => goal_spec%get_aspect('GEOM', _RC)
         select type (geom_aspect)
         class is (GeomAspect)
            if (allocated(geom_aspect%geom)) then
               geom = geom_aspect%geom
            end if
         class default
            _FAIL('no geom aspect')
         end select

         units_aspect => goal_spec%get_aspect('UNITS', _RC)
         _ASSERT(associated(units_aspect), 'no units aspect')
         select type (units_aspect)
         class is (UnitsAspect)
            if (allocated(units_aspect%units)) then
               units = units_aspect%units
            end if
         class default
            _FAIL('no units aspect')
         end select

         typekind_aspect => goal_spec%get_aspect('TYPEKIND', _RC)
         _ASSERT(associated(typekind_aspect), 'no typekind aspect')
         select type (typekind_aspect)
         class is (TypekindAspect)
            typekind = typekind_aspect%typekind
         class default
            _FAIL('no typekind aspect')
         end select

         vertical_grid_adapter = VerticalGridAdapter( &
              goal_spec%vertical_grid, &
              geom, &
              typekind, &
              units, &
              goal_spec%vertical_dim_spec, &
              VERTICAL_REGRID_LINEAR)
         allocate(adapters(1)%adapter, source=vertical_grid_adapter)
      type is (WildCardSpec)
         adapters = goal_spec%make_adapters(goal_spec, _RC)
      class default
         allocate(adapters(0))
         _FAIL('unsupported subclass of StateItemSpec')
      end select

      _RETURN(_SUCCESS)
   end function make_adapters

   function get_aspect_priorities(src_spec, dst_spec) result(order)
      character(:), allocatable :: order
      class(FieldSpec), intent(in) :: src_spec
      class(StateItemSpec), intent(in) :: dst_spec

      order = 'UNGRIDDED_DIMS::GEOM::UNITS::TYPEKIND'
   end function get_aspect_priorities
   

end module mapl3g_FieldSpec

#undef _SET_FIELD
#undef _SET_ALLOCATED_FIELD
