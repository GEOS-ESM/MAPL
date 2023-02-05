#include "MAPL_Generic.h"

module mapl3g_FieldSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_ExtraDimsSpec
   use mapl3g_VariableSpec
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   use nuopc

   implicit none
   private

   public :: FieldSpec

   type, extends(AbstractStateItemSpec) :: FieldSpec
      private

      character(:), allocatable :: units
      type(ESMF_typekind_flag) :: typekind
      type(ESMF_GeomBase) :: geom_base
      type(ExtraDimsSpec) :: extra_dims
!!$      type(FrequencySpec) :: freq_spec
!!$      class(AbstractFrequencySpec), allocatable :: freq_spec
      integer :: halo_width = 0

      type(ESMF_Field) :: payload

   contains
      procedure :: initialize
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: make_extension
      procedure :: add_to_state
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geombase
      module procedure new_FieldSpec_defaults
   end interface FieldSpec

contains

   subroutine initialize(this, geom_base, var_spec, unusable, rc)
      class(FieldSpec), intent(inout) :: this
      type(ESMF_GeomBase), intent(in) :: geom_base
      type(VariableSpec), intent(in) :: var_spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(:), allocatable :: units
      integer :: status

      this%geom_base = geom_base
!!$      this%extra_dims = var_spec%extra_dims
!!$      this%typekind = var_spec%typekind

      call get_units(units, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

      subroutine get_units(units, rc)
         character(:), intent(out), allocatable :: units
         integer, optional, intent(out) :: rc

         character(ESMF_MAXSTR) :: esmf_units
         integer :: status
         
         if (allocated(var_spec%units)) units = var_spec%units ! user override

         if (.not. allocated(units)) then
            call NUOPC_FieldDictionaryGetEntry(var_spec%standard_name, esmf_units, status)
            _ASSERT(status == ESMF_SUCCESS,'Units not found for standard name: <'//var_spec%standard_name//'>')
            units = trim(esmf_units)
         end if

         _RETURN(_SUCCESS)
      end subroutine get_units
      
   end subroutine initialize


   function new_FieldSpec_geombase(extra_dims, typekind, geom_base, units) result(field_spec)
      type(FieldSpec) :: field_spec
      type(ExtraDimsSpec), intent(in) :: extra_dims
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(ESMF_GeomBase), intent(in) :: geom_base
      character(*), intent(in) :: units

      field_spec%extra_dims = extra_dims
      field_spec%typekind = typekind
      field_spec%geom_base = geom_base
      field_spec%units = units
   end function new_FieldSpec_geombase


   function new_FieldSpec_defaults(extra_dims, geom_base, units) result(field_spec)
      type(FieldSpec) :: field_spec
      type(ExtraDimsSpec), intent(in) :: extra_dims
      type(ESMF_GeomBase), intent(in) :: geom_base
      character(*), intent(in) :: units
      
      field_spec = FieldSpec(extra_dims, ESMF_TYPEKIND_R4, geom_base, units)
      
   end function new_FieldSpec_defaults


   subroutine create(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      
      this%payload = ESMF_FieldEmptyCreate(_RC)
      call MAPL_FieldEmptySet(this%payload, this%geom_base, _RC)

      call this%set_created()

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine MAPL_FieldEmptySet(field, geom_base, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_GeomBase), intent(inout) :: geom_base
      integer, optional, intent(out) ::rc

      type(ESMF_GeomType_Flag) :: geom_type
      type(ESMF_Grid) :: grid
      type(ESMF_Mesh) :: mesh
      type(ESMF_XGrid) :: xgrid
      type(ESMF_LocStream) :: locstream
      integer :: status

      call ESMF_GeomBaseGet(geom_base, geomtype=geom_type, _RC)

      if(geom_type == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomBaseGet(geom_base, grid=grid, _RC)
         call ESMF_FieldEmptySet(field, grid, _RC)
      else if (geom_type == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomBaseGet(geom_base, mesh=mesh, _RC)
         call ESMF_FieldEmptySet(field, mesh, _RC)
      else if (geom_type == ESMF_GEOMTYPE_XGRID) then
         call ESMF_GeomBaseGet(geom_base, xgrid=xgrid, _RC)
         call ESMF_FieldEmptySet(field, xgrid, _RC)
      else if (geom_type == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomBaseGet(geom_base, locstream=locstream, _RC)
         call ESMF_FieldEmptySet(field, locstream, _RC)
      else
         _FAIL('Unsupported type of GeomBase')
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_FieldEmptySet

   subroutine destroy(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus
      
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      if (fstatus == ESMF_FIELDSTATUS_EMPTY) then

         call ESMF_FieldEmptyComplete(this%payload, this%typekind, &
              ungriddedLBound= this%extra_dims%get_lbounds(),  &
              ungriddedUBound= this%extra_dims%get_ubounds(),  &
              _RC)

         call this%set_allocated()
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


   subroutine connect_to(this, src_spec, rc)
      class(FieldSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (FieldSpec)
         ! ok
         this%payload = src_spec%payload
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)

   end subroutine connect_to


   logical function can_connect_to(this, src_spec)
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (FieldSpec)
         can_connect_to = all ([ &
              this%typekind == src_spec%typekind,   &
              this%extra_dims == src_spec%extra_dims &
!!$              this%freq_spec == src_spec%freq_spec,   &
!!$              this%halo_width == src_spec%halo_width,  &
!!$              this%vm == sourc%vm, &
!!$              can_convert_units(this, src_spec) &
              ])
      class default
         can_connect_to = .false.
      end select

   end function can_connect_to


   logical function requires_extension(this, src_spec)
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      type(ESMF_GeomType_Flag) :: geom_type
      integer :: status
      
      requires_extension = .true.
      call ESMF_GeomBaseGet(this%geom_base, geomtype=geom_type, rc=status)
      if (status /= 0) return

      select type(src_spec)
      class is (FieldSpec)
         requires_extension = any([ &
              this%extra_dims /= src_spec%extra_dims, &
              this%typekind /= src_spec%typekind,   &
!!$              this%freq_spec /= src_spec%freq_spec,   &
!!$              this%units /= src_spec%units,           &
!!$              this%halo_width /= src_spec%halo_width, &
!!$              this%vm /= sourc%vm,               &
              geom_type /= geom_type &
              ])
!!$         requires_extension = .false.
      end select
   end function requires_extension

   logical function same_typekind(a, b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b
      same_typekind = (a%typekind == b%typekind)
   end function same_typekind

   ! Eventually we will integrate UDunits, but for now
   ! we require units to exactly match when connecting
   ! fields.
   logical function can_convert_units(a,b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b

      can_convert_units = a%units == b%units

   end function can_convert_units

   subroutine add_to_state(this, state, short_name, rc)
      class(FieldSpec), intent(in) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status

      _FAIL('unimplemented')

!!$      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
!!$      call ESMF_StateAdd(state, this%payload, short_name, _RC)
!!$

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function make_extension(this, src_spec, rc) result(action_spec)
      class(AbstractActionSpec), allocatable :: action_spec
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc 
   end function make_extension

end module mapl3g_FieldSpec
