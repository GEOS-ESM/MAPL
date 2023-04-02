#include "MAPL_Generic.h"

module mapl3g_FieldSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_UngriddedDimsSpec
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   use nuopc

   implicit none
   private

   public :: FieldSpec
   public :: new_FieldSpec_geom

   type, extends(AbstractStateItemSpec) :: FieldSpec
      private

      type(ESMF_Geom) :: geom
      type(ESMF_typekind_flag) :: typekind = ESMF_TYPEKIND_R4
      type(UngriddedDimsSpec) :: ungridded_dims

      ! Metadata
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
      character(:), allocatable :: units
      ! TBD
!!$      type(FrequencySpec) :: freq_spec
!!$      class(AbstractFrequencySpec), allocatable :: freq_spec
!!$      integer :: halo_width = 0

      type(ESMF_Field) :: payload
      real, allocatable :: default_value

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: make_extension
      procedure :: add_to_state

      procedure :: check_complete
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geom
!!$      module procedure new_FieldSpec_defaults
   end interface FieldSpec

contains


   function new_FieldSpec_geom(geom, typekind, ungridded_dims, &
        standard_name, long_name, units, &
        default_value) result(field_spec)
      type(FieldSpec) :: field_spec

      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(UngriddedDimsSpec), intent(in) :: ungridded_dims

      character(*), intent(in) :: standard_name
      character(*), intent(in) :: long_name
      character(*), intent(in) :: units
      real, optional, intent(in) :: default_value

      field_spec%geom = geom
      field_spec%typekind = typekind
      field_spec%ungridded_dims = ungridded_dims

      field_spec%units = standard_name
      field_spec%units = long_name
      field_spec%units = units

      if (present(default_value)) field_spec%default_value = default_value

   end function new_FieldSpec_geom


!!$   function new_FieldSpec_defaults(ungridded_dims, geom, units) result(field_spec)
!!$      type(FieldSpec) :: field_spec
!!$      type(ExtraDimsSpec), intent(in) :: ungridded_dims
!!$      type(ESMF_Geom), intent(in) :: geom
!!$      character(*), intent(in) :: units
!!$      
!!$      field_spec = FieldSpec(ungridded_dims, ESMF_TYPEKIND_R4, geom, units)
!!$      
!!$   end function new_FieldSpec_defaults
!!$

   subroutine create(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      call MAPL_FieldEmptySet(this%payload, this%geom, _RC)

      call this%set_created()

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
      if (fstatus == ESMF_FIELDSTATUS_GRIDSET) then

         call ESMF_FieldEmptyComplete(this%payload, this%typekind, &
              ungriddedLBound= this%ungridded_dims%get_lbounds(),  &
              ungriddedUBound= this%ungridded_dims%get_ubounds(),  &
              _RC)
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      _ASSERT(fstatus == ESMF_FIELDSTATUS_COMPLETE, 'ESMF field status problem.')

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
              this%ungridded_dims == src_spec%ungridded_dims &
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
      call ESMF_GeomGet(this%geom, geomtype=geom_type, rc=status)
      if (status /= 0) return

      select type(src_spec)
      class is (FieldSpec)
         requires_extension = any([ &
              this%ungridded_dims /= src_spec%ungridded_dims, &
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

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(FieldSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus
      type(ESMF_State) :: state, substate
      character(:), allocatable :: short_name

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)
      call get_substate(state, actual_pt%get_comp_name(), substate=substate, _RC)

      short_name = actual_pt%get_esmf_name()
      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function make_extension(this, src_spec, rc) result(action_spec)
      class(AbstractActionSpec), allocatable :: action_spec
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc 
   end function make_extension

   logical function check_complete(this, rc)
      class(FieldSpec), intent(in) :: this
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      check_complete = (fstatus == ESMF_FIELDSTATUS_COMPLETE)

   end function check_complete

end module mapl3g_FieldSpec
