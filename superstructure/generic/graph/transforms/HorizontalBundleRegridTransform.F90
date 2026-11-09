#include "MAPL.h"

!------------------------------------------------------------------------------
! HorizontalBundleRegridTransform: regrids a field bundle (VECTOR or BRACKET)
! from the input bundle's geom to the output bundle's geom, for one regridder
! param. The bundle is regridded by one call, so the two components of a
! vector are rotated together.
!
! The output bundle's members are created at compute, by cloning each input
! member onto the output bundle's geom, and are created again when the input
! member count differs from the output's (the member count is not known when
! the chain is built, and ExtData changes it at run time).
!
! A vector keeps the basis of the input bundle: the output bundle takes it,
! and the two bases are never compared. A bracket-typed bundle also carries
! its interpolation weights to the output, where the time interpolation step
! reads them, and is regridded only when its bracket_updated flag is set or
! the output holds no result yet.
!
! The regridder is looked up in RegridderManager at the first compute and
! looked up again whenever either geom or typekind changes, as
! HorizontalRegridTransform does. Both geoms must come from the geom manager
! (carry a MAPL geom id); compute fails otherwise.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_HorizontalBundleRegridTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_EsmfRegridder_mod, only: EsmfRegridderParam
   use mapl_Regridder_mod, only: Regridder
   use mapl_RegridderSpec_mod, only: RegridderSpec
   use mapl_RegridderManager_mod, only: RegridderManager, get_regridder_manager
   use mapl_geom_api, only: mapl_SameGeom, mapl_GeomGetId
   use mapl_GeomId_mod, only: GeomId
   use mapl_field_bundle_api, only: MAPL_FieldBundleGet, MAPL_FieldBundleSet
   use mapl_esmf_info_keys_mod, only: INFO_INTERNAL_NAMESPACE, &
        KEY_INTERPOLATION_WEIGHTS
   use mapl_enums_api, only: MAPL_FieldBundleType_Flag, &
        MAPL_FIELDBUNDLETYPE_BRACKET, MAPL_FIELDBUNDLETYPE_VECTOR, &
        MAPL_VectorBasisKind, &
        operator(==)
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_Geom, ESMF_Info
   use ESMF, only: ESMF_FieldGet, ESMF_FieldCreate, ESMF_GeomGet
   use ESMF, only: ESMF_FieldBundleGet, ESMF_FieldBundleAdd
   use ESMF, only: ESMF_FieldBundleRemove, ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoUpdate, ESMF_InfoIsPresent, ESMF_MAXSTR
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_KIND_R4
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELDBUNDLE
   use ESMF, only: operator(==), operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: HorizontalBundleRegridTransform

   type, extends(Transform) :: HorizontalBundleRegridTransform
      private
      type(EsmfRegridderParam) :: regridder_param
      type(ESMF_Geom) :: src_geom
      type(ESMF_Geom) :: dst_geom
      type(ESMF_TypeKind_Flag) :: typekind_in
      type(ESMF_TypeKind_Flag) :: typekind_out
      class(Regridder), pointer :: regrdr => null()
      ! False until the output has been regridded by the current regridder
      ! into its current members.
      logical :: has_result = .false.
   contains
      procedure :: compute => bundle_regrid_compute
      procedure :: get_regridder_param
      procedure, private :: update_regridder_for
   end type HorizontalBundleRegridTransform

   interface HorizontalBundleRegridTransform
      module procedure new_HorizontalBundleRegridTransform
   end interface HorizontalBundleRegridTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_HorizontalBundleRegridTransform(regridder_param) &
        result(transform)
      type(EsmfRegridderParam), intent(in) :: regridder_param
      type(HorizontalBundleRegridTransform) :: transform

      integer :: status

      transform%regridder_param = regridder_param

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_HorizontalBundleRegridTransform

   subroutine bundle_regrid_compute(this, inputs, outputs, rc)
      class(HorizontalBundleRegridTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_FieldBundle) :: fb_in, fb_out
      type(ESMF_Field), allocatable :: members_in(:)
      type(ESMF_Geom), allocatable :: geom_in, geom_out
      type(ESMF_TypeKind_Flag) :: typekind_in, typekind_out
      type(MAPL_FieldBundleType_Flag) :: bundle_type
      logical :: updated
      integer :: n_out

      call get_port_bundle(inputs, INPUT_PORT_NAME, fb_in, _RC)
      call get_port_bundle(outputs, OUTPUT_PORT_NAME, fb_out, _RC)

      call MAPL_FieldBundleGet(fb_in, geom=geom_in, typekind=typekind_in, &
           fieldBundleType=bundle_type, fieldList=members_in, _RC)
      call MAPL_FieldBundleGet(fb_out, geom=geom_out, typekind=typekind_out, &
           _RC)
      call check_geom(geom_in, INPUT_PORT_NAME, _RC)
      call check_geom(geom_out, OUTPUT_PORT_NAME, _RC)

      call this%update_regridder_for(geom_in, geom_out, typekind_in, &
           typekind_out, _RC)

      call ESMF_FieldBundleGet(fb_out, fieldCount=n_out, _RC)
      if (n_out /= size(members_in)) then
         call recreate_members(fb_out, members_in, geom_out, typekind_out, _RC)
         this%has_result = .false.
      end if

      if (bundle_type == MAPL_FIELDBUNDLETYPE_VECTOR) then
         call copy_vector_basis(fb_in, fb_out, _RC)
      end if

      updated = .true.
      if (bundle_type == MAPL_FIELDBUNDLETYPE_BRACKET) then
         call copy_interpolation_weights(fb_in, fb_out, _RC)
         call MAPL_FieldBundleGet(fb_in, bracket_updated=updated, _RC)
      end if

      if (updated .or. .not. this%has_result) then
         call this%regrdr%regrid(fb_in, fb_out, _RC)
         this%has_result = .true.
      end if

      _RETURN(_SUCCESS)
   end subroutine bundle_regrid_compute

   ! Looks up the regridder on first use and again when a geom or typekind
   ! no longer matches the one the regridder was made for. Geoms are compared
   ! by MAPL geom id, as RegridderManager does.
   subroutine update_regridder_for(this, geom_in, geom_out, typekind_in, &
        typekind_out, rc)
      class(HorizontalBundleRegridTransform), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom_in, geom_out
      type(ESMF_TypeKind_Flag), intent(in) :: typekind_in, typekind_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      class(Regridder), pointer :: new_regrdr
      logical :: changed

      changed = .not. associated(this%regrdr)
      if (.not. changed) then
         changed = .not. mapl_SameGeom(geom_in, this%src_geom) &
              .or. .not. mapl_SameGeom(geom_out, this%dst_geom) &
              .or. (typekind_in /= this%typekind_in) &
              .or. (typekind_out /= this%typekind_out)
      end if
      _RETURN_UNLESS(changed)

      ! Cache nothing until the lookup succeeds, so a failed lookup cannot
      ! leave the old regridder paired with the new geoms.
      regridder_manager => get_regridder_manager()
      spec = RegridderSpec(this%regridder_param, geom_in, geom_out, &
           typekind_in=typekind_in, typekind_out=typekind_out)
      new_regrdr => regridder_manager%get_regridder(spec, _RC)

      this%regrdr => new_regrdr
      this%src_geom = geom_in
      this%dst_geom = geom_out
      this%typekind_in = typekind_in
      this%typekind_out = typekind_out
      this%has_result = .false.

      _RETURN(_SUCCESS)
   end subroutine update_regridder_for

   subroutine get_port_bundle(items, port_name, bundle, rc)
      type(PortNameGraphStateItemMap), intent(in) :: items
      character(*), intent(in) :: port_name
      type(ESMF_FieldBundle), intent(out) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: item
      type(ESMF_StateItem_Flag) :: item_type
      character(:), allocatable :: msg

      item => items%at(port_name)
      msg = 'HorizontalBundleRegridTransform: port not bound: ' // port_name
      _ASSERT(associated(item), msg)
      item_type = item%itemType(_RC)
      if (.not. (item_type == ESMF_STATEITEM_FIELDBUNDLE)) then
         msg = 'HorizontalBundleRegridTransform: port is not a field ' // &
              'bundle: ' // port_name
         _FAIL(msg)
      end if
      bundle = item%get_field_bundle(_RC)

      _RETURN(_SUCCESS)
   end subroutine get_port_bundle

   ! Without an id every compute would look as if the geom changed, and each
   ! lookup would add another regridder to RegridderManager.
   subroutine check_geom(geom, port_name, rc)
      type(ESMF_Geom), allocatable, intent(in) :: geom
      character(*), intent(in) :: port_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomId) :: id
      logical :: has_id
      character(:), allocatable :: msg

      msg = 'HorizontalBundleRegridTransform: bundle has no geom on port: ' &
           // port_name
      _ASSERT(allocated(geom), msg)
      id = mapl_GeomGetId(geom, isPresent=has_id, _RC)
      msg = 'HorizontalBundleRegridTransform: geom has no MAPL geom id ' // &
           '(not from the geom manager) on port: ' // port_name
      _ASSERT(has_id, msg)

      _RETURN(_SUCCESS)
   end subroutine check_geom

   ! Replaces the output's members by one clone of each input member on
   ! geom_out. The old members are removed from the output bundle and are
   ! left undestroyed.
   subroutine recreate_members(fb_out, members_in, geom_out, typekind_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_out
      type(ESMF_Field), intent(inout) :: members_in(:)
      type(ESMF_Geom), intent(in) :: geom_out
      type(ESMF_TypeKind_Flag), intent(in) :: typekind_out
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n_old
      character(len=ESMF_MAXSTR), allocatable :: old_names(:)
      type(ESMF_Field), allocatable :: new_members(:)

      call ESMF_FieldBundleGet(fb_out, fieldCount=n_old, _RC)
      if (n_old > 0) then
         allocate(old_names(n_old))
         call ESMF_FieldBundleGet(fb_out, fieldNameList=old_names, _RC)
         call ESMF_FieldBundleRemove(fb_out, old_names, multiflag=.true., _RC)
      end if

      allocate(new_members(size(members_in)))
      do i = 1, size(members_in)
         call clone_onto(members_in(i), geom_out, typekind_out, &
              new_members(i), _RC)
      end do
      if (size(new_members) > 0) then
         call ESMF_FieldBundleAdd(fb_out, new_members, multiFlag=.true., &
              relaxedFlag=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine recreate_members

   ! A field like `archetype` (name, shape beyond the horizontal, metadata)
   ! on another geom, with its own allocation.
   subroutine clone_onto(archetype, geom, typekind, clone, rc)
      type(ESMF_Field), intent(inout) :: archetype
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Field), intent(out) :: clone
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: field_rank, geom_rank
      integer, allocatable :: grid_to_field_map(:)
      integer, allocatable :: ungridded_lbound(:), ungridded_ubound(:)
      character(len=ESMF_MAXSTR) :: name
      type(ESMF_Info) :: info_in, info_out

      call ESMF_FieldGet(archetype, rank=field_rank, name=name, _RC)
      call ESMF_GeomGet(geom, dimCount=geom_rank, _RC)
      allocate(grid_to_field_map(geom_rank))
      allocate(ungridded_lbound(field_rank - geom_rank))
      allocate(ungridded_ubound(field_rank - geom_rank))
      call ESMF_FieldGet(archetype, gridToFieldMap=grid_to_field_map, &
           ungriddedLBound=ungridded_lbound, &
           ungriddedUBound=ungridded_ubound, _RC)

      clone = ESMF_FieldCreate(geom, typekind=typekind, &
           gridToFieldMap=grid_to_field_map, &
           ungriddedLBound=ungridded_lbound, &
           ungriddedUBound=ungridded_ubound, name=trim(name), _RC)

      call ESMF_InfoGetFromHost(archetype, info_in, _RC)
      call ESMF_InfoGetFromHost(clone, info_out, _RC)
      call ESMF_InfoUpdate(info_out, info_in, recursive=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine clone_onto

   subroutine copy_vector_basis(fb_in, fb_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(MAPL_VectorBasisKind) :: basis_kind

      call MAPL_FieldBundleGet(fb_in, vector_basis_kind=basis_kind, _RC)
      call MAPL_FieldBundleSet(fb_out, vector_basis_kind=basis_kind, _RC)

      _RETURN(_SUCCESS)
   end subroutine copy_vector_basis

   subroutine copy_interpolation_weights(fb_in, fb_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(ESMF_KIND_R4), allocatable :: weights(:)
      type(ESMF_Info) :: info
      logical :: has_weights

      ! A bracket that has no weights yet has nothing to carry over.
      call ESMF_InfoGetFromHost(fb_in, info, _RC)
      has_weights = ESMF_InfoIsPresent(info, &
           key=INFO_INTERNAL_NAMESPACE // KEY_INTERPOLATION_WEIGHTS)
      _RETURN_UNLESS(has_weights)

      call MAPL_FieldBundleGet(fb_in, interpolation_weights=weights, _RC)
      call MAPL_FieldBundleSet(fb_out, interpolation_weights=weights, _RC)

      _RETURN(_SUCCESS)
   end subroutine copy_interpolation_weights

   function get_regridder_param(this) result(regridder_param)
      class(HorizontalBundleRegridTransform), intent(in) :: this
      type(EsmfRegridderParam) :: regridder_param

      regridder_param = this%regridder_param
   end function get_regridder_param

end module mapl_HorizontalBundleRegridTransform_mod
