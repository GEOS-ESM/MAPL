#include "MAPL.h"

!------------------------------------------------------------------------------
! WeightedDeltaBackTransform: returns the change an Invoker made to a
! callback argument on its grid T to the owner field X on grid O, so that
! the owner budget closes.
!
! Input ports:
!   value          c,  on T: the Invoker's result
!   snapshot       c0, on T: the value the Invoker was given
!   weight_invoker mT, on T: the weight the Invoker was given (weighted only)
!   weight_owner   mO, on O: the owner's own weight (weighted only)
!   owner          X,  on O: the owner field before the put
! Output port:
!   owner          X + dO, on O
!
! Weighted:   dO = R(mT (c - c0)) / mO
! Unweighted: dO = R(c - c0)
! R is the horizontal regridder named by the regridder param, first-order
! conservative by default, so that R preserves the area integral of
! mT (c - c0). When T and O are the same geom, R is the identity.
!
! All port fields share one typekind (R4 or R8); fields on the same geom
! share one shape. mO must be on the owner geom: a weight on another geom
! does not close the owner budget, so it is rejected.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_WeightedDeltaBackTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_PortSpecMap_mod, only: PortSpecMap
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use mapl_FieldPointerUtilities_mod, only: FieldClone
   use mapl_EsmfRegridder_mod, only: EsmfRegridderParam
   use mapl_Regridder_mod, only: Regridder
   use mapl_RegridderSpec_mod, only: RegridderSpec
   use mapl_RegridderManager_mod, only: RegridderManager, get_regridder_manager
   use mapl_geom_api, only: mapl_SameGeom, mapl_GeomGetId
   use mapl_GeomId_mod, only: GeomId
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldDestroy, ESMF_Geom
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use ESMF, only: ESMF_REGRIDMETHOD_CONSERVE
   use ESMF, only: operator(==), operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: WeightedDeltaBackTransform

   type, extends(Transform) :: WeightedDeltaBackTransform
      private
      logical :: weighted = .false.
      type(EsmfRegridderParam) :: regridder_param
      type(ESMF_Geom) :: src_geom
      type(ESMF_Geom) :: dst_geom
      type(ESMF_TypeKind_Flag) :: typekind
      class(Regridder), pointer :: regrdr => null()
   contains
      procedure :: compute => delta_back_compute
      procedure :: is_input_port => delta_back_is_input_port
      procedure :: get_input_port => delta_back_get_input_port
      procedure :: get_input_ports => delta_back_get_input_ports
      procedure :: is_weighted
      procedure, private :: update_regridder_for
      procedure, private :: input_port_names
   end type WeightedDeltaBackTransform

   interface WeightedDeltaBackTransform
      module procedure new_WeightedDeltaBackTransform
   end interface WeightedDeltaBackTransform

   character(*), parameter :: VALUE_PORT = 'value'
   character(*), parameter :: SNAPSHOT_PORT = 'snapshot'
   character(*), parameter :: WEIGHT_INVOKER_PORT = 'weight_invoker'
   character(*), parameter :: WEIGHT_OWNER_PORT = 'weight_owner'
   character(*), parameter :: OWNER_PORT = 'owner'
   integer, parameter :: MAX_PORT_NAME_LEN = len(WEIGHT_INVOKER_PORT)

contains

   ! `weighted` selects the weighted form and declares the two weight
   ! ports. The default regridder is first-order conservative, the one a
   ! callback argument uses on its get path.
   function new_WeightedDeltaBackTransform(weighted, regridder_param) result(transform)
      logical, intent(in) :: weighted
      type(EsmfRegridderParam), optional, intent(in) :: regridder_param
      type(WeightedDeltaBackTransform) :: transform

      integer :: status

      transform%weighted = weighted
      if (present(regridder_param)) then
         transform%regridder_param = regridder_param
      else
         transform%regridder_param = EsmfRegridderParam(regridmethod=ESMF_REGRIDMETHOD_CONSERVE)
      end if

      ! Input ports are answered from a fixed name list, not declared in the
      ! base port map: gfortran's polymorphic copy of a TransformGraphNode
      ! leaves a multi-entry gFTL map with dangling internal pointers.
      call transform%declare_output_port(OWNER_PORT, rc=status)
   end function new_WeightedDeltaBackTransform

   logical function is_weighted(this)
      class(WeightedDeltaBackTransform), intent(in) :: this
      is_weighted = this%weighted
   end function is_weighted

   function input_port_names(this) result(names)
      class(WeightedDeltaBackTransform), intent(in) :: this
      character(len=MAX_PORT_NAME_LEN), allocatable :: names(:)

      if (this%weighted) then
         names = [character(len=MAX_PORT_NAME_LEN) :: VALUE_PORT, SNAPSHOT_PORT, &
              WEIGHT_INVOKER_PORT, WEIGHT_OWNER_PORT, OWNER_PORT]
      else
         names = [character(len=MAX_PORT_NAME_LEN) :: VALUE_PORT, SNAPSHOT_PORT, OWNER_PORT]
      end if
   end function input_port_names

   logical function delta_back_is_input_port(this, name) result(is_port)
      class(WeightedDeltaBackTransform), intent(in) :: this
      character(*), intent(in) :: name

      is_port = any(this%input_port_names() == name)
   end function delta_back_is_input_port

   function delta_back_get_input_port(this, name, rc) result(spec)
      class(WeightedDeltaBackTransform), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      spec = PortSpec(name)
      _ASSERT(this%is_input_port(name), 'WeightedDeltaBackTransform: input port not declared')
      _RETURN(_SUCCESS)
   end function delta_back_get_input_port

   function delta_back_get_input_ports(this) result(ports)
      class(WeightedDeltaBackTransform), intent(in) :: this
      type(PortSpecMap) :: ports

      character(len=MAX_PORT_NAME_LEN), allocatable :: names(:)
      integer :: i

      names = this%input_port_names()
      do i = 1, size(names)
         call ports%insert(trim(names(i)), PortSpec(trim(names(i))))
      end do
   end function delta_back_get_input_ports

   subroutine delta_back_compute(this, inputs, outputs, rc)
      class(WeightedDeltaBackTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status, destroy_status
      type(ESMF_Field) :: f_value, f_snapshot, f_weight_invoker, f_weight_owner
      type(ESMF_Field) :: f_owner_in, f_owner_out
      type(ESMF_Field) :: source_on_t
      type(ESMF_Geom) :: geom_t, geom_o
      type(ESMF_TypeKind_Flag) :: typekind
      integer :: shape_t(3), shape_o(3)

      call get_port_field(inputs, VALUE_PORT, f_value, _RC)
      call get_port_field(inputs, SNAPSHOT_PORT, f_snapshot, _RC)
      call get_port_field(inputs, OWNER_PORT, f_owner_in, _RC)
      call get_port_field(outputs, OWNER_PORT, f_owner_out, _RC)
      if (this%weighted) then
         call get_port_field(inputs, WEIGHT_INVOKER_PORT, f_weight_invoker, _RC)
         call get_port_field(inputs, WEIGHT_OWNER_PORT, f_weight_owner, _RC)
      end if

      call ESMF_FieldGet(f_value, geom=geom_t, typekind=typekind, _RC)
      call ESMF_FieldGet(f_owner_in, geom=geom_o, _RC)
      _ASSERT(typekind == ESMF_TYPEKIND_R4 .or. typekind == ESMF_TYPEKIND_R8, 'WeightedDeltaBackTransform: unsupported typekind')

      ! Every check that can fail runs before the temporaries are made.
      shape_t = condensed_shape(f_value, _RC)
      shape_o = condensed_shape(f_owner_in, _RC)
      call check_port(f_value, VALUE_PORT, geom_t, typekind, shape_t, _RC)
      call check_port(f_snapshot, SNAPSHOT_PORT, geom_t, typekind, shape_t, _RC)
      call check_port(f_owner_in, OWNER_PORT, geom_o, typekind, shape_o, _RC)
      call check_port(f_owner_out, OWNER_PORT, geom_o, typekind, shape_o, _RC)
      if (this%weighted) then
         call check_port(f_weight_invoker, WEIGHT_INVOKER_PORT, geom_t, typekind, shape_t, _RC)
         call check_port(f_weight_owner, WEIGHT_OWNER_PORT, geom_o, typekind, shape_o, _RC)
      end if
      _ASSERT(all(shape_t(2:3) == shape_o(2:3)), 'WeightedDeltaBackTransform: value and owner differ in vertical or ungridded size')

      ! The temporary is destroyed on every exit path, so it is not leaked
      ! when the regrid fails.
      call FieldClone(f_value, source_on_t, name='weighted_delta_back_source', _RC)
      if (this%weighted) then
         call deliver_delta(this, f_value, f_snapshot, f_owner_in, f_owner_out, &
              source_on_t, geom_t, geom_o, typekind, &
              weight_invoker=f_weight_invoker, weight_owner=f_weight_owner, rc=status)
      else
         call deliver_delta(this, f_value, f_snapshot, f_owner_in, f_owner_out, &
              source_on_t, geom_t, geom_o, typekind, rc=status)
      end if
      call ESMF_FieldDestroy(source_on_t, noGarbage=.true., rc=destroy_status)
      _VERIFY(status)
      _VERIFY(destroy_status)

      _RETURN(_SUCCESS)
   end subroutine delta_back_compute

   ! Forms the source on T in source_on_t, regrids it to O unless the geoms
   ! are the same, and adds the result to the owner.
   subroutine deliver_delta(this, f_value, f_snapshot, f_owner_in, f_owner_out, &
        source_on_t, geom_t, geom_o, typekind, weight_invoker, weight_owner, rc)
      class(WeightedDeltaBackTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: f_value, f_snapshot, f_owner_in, f_owner_out
      type(ESMF_Field), intent(inout) :: source_on_t
      type(ESMF_Geom), intent(in) :: geom_t, geom_o
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Field), optional, intent(inout) :: weight_invoker, weight_owner
      integer, optional, intent(out) :: rc

      integer :: status, destroy_status
      type(ESMF_Field) :: source_on_o

      call form_source(f_value, f_snapshot, source_on_t, typekind, weight=weight_invoker, _RC)

      if (mapl_SameGeom(geom_t, geom_o)) then
         call add_delta(source_on_t, f_owner_in, f_owner_out, typekind, weight=weight_owner, _RC)
         _RETURN(_SUCCESS)
      end if

      call FieldClone(f_owner_in, source_on_o, name='weighted_delta_back_regridded', _RC)
      call regrid_and_add(this, source_on_t, source_on_o, f_owner_in, f_owner_out, &
           geom_t, geom_o, typekind, weight_owner=weight_owner, rc=status)
      call ESMF_FieldDestroy(source_on_o, noGarbage=.true., rc=destroy_status)
      _VERIFY(status)
      _VERIFY(destroy_status)

      _RETURN(_SUCCESS)
   end subroutine deliver_delta

   subroutine regrid_and_add(this, source_on_t, source_on_o, f_owner_in, f_owner_out, &
        geom_t, geom_o, typekind, weight_owner, rc)
      class(WeightedDeltaBackTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: source_on_t, source_on_o, f_owner_in, f_owner_out
      type(ESMF_Geom), intent(in) :: geom_t, geom_o
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Field), optional, intent(inout) :: weight_owner
      integer, optional, intent(out) :: rc

      integer :: status

      call this%update_regridder_for(geom_t, geom_o, typekind, _RC)
      call this%regrdr%regrid(source_on_t, source_on_o, _RC)
      call add_delta(source_on_o, f_owner_in, f_owner_out, typekind, weight=weight_owner, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_and_add

   ! source = weight * (value - snapshot), or value - snapshot without a
   ! weight. Shapes were checked by the caller.
   subroutine form_source(f_value, f_snapshot, f_source, typekind, weight, rc)
      type(ESMF_Field), intent(inout) :: f_value, f_snapshot, f_source
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Field), optional, intent(inout) :: weight
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: c4(:,:,:), c04(:,:,:), s4(:,:,:), m4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: c8(:,:,:), c08(:,:,:), s8(:,:,:), m8(:,:,:)

      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr_condensed_array(f_value, c4, _RC)
         call assign_fptr_condensed_array(f_snapshot, c04, _RC)
         call assign_fptr_condensed_array(f_source, s4, _RC)
         if (present(weight)) then
            call assign_fptr_condensed_array(weight, m4, _RC)
            s4 = m4 * (c4 - c04)
         else
            s4 = c4 - c04
         end if
      else
         call assign_fptr_condensed_array(f_value, c8, _RC)
         call assign_fptr_condensed_array(f_snapshot, c08, _RC)
         call assign_fptr_condensed_array(f_source, s8, _RC)
         if (present(weight)) then
            call assign_fptr_condensed_array(weight, m8, _RC)
            s8 = m8 * (c8 - c08)
         else
            s8 = c8 - c08
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine form_source

   ! owner_out = owner_in + source / weight, or owner_in + source without a
   ! weight. owner_in and owner_out may be the same Field. The owner weight
   ! is a mass and assumed nonzero; it is not guarded.
   subroutine add_delta(f_source, f_owner_in, f_owner_out, typekind, weight, rc)
      type(ESMF_Field), intent(inout) :: f_source, f_owner_in, f_owner_out
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Field), optional, intent(inout) :: weight
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: s4(:,:,:), x4_in(:,:,:), x4_out(:,:,:), m4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: s8(:,:,:), x8_in(:,:,:), x8_out(:,:,:), m8(:,:,:)

      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr_condensed_array(f_source, s4, _RC)
         call assign_fptr_condensed_array(f_owner_in, x4_in, _RC)
         call assign_fptr_condensed_array(f_owner_out, x4_out, _RC)
         if (present(weight)) then
            call assign_fptr_condensed_array(weight, m4, _RC)
            x4_out = x4_in + s4 / m4
         else
            x4_out = x4_in + s4
         end if
      else
         call assign_fptr_condensed_array(f_source, s8, _RC)
         call assign_fptr_condensed_array(f_owner_in, x8_in, _RC)
         call assign_fptr_condensed_array(f_owner_out, x8_out, _RC)
         if (present(weight)) then
            call assign_fptr_condensed_array(weight, m8, _RC)
            x8_out = x8_in + s8 / m8
         else
            x8_out = x8_in + s8
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine add_delta

   ! Condensed (horizontal, vertical, ungridded) shape of an R4 or R8
   ! field, read with the field's own typekind.
   function condensed_shape(field, rc) result(fshape)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: fshape(3)

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R4), pointer :: x4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x8(:,:,:)

      fshape = 0
      call ESMF_FieldGet(field, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr_condensed_array(field, x4, _RC)
         fshape = shape(x4)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call assign_fptr_condensed_array(field, x8, _RC)
         fshape = shape(x8)
      else
         _FAIL('WeightedDeltaBackTransform: unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end function condensed_shape

   ! Looks up the regridder on first use and again when the geom pair or
   ! typekind changes. Geoms are compared by MAPL geom id, as
   ! RegridderManager does.
   subroutine update_regridder_for(this, geom_t, geom_o, typekind, rc)
      class(WeightedDeltaBackTransform), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom_t, geom_o
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      class(Regridder), pointer :: new_regrdr
      logical :: changed

      changed = .not. associated(this%regrdr)
      if (.not. changed) then
         changed = .not. mapl_SameGeom(geom_t, this%src_geom) &
              .or. .not. mapl_SameGeom(geom_o, this%dst_geom) &
              .or. (typekind /= this%typekind)
      end if
      _RETURN_UNLESS(changed)

      ! Cache nothing until the lookup succeeds, so a failed lookup cannot
      ! leave the old regridder paired with the new geoms.
      regridder_manager => get_regridder_manager()
      spec = RegridderSpec(this%regridder_param, geom_t, geom_o, &
           typekind_in=typekind, typekind_out=typekind)
      new_regrdr => regridder_manager%get_regridder(spec, _RC)

      this%regrdr => new_regrdr
      this%src_geom = geom_t
      this%dst_geom = geom_o
      this%typekind = typekind

      _RETURN(_SUCCESS)
   end subroutine update_regridder_for

   subroutine get_port_field(items, port_name, field, rc)
      type(PortNameGraphStateItemMap), intent(in) :: items
      character(*), intent(in) :: port_name
      type(ESMF_Field), intent(out) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: item
      type(ESMF_StateItem_Flag) :: item_type
      character(:), allocatable :: msg

      item => items%at(port_name)
      msg = 'WeightedDeltaBackTransform: port not bound: ' // port_name
      _ASSERT(associated(item), msg)
      item_type = item%itemType(_RC)
      msg = 'WeightedDeltaBackTransform: port is not a field: ' // port_name
      _ASSERT(item_type == ESMF_STATEITEM_FIELD, msg)
      field = item%get_field(_RC)

      _RETURN(_SUCCESS)
   end subroutine get_port_field

   ! Checks that a port field is on `geom` (by MAPL geom id), has
   ! `typekind`, and has condensed shape `expected_shape`. Without an id,
   ! geoms cannot be compared, and each regridder lookup would add another
   ! regridder to RegridderManager.
   subroutine check_port(field, port_name, geom, typekind, expected_shape, rc)
      type(ESMF_Field), intent(inout) :: field
      character(*), intent(in) :: port_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      integer, intent(in) :: expected_shape(3)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: field_geom
      type(ESMF_TypeKind_Flag) :: field_typekind
      type(GeomId) :: id
      logical :: has_id
      integer :: field_shape(3)
      character(:), allocatable :: msg

      call ESMF_FieldGet(field, geom=field_geom, typekind=field_typekind, _RC)
      id = mapl_GeomGetId(field_geom, isPresent=has_id, _RC)
      msg = 'WeightedDeltaBackTransform: geom has no MAPL geom id on port: ' // port_name
      _ASSERT(has_id, msg)
      msg = 'WeightedDeltaBackTransform: port is on the wrong geom: ' // port_name
      _ASSERT(mapl_SameGeom(field_geom, geom), msg)
      msg = 'WeightedDeltaBackTransform: typekind differs from value on port: ' // port_name
      _ASSERT(field_typekind == typekind, msg)
      field_shape = condensed_shape(field, _RC)
      msg = 'WeightedDeltaBackTransform: shape differs on port: ' // port_name
      _ASSERT(all(field_shape == expected_shape), msg)

      _RETURN(_SUCCESS)
   end subroutine check_port

end module mapl_WeightedDeltaBackTransform_mod
