#include "MAPL.h"

!------------------------------------------------------------------------------
! ConservativeColumnFillTransform: removes negative values from an owner
! field that is declared non_negative with column fill, after a delta has
! been added to it, without changing any column's mass total.
!
! Input ports:
!   owner        X,  on O: the owner field after the delta
!   weight_owner mO, on O: the owner's own weight
! Output ports:
!   owner        X filled, on O
!   fill         X filled - X, on O: the fill diagnostic
!
! A column is one horizontal point and one ungridded index of the condensed
! (horizontal, vertical, ungridded) view. In a column with a negative value,
!   X filled = max(X, 0) * (sum_k mO X) / (sum_k mO max(X, 0))
! so the column total sum_k mO X is unchanged. Other columns are copied, and
! their fill is zero. A column whose total is within round-off of zero (a
! typekind-dependent fraction of sum_k mO |X|) is filled to zero. A column
! whose total is more negative, or that holds a NaN or infinite value or
! weight, cannot be filled and is an error naming the column; the outputs
! are then left unchanged.
!
! All port fields share one typekind (R4 or R8), one geom and one shape.
! The weight is a mass and assumed positive; it is not guarded.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_ConservativeColumnFillTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_PortSpecMap_mod, only: PortSpecMap
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use mapl_geom_api, only: mapl_SameGeom
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_Geom
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_MAXSTR
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use ESMF, only: ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet
   use ESMF, only: operator(==), operator(/=)
   use mapl_ErrorHandling_mod
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   implicit none(type, external)
   private

   public :: ConservativeColumnFillTransform

   type, extends(Transform) :: ConservativeColumnFillTransform
   contains
      procedure :: compute => fill_compute
      procedure :: is_input_port => fill_is_input_port
      procedure :: get_input_port => fill_get_input_port
      procedure :: get_input_ports => fill_get_input_ports
      procedure :: is_output_port => fill_is_output_port
      procedure :: get_output_port => fill_get_output_port
      procedure :: get_output_ports => fill_get_output_ports
   end type ConservativeColumnFillTransform

   interface ConservativeColumnFillTransform
      module procedure new_ConservativeColumnFillTransform
   end interface ConservativeColumnFillTransform

   character(*), parameter :: OWNER_PORT = 'owner'
   character(*), parameter :: WEIGHT_OWNER_PORT = 'weight_owner'
   character(*), parameter :: FILL_PORT = 'fill'
   integer, parameter :: MAX_PORT_NAME_LEN = len(WEIGHT_OWNER_PORT)
   ! A column total below zero by at most this fraction of the column scale
   ! sum_k mO |X| is round-off and is treated as zero. R4 data carries R4
   ! round-off from the regrid, which summing in R8 does not remove.
   real(kind=ESMF_KIND_R8), parameter :: ZERO_TOTAL_TOLERANCE_R8 = 1.0e-12_ESMF_KIND_R8
   real(kind=ESMF_KIND_R8), parameter :: ZERO_TOTAL_TOLERANCE_R4 = &
        4 * real(epsilon(1.0_ESMF_KIND_R4), ESMF_KIND_R8)

contains

   ! Ports are answered from fixed name lists, not declared in the base port
   ! maps: gfortran's polymorphic copy of a TransformGraphNode leaves a
   ! multi-entry gFTL map with dangling internal pointers.
   function new_ConservativeColumnFillTransform() result(transform)
      type(ConservativeColumnFillTransform) :: transform
   end function new_ConservativeColumnFillTransform

   function input_port_names() result(names)
      character(len=MAX_PORT_NAME_LEN), allocatable :: names(:)
      names = [character(len=MAX_PORT_NAME_LEN) :: OWNER_PORT, WEIGHT_OWNER_PORT]
   end function input_port_names

   function output_port_names() result(names)
      character(len=MAX_PORT_NAME_LEN), allocatable :: names(:)
      names = [character(len=MAX_PORT_NAME_LEN) :: OWNER_PORT, FILL_PORT]
   end function output_port_names

   logical function fill_is_input_port(this, name) result(is_port)
      class(ConservativeColumnFillTransform), intent(in) :: this
      character(*), intent(in) :: name

      is_port = any(input_port_names() == name)
      _UNUSED_DUMMY(this)
   end function fill_is_input_port

   logical function fill_is_output_port(this, name) result(is_port)
      class(ConservativeColumnFillTransform), intent(in) :: this
      character(*), intent(in) :: name

      is_port = any(output_port_names() == name)
      _UNUSED_DUMMY(this)
   end function fill_is_output_port

   function fill_get_input_port(this, name, rc) result(spec)
      class(ConservativeColumnFillTransform), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      spec = PortSpec(name)
      _ASSERT(this%is_input_port(name), 'ConservativeColumnFillTransform: input port not declared')
      _RETURN(_SUCCESS)
   end function fill_get_input_port

   function fill_get_output_port(this, name, rc) result(spec)
      class(ConservativeColumnFillTransform), target, intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PortSpec) :: spec

      spec = PortSpec(name)
      _ASSERT(this%is_output_port(name), 'ConservativeColumnFillTransform: output port not declared')
      _RETURN(_SUCCESS)
   end function fill_get_output_port

   function fill_get_input_ports(this) result(ports)
      class(ConservativeColumnFillTransform), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = names_to_ports(input_port_names())
      _UNUSED_DUMMY(this)
   end function fill_get_input_ports

   function fill_get_output_ports(this) result(ports)
      class(ConservativeColumnFillTransform), intent(in) :: this
      type(PortSpecMap) :: ports

      ports = names_to_ports(output_port_names())
      _UNUSED_DUMMY(this)
   end function fill_get_output_ports

   function names_to_ports(names) result(ports)
      character(*), intent(in) :: names(:)
      type(PortSpecMap) :: ports

      integer :: i

      do i = 1, size(names)
         call ports%insert(trim(names(i)), PortSpec(trim(names(i))))
      end do
   end function names_to_ports

   subroutine fill_compute(this, inputs, outputs, rc)
      class(ConservativeColumnFillTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_owner_in, f_weight, f_owner_out, f_fill
      type(ESMF_Geom) :: geom
      type(ESMF_TypeKind_Flag) :: typekind
      integer :: fshape(3)
      character(len=ESMF_MAXSTR) :: field_name
      logical :: supported

      call get_port_field(inputs, OWNER_PORT, f_owner_in, _RC)
      call get_port_field(inputs, WEIGHT_OWNER_PORT, f_weight, _RC)
      call get_port_field(outputs, OWNER_PORT, f_owner_out, _RC)
      call get_port_field(outputs, FILL_PORT, f_fill, _RC)

      call ESMF_FieldGet(f_owner_in, geom=geom, typekind=typekind, name=field_name, _RC)
      supported = typekind == ESMF_TYPEKIND_R4 .or. typekind == ESMF_TYPEKIND_R8
      _ASSERT(supported, 'ConservativeColumnFillTransform: unsupported typekind')
      fshape = condensed_shape(f_owner_in, _RC)
      call check_port(f_weight, WEIGHT_OWNER_PORT, geom, typekind, fshape, _RC)
      call check_port(f_owner_out, OWNER_PORT, geom, typekind, fshape, _RC)
      call check_port(f_fill, FILL_PORT, geom, typekind, fshape, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call fill_r4(f_owner_in, f_weight, f_owner_out, f_fill, trim(field_name), _RC)
      else
         call fill_r8(f_owner_in, f_weight, f_owner_out, f_fill, trim(field_name), _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine fill_compute

   ! Column sums are accumulated in R8 so that an R4 field keeps its column
   ! total to R4 rounding.
   subroutine fill_r4(f_owner_in, f_weight, f_owner_out, f_fill, field_name, rc)
      type(ESMF_Field), intent(inout) :: f_owner_in, f_weight, f_owner_out, f_fill
      character(*), intent(in) :: field_name
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x_in(:,:,:), m(:,:,:), x_out(:,:,:), d(:,:,:)
      real(kind=ESMF_KIND_R8), allocatable :: ratio(:,:)
      logical, allocatable :: needs_fill(:,:)
      real(kind=ESMF_KIND_R4), allocatable :: filled(:)
      integer :: h, u

      call assign_fptr_condensed_array(f_owner_in, x_in, _RC)
      call assign_fptr_condensed_array(f_weight, m, _RC)
      call assign_fptr_condensed_array(f_owner_out, x_out, _RC)
      call assign_fptr_condensed_array(f_fill, d, _RC)

      call column_ratios(real(x_in, ESMF_KIND_R8), real(m, ESMF_KIND_R8), field_name, &
           ZERO_TOTAL_TOLERANCE_R4, needs_fill, ratio, _RC)

      ! A column is filled into a temporary, as owner in and out may be the
      ! same Field.
      do u = 1, size(x_in, 3)
         do h = 1, size(x_in, 1)
            if (needs_fill(h, u)) then
               filled = real(max(x_in(h, :, u), 0.0_ESMF_KIND_R4) * ratio(h, u), ESMF_KIND_R4)
               d(h, :, u) = filled - x_in(h, :, u)
               x_out(h, :, u) = filled
            else
               d(h, :, u) = 0.0_ESMF_KIND_R4
               x_out(h, :, u) = x_in(h, :, u)
            end if
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine fill_r4

   subroutine fill_r8(f_owner_in, f_weight, f_owner_out, f_fill, field_name, rc)
      type(ESMF_Field), intent(inout) :: f_owner_in, f_weight, f_owner_out, f_fill
      character(*), intent(in) :: field_name
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: x_in(:,:,:), m(:,:,:), x_out(:,:,:), d(:,:,:)
      real(kind=ESMF_KIND_R8), allocatable :: ratio(:,:)
      logical, allocatable :: needs_fill(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: filled(:)
      integer :: h, u

      call assign_fptr_condensed_array(f_owner_in, x_in, _RC)
      call assign_fptr_condensed_array(f_weight, m, _RC)
      call assign_fptr_condensed_array(f_owner_out, x_out, _RC)
      call assign_fptr_condensed_array(f_fill, d, _RC)

      call column_ratios(x_in, m, field_name, ZERO_TOTAL_TOLERANCE_R8, needs_fill, ratio, _RC)

      ! A column is filled into a temporary, as owner in and out may be the
      ! same Field.
      do u = 1, size(x_in, 3)
         do h = 1, size(x_in, 1)
            if (needs_fill(h, u)) then
               filled = max(x_in(h, :, u), 0.0_ESMF_KIND_R8) * ratio(h, u)
               d(h, :, u) = filled - x_in(h, :, u)
               x_out(h, :, u) = filled
            else
               d(h, :, u) = 0.0_ESMF_KIND_R8
               x_out(h, :, u) = x_in(h, :, u)
            end if
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine fill_r8

   ! Finds the columns with a negative value and their fill ratios. Every
   ! column is checked before any output is written, so an unfillable
   ! column leaves the outputs unchanged.
   subroutine column_ratios(x, m, field_name, tolerance, needs_fill, ratio, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: x(:,:,:), m(:,:,:)
      character(*), intent(in) :: field_name
      real(kind=ESMF_KIND_R8), intent(in) :: tolerance
      logical, allocatable, intent(out) :: needs_fill(:,:)
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: ratio(:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: h, u
      real(kind=ESMF_KIND_R8) :: total, positive_total, scale
      character(:), allocatable :: label

      allocate(needs_fill(size(x, 1), size(x, 3)), source=.false.)
      allocate(ratio(size(x, 1), size(x, 3)), source=1.0_ESMF_KIND_R8)

      do u = 1, size(x, 3)
         do h = 1, size(x, 1)
            ! Checked before any comparison: a NaN would otherwise compare
            ! false everywhere and be passed through or filled to zero, and
            ! an ordered comparison with NaN may trap under -ieee=stop.
            if (.not. (all(ieee_is_finite(x(h, :, u))) .and. all(ieee_is_finite(m(h, :, u))))) then
               label = column_label(field_name, h, u, size(x, 3), _RC)
               _FAIL('ConservativeColumnFillTransform: column holds a NaN or infinite value: ' // label)
            end if
            if (.not. any(x(h, :, u) < 0)) cycle
            total = sum(m(h, :, u) * x(h, :, u))
            scale = sum(m(h, :, u) * abs(x(h, :, u)))
            if (.not. (total >= -tolerance * scale)) then
               label = column_label(field_name, h, u, size(x, 3), _RC)
               _FAIL('ConservativeColumnFillTransform: column total is negative and cannot be filled: ' // label)
            end if
            needs_fill(h, u) = .true.
            ! A total within round-off of zero is a drained column: fill it
            ! to zero rather than fail.
            ratio(h, u) = 0.0_ESMF_KIND_R8
            if (total > 0) then
               positive_total = sum(m(h, :, u) * max(x(h, :, u), 0.0_ESMF_KIND_R8))
               ratio(h, u) = total / positive_total
            end if
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine column_ratios

   ! Names a column by the field name, the PET, its PET-local horizontal
   ! index in the condensed view, and its ungridded index when there is one.
   function column_label(field_name, h, u, num_ungridded, rc) result(label)
      character(*), intent(in) :: field_name
      integer, intent(in) :: h, u, num_ungridded
      integer, optional, intent(out) :: rc
      character(:), allocatable :: label

      integer :: status, pet
      type(ESMF_VM) :: vm
      character(32) :: buf

      label = ''
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=pet, _RC)
      write(buf, '(i0)') pet
      label = "field '" // field_name // "' on PET " // trim(buf)
      write(buf, '(i0)') h
      label = label // ' local column ' // trim(buf)
      if (num_ungridded > 1) then
         write(buf, '(i0)') u
         label = label // ' ungridded index ' // trim(buf)
      end if

      _RETURN(_SUCCESS)
   end function column_label

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
         _FAIL('ConservativeColumnFillTransform: unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end function condensed_shape

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
      msg = 'ConservativeColumnFillTransform: port not bound: ' // port_name
      _ASSERT(associated(item), msg)
      item_type = item%itemType(_RC)
      msg = 'ConservativeColumnFillTransform: port is not a field: ' // port_name
      _ASSERT(item_type == ESMF_STATEITEM_FIELD, msg)
      field = item%get_field(_RC)

      _RETURN(_SUCCESS)
   end subroutine get_port_field

   ! The weight must be on the owner geom: a weight on another geom does
   ! not give the owner column totals.
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
      integer :: field_shape(3)
      character(:), allocatable :: msg

      call ESMF_FieldGet(field, geom=field_geom, typekind=field_typekind, _RC)
      msg = 'ConservativeColumnFillTransform: port is not on the owner geom: ' // port_name
      _ASSERT(mapl_SameGeom(field_geom, geom), msg)
      msg = 'ConservativeColumnFillTransform: typekind differs from owner on port: ' // port_name
      _ASSERT(field_typekind == typekind, msg)
      field_shape = condensed_shape(field, _RC)
      msg = 'ConservativeColumnFillTransform: shape differs from owner on port: ' // port_name
      _ASSERT(all(field_shape == expected_shape), msg)

      _RETURN(_SUCCESS)
   end subroutine check_port

end module mapl_ConservativeColumnFillTransform_mod
