#include "MAPL.h"

!------------------------------------------------------------------------------
! SnapshotTransform: a value copy of a Field, taken each time it computes.
! The callback layer places one on the get return path of a callback
! argument when a horizontal regrid lies between the owner and the Invoker,
! so that a later put can form the delta c - c0 against the value c0 the
! Invoker was given. The delta itself is not formed here.
!
! One input port 'source_field' and one output port 'destination_field'; the
! output is a distinct Field of the same shape and typekind, so a later
! change to the input does not reach it. R4 and R8; any rank that
! assign_fptr_condensed_array supports. Bundles are not supported.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_SnapshotTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_TypeKind_Flag
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use ESMF, only: operator(==), operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: SnapshotTransform

   type, extends(Transform) :: SnapshotTransform
   contains
      procedure :: compute => snapshot_compute
   end type SnapshotTransform

   interface SnapshotTransform
      module procedure new_SnapshotTransform
   end interface SnapshotTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_SnapshotTransform() result(transform)
      type(SnapshotTransform) :: transform

      integer :: status

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_SnapshotTransform

   subroutine snapshot_compute(this, inputs, outputs, rc)
      class(SnapshotTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

      call get_port_field(inputs, INPUT_PORT_NAME, f_in, _RC)
      call get_port_field(outputs, OUTPUT_PORT_NAME, f_out, _RC)
      call copy_field(f_in, f_out, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine snapshot_compute

   subroutine get_port_field(items, port_name, field, rc)
      type(PortNameGraphStateItemMap), intent(in) :: items
      character(*), intent(in) :: port_name
      type(ESMF_Field), intent(out) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: item
      type(ESMF_StateItem_Flag) :: item_type

      item => items%at(port_name)
      _ASSERT(associated(item), 'SnapshotTransform: port not bound: ' // port_name)
      item_type = item%itemType(_RC)
      _ASSERT(item_type == ESMF_STATEITEM_FIELD, 'SnapshotTransform: port is not a field: ' // port_name)
      field = item%get_field(_RC)

      _RETURN(_SUCCESS)
   end subroutine get_port_field

   subroutine copy_field(f_in, f_out, rc)
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_in, tk_out
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:,:,:), x4_out(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x8_in(:,:,:), x8_out(:,:,:)

      call ESMF_FieldGet(f_in, typekind=tk_in, _RC)
      call ESMF_FieldGet(f_out, typekind=tk_out, _RC)
      _ASSERT(tk_in == tk_out, 'SnapshotTransform: typekinds differ')

      if (tk_in == ESMF_TYPEKIND_R4) then
         call assign_fptr_condensed_array(f_in, x4_in, _RC)
         call assign_fptr_condensed_array(f_out, x4_out, _RC)
         _ASSERT(all(shape(x4_in) == shape(x4_out)), 'SnapshotTransform: shapes differ')
         x4_out = x4_in
      else if (tk_in == ESMF_TYPEKIND_R8) then
         call assign_fptr_condensed_array(f_in, x8_in, _RC)
         call assign_fptr_condensed_array(f_out, x8_out, _RC)
         _ASSERT(all(shape(x8_in) == shape(x8_out)), 'SnapshotTransform: shapes differ')
         x8_out = x8_in
      else
         _FAIL('SnapshotTransform: unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end subroutine copy_field

end module mapl_SnapshotTransform_mod
