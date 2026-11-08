#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalFlipTransform: copies a field into another of the same shape with
! the vertical index reversed. It is the step for an orientation-only
! mismatch (same vertical grid, opposite UP/DOWN storage order). Field only;
! R4 and R8.
!
! The flip helpers are public so that VerticalColumnRegridTransform can use
! them for the same-grid case.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_VerticalFlipTransform_mod
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

   public :: VerticalFlipTransform
   public :: copy_vertical_column
   public :: get_vertical_port_field

   type, extends(Transform) :: VerticalFlipTransform
   contains
      procedure :: compute => flip_compute
   end type VerticalFlipTransform

   interface VerticalFlipTransform
      module procedure new_VerticalFlipTransform
   end interface VerticalFlipTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_VerticalFlipTransform() result(transform)
      type(VerticalFlipTransform) :: transform

      integer :: status

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_VerticalFlipTransform

   subroutine flip_compute(this, inputs, outputs, rc)
      class(VerticalFlipTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

      call get_vertical_port_field(inputs, INPUT_PORT_NAME, &
           'VerticalFlipTransform', f_in, _RC)
      call get_vertical_port_field(outputs, OUTPUT_PORT_NAME, &
           'VerticalFlipTransform', f_out, _RC)
      call copy_vertical_column(f_in, f_out, flip=.true., _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine flip_compute

   ! Copies f_in to f_out (same shape and typekind), reversing the vertical
   ! index when flip is true.
   subroutine copy_vertical_column(f_in, f_out, flip, rc)
      type(ESMF_Field), intent(inout) :: f_in, f_out
      logical, intent(in) :: flip
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_in, tk_out
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:,:,:), x4_out(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x8_in(:,:,:), x8_out(:,:,:)
      integer :: n

      call ESMF_FieldGet(f_in, typekind=tk_in, _RC)
      call ESMF_FieldGet(f_out, typekind=tk_out, _RC)
      _ASSERT(tk_in == tk_out, 'typekinds differ')

      if (tk_in == ESMF_TYPEKIND_R4) then
         call assign_fptr_condensed_array(f_in, x4_in, _RC)
         call assign_fptr_condensed_array(f_out, x4_out, _RC)
         _ASSERT(all(shape(x4_in) == shape(x4_out)), 'shapes differ')
         n = size(x4_in, 2)
         if (flip) then
            x4_out = x4_in(:, n:1:-1, :)
         else
            x4_out = x4_in
         end if
      else if (tk_in == ESMF_TYPEKIND_R8) then
         call assign_fptr_condensed_array(f_in, x8_in, _RC)
         call assign_fptr_condensed_array(f_out, x8_out, _RC)
         _ASSERT(all(shape(x8_in) == shape(x8_out)), 'shapes differ')
         n = size(x8_in, 2)
         if (flip) then
            x8_out = x8_in(:, n:1:-1, :)
         else
            x8_out = x8_in
         end if
      else
         _FAIL('unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end subroutine copy_vertical_column

   subroutine get_vertical_port_field(items, port_name, who, field, rc)
      type(PortNameGraphStateItemMap), intent(in) :: items
      character(*), intent(in) :: port_name
      character(*), intent(in) :: who
      type(ESMF_Field), intent(out) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: item
      type(ESMF_StateItem_Flag) :: item_type

      item => items%at(port_name)
      _ASSERT(associated(item), who // ': port not bound: ' // port_name)
      item_type = item%itemType(_RC)
      if (item_type /= ESMF_STATEITEM_FIELD) then
         _FAIL(who // ': port is not a field: ' // port_name)
      end if
      field = item%get_field(_RC)

      _RETURN(_SUCCESS)
   end subroutine get_vertical_port_field

end module mapl_VerticalFlipTransform_mod
