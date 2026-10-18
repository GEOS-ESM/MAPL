#include "MAPL.h"

!------------------------------------------------------------------------------
! UnitsConverterTransform: the one real Transform (mapl_Transform_mod)
! built by a Characteristic%build_transform in the extension-reuse
! change (UnitsCharacteristic.F90; design.md Decisions - "Extension-chain
! execution ships with one real characteristic provider"). Deliberately
! independent of legacy's ExtensionTransform interface (initialize/
! update(this, importState, exportState, clock, rc),
! superstructure/generic/transforms/ExtensionTransform.F90):
! TransformGraphNode%execute() calls Transform%compute(this, inputs,
! outputs, rc), passing this transform's own already-resolved, bound
! input/output GraphStateItem values (one per declared port name,
! openspec/changes/decouple-transform-from-graph) - no stored
! ComponentGraph/NodeId of its own.
!
! Named UnitsConverterTransform, not ConvertUnitsTransform, because
! `mapl_ConvertUnitsTransform_mod`/`ConvertUnitsTransform`
! (superstructure/generic/transforms/ConvertUnitsTransform.F90) is
! already taken by the legacy module this type is deliberately
! independent of - module names share one global namespace, so the two
! cannot coexist under the same name even though they live in different
! modules. Rename to ConvertUnitsTransform (matching RegridTransform's
! naming convention) once the legacy module is removed - tracked as a
! task in the 3c2 follow-up (docs/graph/spec/20-implementation-roadmap.md).
!
! Reuses two general-purpose infrastructure pieces, not legacy
! StateRegistry/ExtensionFamily machinery: mapl_FieldPointerUtilities_mod
! (assign_fptr - the same low-level ESMF field-pointer helper used
! throughout infrastructure/field) and udunits2f's Converter (the same
! real unit-conversion library legacy's own ConvertUnitsTransform uses -
! read as a faithful behavioral reference for the r4/r8 typekind
! dispatch, never called into).
!------------------------------------------------------------------------------
module mapl_UnitsConverterTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_FieldPointerUtilities_mod, only: assign_fptr
   use udunits2f, only: UDUNITS_Converter => Converter
   use udunits2f, only: UDUNITS_GetConverter => get_converter
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_TypeKind_Flag
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: UnitsConverterTransform

   type, extends(Transform) :: UnitsConverterTransform
      private
      character(:), allocatable :: src_units
      character(:), allocatable :: dst_units
      type(UDUNITS_Converter) :: converter
      logical :: converter_ready = .false.
   contains
      procedure :: compute => units_compute
   end type UnitsConverterTransform

   interface UnitsConverterTransform
      module procedure new_UnitsConverterTransform
   end interface UnitsConverterTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'

contains

   function new_UnitsConverterTransform(src_units, dst_units) result(transform)
      character(*), intent(in) :: src_units
      character(*), intent(in) :: dst_units
      type(UnitsConverterTransform) :: transform

      integer :: status

      transform%src_units = src_units
      transform%dst_units = dst_units

      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_UnitsConverterTransform

   subroutine units_compute(this, inputs, outputs, rc)
      class(UnitsConverterTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(GraphStateItem), pointer :: input_item, output_item
      type(ESMF_Field) :: input_field, output_field
      type(ESMF_TypeKind_Flag) :: typekind

      if (.not. this%converter_ready) then
         call UDUNITS_GetConverter(this%converter, from=this%src_units, to=this%dst_units, _RC)
         this%converter_ready = .true.
      end if

      input_item => inputs%at(INPUT_PORT_NAME)
      _ASSERT(associated(input_item), 'UnitsConverterTransform: input port not found in inputs')
      output_item => outputs%at(OUTPUT_PORT_NAME)
      _ASSERT(associated(output_item), 'UnitsConverterTransform: output port not found in outputs')

      input_field = input_item%get_field(_RC)
      output_field = output_item%get_field(_RC)

      call ESMF_FieldGet(input_field, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4) then
         call convert_r4(input_field, output_field, this%converter, _RC)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call convert_r8(input_field, output_field, this%converter, _RC)
      else
         _FAIL('UnitsConverterTransform: unsupported typekind')
      end if

      _RETURN(_SUCCESS)
   end subroutine units_compute

   subroutine convert_r4(input_field, output_field, converter, rc)
      type(ESMF_Field), intent(inout) :: input_field, output_field
      type(UDUNITS_Converter), intent(in) :: converter
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x_in(:), x_out(:)

      call assign_fptr(input_field, x_in, _RC)
      call assign_fptr(output_field, x_out, _RC)
      x_out = converter%convert(x_in)

      _RETURN(_SUCCESS)
   end subroutine convert_r4

   subroutine convert_r8(input_field, output_field, converter, rc)
      type(ESMF_Field), intent(inout) :: input_field, output_field
      type(UDUNITS_Converter), intent(in) :: converter
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: x_in(:), x_out(:)

      call assign_fptr(input_field, x_in, _RC)
      call assign_fptr(output_field, x_out, _RC)
      x_out = converter%convert(x_in)

      _RETURN(_SUCCESS)
   end subroutine convert_r8

end module mapl_UnitsConverterTransform_mod
